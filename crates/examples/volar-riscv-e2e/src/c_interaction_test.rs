//! The M1 C interaction harness (consumer side): drives the **lowered**
//! prover/QSim/verifier role functions through the real per-step protocol
//! in native C, over committed-memory ground truth, with a real IOP fold
//! accumulator, and asserts both the honest run verifies AND a tampered
//! run is rejected.
//!
//! Pipeline (all under `target/c_interaction/`):
//! 1. weave the three roles (`weave_vole_{prover,qsim,verifier}_ir_split`)
//!    and lower them to one C module via `lower_module_monomorphized` +
//!    `CBackend` (the `lir_probe_three_roles_lower_to_c` output);
//! 2. `split_roles.py` splits the module into per-function translation
//!    units + a shared header (387 MB does not cc in one TU; the split
//!    weave's per-function model is exactly what per-TU compilation is
//!    for) and sizes the wrapper-local piece pools to the pieces' var-id
//!    indexing (the Rust driver's pools are TOTAL_VARS-sized for the same
//!    reason);
//! 3. compile every TU (`-O0`, parallelizable), plus the checked-in
//!    harness (`c_interaction_harness.c` — the ideal C-OT, the GF(2^128)
//!    fold accumulator with its relaxed-R1CS final check, and the driver
//!    loop mirroring `generate_split_step_ir`'s call sequence) and
//!    `mul_vope.c` (the GF(2^8) Vope polynomial product the weave left
//!    as a host hook);
//! 4. link and run: honest run exits 0 (all_ok at every region, fold
//!    relaxed-satisfied), a QSim-tampered run exits 1.
//!
//! Run: `cargo test -p volar-riscv-e2e --lib c_interaction -- --ignored
//! --nocapture`. Budget ~50 min wall (one-time; the .o files are cached
//! across runs in the work dir).

use std::path::PathBuf;
use std::process::Command;

const HARNESS_DIR: &str = "tests";
const WORK_SUBDIR: &str = "c_interaction";

fn work_dir() -> PathBuf {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("manifest dir");
    let target = PathBuf::from(manifest)
        .join("../../target")
        .join(WORK_SUBDIR);
    std::fs::create_dir_all(&target).expect("create work dir");
    target.canonicalize().expect("canonicalize work dir")
}

#[test]
#[ignore]
fn c_interaction_lowers_runs_and_verifies() {
    // ---------- 1. weave + lower the three roles to C ----------
    let c_src = crate::mem_probe::tests::three_roles_lowered_c_source();
    assert!(!c_src.is_empty(), "lowered role C is empty");
    let work = work_dir();
    let roles_c = work.join("roles.c");
    std::fs::write(&roles_c, &c_src).expect("write roles.c");

    // ---------- 2. split into per-function TUs ----------
    let splitter = manifest_dir().join(HARNESS_DIR).join("split_roles.py");
    run(Command::new("python3")
        .arg(&splitter)
        .arg(&roles_c)
        .arg(&work));
    let manifest = work.join("manifest.txt");
    assert!(manifest.exists(), "split produced no manifest");

    // ---------- 3a. compile the role TUs (cached across runs) ----------
    let build = manifest_dir().join(HARNESS_DIR).join("build_roles.sh");
    run(Command::new("bash").arg(&build).arg(&work));

    // ---------- 3b. compile the harness + helpers ----------
    for c in ["c_interaction_harness.c", "mul_vope.c"] {
        let out = work.join(format!("{}.o", c.trim_end_matches(".c")));
        if !out.exists() {
            run(Command::new("cc")
                .args(["-O0", "-w", "-std=c99", "-c"])
                .arg(manifest_dir().join(HARNESS_DIR).join(c))
                .arg("-o")
                .arg(&out));
        }
    }

    // ---------- 3c. link ----------
    let bin = work.join("m1_interaction");
    let mut link = Command::new("cc");
    link.arg("-w");
    link.arg(work.join("c_interaction_harness.o"));
    link.arg(work.join("mul_vope.o"));
    for tu in std::fs::read_to_string(&manifest)
        .expect("manifest")
        .lines()
    {
        let obj = PathBuf::from(tu).with_extension("o");
        if obj.exists() {
            link.arg(obj);
        }
    }
    link.arg("-o").arg(&bin);
    run(&mut link);

    // ---------- 4a. honest run must verify ----------
    let out = run_output(&mut Command::new(&bin));
    assert!(
        out.status.success(),
        "honest 3-step interaction failed:\n{}",
        out.stderr
    );
    let stdout = out.stdout;
    assert!(stdout.contains("STEP 0 OK"), "step 0 missing: {stdout}");
    assert!(stdout.contains("STEP 1 OK"), "step 1 missing: {stdout}");
    assert!(stdout.contains("STEP 2 OK"), "step 2 missing: {stdout}");
    assert!(
        stdout.contains("ALL CHECKS PASSED"),
        "final checks missing: {stdout}"
    );
    assert!(
        stdout.contains("fold accumulator relaxed-satisfied"),
        "fold check missing: {stdout}"
    );

    // ---------- 4b. a tampered QSim must be rejected ----------
    let tampered_bin = work.join("m1_interaction_tampered");
    let tampered_c = work.join("c_interaction_harness_tampered.c");
    let harness = std::fs::read_to_string(
        manifest_dir()
            .join(HARNESS_DIR)
            .join("c_interaction_harness.c"),
    )
    .expect("harness source");
    // A lying QSim: corrupt one derived q_and before the verifier consumes
    // it. The verifier's own gate check (k_a·k_b + hat == k_and·Δ) must
    // fail => all_ok false => nonzero exit.
    let tampered = harness.replace(
        "        IopChallenge rands[5];\n        r_ands_make(rands, 5, step, ++*gate_seed);\n        __Tuple_as3x64_as3x32_b_s0 v = vole_verify_ir_mp_block_0(",
        "        IopChallenge rands[5];\n        r_ands_make(rands, 5, step, ++*gate_seed);\n        q._2.data[0].q.data[0] ^= 1; /* lying QSim: corrupt q_and_0 */\n        __Tuple_as3x64_as3x32_b_s0 v = vole_verify_ir_mp_block_0(",
    );
    assert!(tampered != harness, "tamper site not found in harness");
    std::fs::write(&tampered_c, tampered).expect("write tampered harness");
    run(Command::new("cc")
        .args(["-O0", "-w", "-std=c99", "-c"])
        .arg(&tampered_c)
        .arg("-o")
        .arg(work.join("c_interaction_harness_tampered.o")));
    let mut link_t = Command::new("cc");
    link_t.arg("-w");
    link_t.arg(work.join("c_interaction_harness_tampered.o"));
    link_t.arg(work.join("mul_vope.o"));
    for tu in std::fs::read_to_string(&manifest)
        .expect("manifest")
        .lines()
    {
        let obj = PathBuf::from(tu).with_extension("o");
        if obj.exists() {
            link_t.arg(obj);
        }
    }
    link_t.arg("-o").arg(&tampered_bin);
    run(&mut link_t);

    let out_t = run_output(&mut Command::new(&tampered_bin));
    assert!(
        !out_t.status.success(),
        "a tampered q_and must NOT verify:\n{}",
        out_t.stdout
    );
    assert!(
        out_t.stdout.contains("FAIL"),
        "tampered run must report a failure: {}",
        out_t.stdout
    );
}

fn manifest_dir() -> PathBuf {
    PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").expect("manifest dir"))
}

fn run(cmd: &mut Command) {
    let out = run_output(cmd);
    assert!(
        out.status.success(),
        "command failed: {:?}\nstdout: {}\nstderr: {}",
        cmd,
        out.stdout,
        out.stderr
    );
}

struct RunOut {
    status: std::process::ExitStatus,
    stdout: String,
    stderr: String,
}

fn run_output(cmd: &mut Command) -> RunOut {
    let out = cmd.output().expect("spawn");
    RunOut {
        status: out.status,
        stdout: String::from_utf8_lossy(&out.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&out.stderr).into_owned(),
    }
}

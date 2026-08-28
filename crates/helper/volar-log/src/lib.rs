use std::io::{self, Write};
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Clone, Copy, Debug)]
pub struct LlmtrimLogger {
    pub json_mode: bool,
    pub batch_mode: bool,
    pub autominify: bool,
}

impl LlmtrimLogger {
    pub fn from_env() -> Self {
        Self {
            json_mode: std::env::var_os("PORTAL_LOG_JSON").as_deref()
                == Some(std::ffi::OsStr::new("1")),
            batch_mode: std::env::var_os("PORTAL_LOG_BATCH").as_deref()
                == Some(std::ffi::OsStr::new("1")),
            autominify: std::env::var_os("PORTAL_AUTOMINIFY").as_deref()
                == Some(std::ffi::OsStr::new("1")),
        }
    }

    pub fn disabled() -> Self {
        Self {
            json_mode: false,
            batch_mode: false,
            autominify: false,
        }
    }

    fn now_ms() -> u64 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_millis() as u64)
            .unwrap_or(0)
    }

    pub fn log_event(&self, level: &str, phase: &str, msg: &str, fields: &[(&str, &str)]) {
        if !self.json_mode {
            return;
        }
        let t = Self::now_ms();
        let mut line = format!(
            r#"{{"l":{l:?},"phase":{ph:?},"msg":{msg:?},"t":{t}"#,
            l = level,
            ph = phase,
            msg = msg,
            t = t
        );
        for (k, v) in fields {
            line.push_str(&format!(r#",{k:?}:{v:?}"#, k = k, v = v));
        }
        line.push('}');
        let _ = writeln!(io::stderr(), "{line}");
    }

    pub fn begin_batch(&self, unit: &str) -> Batch {
        Batch {
            logger: *self,
            unit: unit.to_owned(),
            events: Vec::new(),
            start_ms: Self::now_ms(),
        }
    }
}

pub struct LogEvent {
    pub level: String,
    pub phase: String,
    pub msg: String,
    pub fields: Vec<(String, String)>,
    pub t: u64,
}

pub struct Batch {
    pub logger: LlmtrimLogger,
    pub unit: String,
    pub events: Vec<LogEvent>,
    pub start_ms: u64,
}

impl Batch {
    pub fn event(&mut self, level: &str, phase: &str, msg: &str, fields: &[(&str, &str)]) {
        if !self.logger.json_mode {
            return;
        }
        self.events.push(LogEvent {
            level: level.to_owned(),
            phase: phase.to_owned(),
            msg: msg.to_owned(),
            fields: fields
                .iter()
                .map(|(k, v)| (k.to_string(), v.to_string()))
                .collect(),
            t: LlmtrimLogger::now_ms(),
        });
    }
}

impl Drop for Batch {
    fn drop(&mut self) {
        if !self.logger.json_mode || self.events.is_empty() {
            return;
        }
        let duration_ms = LlmtrimLogger::now_ms().saturating_sub(self.start_ms);
        let mut events_json = String::from('[');
        for (i, ev) in self.events.iter().enumerate() {
            if i > 0 {
                events_json.push(',');
            }
            events_json.push_str(&format!(
                r#"{{"l":{l:?},"phase":{ph:?},"msg":{msg:?},"t":{t}"#,
                l = ev.level,
                ph = ev.phase,
                msg = ev.msg,
                t = ev.t
            ));
            for (k, v) in &ev.fields {
                events_json.push_str(&format!(r#",{k:?}:{v:?}"#, k = k, v = v));
            }
            events_json.push('}');
        }
        events_json.push(']');
        let line = format!(
            r#"{{"batch":{unit:?},"events":{events},"duration_ms":{d}}}"#,
            unit = self.unit,
            events = events_json,
            d = duration_ms
        );
        let _ = writeln!(io::stderr(), "{line}");
    }
}

#[cfg(feature = "log-subscriber")]
impl log::Log for LlmtrimLogger {
    fn enabled(&self, _: &log::Metadata) -> bool {
        self.json_mode
    }

    fn log(&self, record: &log::Record) {
        if !self.json_mode {
            return;
        }
        let msg = record.args().to_string();
        self.log_event(record.level().as_str(), record.target(), &msg, &[]);
    }

    fn flush(&self) {}
}

#[cfg(feature = "log-subscriber")]
pub fn install_as_global_logger(logger: LlmtrimLogger) {
    let _ = log::set_boxed_logger(Box::new(logger));
    log::set_max_level(log::LevelFilter::Trace);
}

/// Strip C/C++ comments and collapse whitespace. Preserves string literals.
pub fn minify_c(src: &str) -> String {
    let mut out = String::with_capacity(src.len() / 2);
    let mut chars = src.chars().peekable();
    let mut in_line_comment = false;
    let mut in_block_comment = false;
    let mut in_string = false;
    let mut prev_was_space = true;

    while let Some(c) = chars.next() {
        if in_line_comment {
            if c == '\n' {
                in_line_comment = false;
                if !prev_was_space && !out.is_empty() {
                    out.push(' ');
                    prev_was_space = true;
                }
            }
            continue;
        }
        if in_block_comment {
            if c == '*' && chars.peek() == Some(&'/') {
                chars.next();
                in_block_comment = false;
                if !prev_was_space && !out.is_empty() {
                    out.push(' ');
                    prev_was_space = true;
                }
            }
            continue;
        }
        if in_string {
            if c == '\\' {
                out.push(c);
                if let Some(esc) = chars.next() {
                    out.push(esc);
                }
                prev_was_space = false;
                continue;
            }
            if c == '"' {
                in_string = false;
            }
            out.push(c);
            prev_was_space = false;
            continue;
        }
        if c == '"' {
            in_string = true;
            out.push(c);
            prev_was_space = false;
            continue;
        }
        if c == '/' {
            if chars.peek() == Some(&'/') {
                chars.next();
                in_line_comment = true;
                continue;
            }
            if chars.peek() == Some(&'*') {
                chars.next();
                in_block_comment = true;
                continue;
            }
        }
        if c.is_ascii_whitespace() {
            if !prev_was_space && !out.is_empty() {
                out.push(' ');
                prev_was_space = true;
            }
        } else {
            out.push(c);
            prev_was_space = false;
        }
    }
    out.trim_end().to_owned()
}

/// Strip LLVM IR `;` comments and collapse whitespace, preserving label lines.
pub fn minify_llvm_ir(src: &str) -> String {
    src.lines()
        .filter_map(|line| {
            let code = if let Some(idx) = line.find(';') {
                &line[..idx]
            } else {
                line
            };
            let trimmed = code.trim();
            if trimmed.is_empty() {
                None
            } else {
                Some(trimmed)
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Strip `//` comments from volar IR text and collapse whitespace.
pub fn minify_ir_text(src: &str) -> String {
    src.lines()
        .filter_map(|line| {
            let code = if let Some(idx) = line.find("//") {
                &line[..idx]
            } else {
                line
            };
            let trimmed = code.trim();
            if trimmed.is_empty() {
                None
            } else {
                Some(trimmed)
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Strip Rust `//` and `/* */` comments (but not `///` doc comments) and collapse whitespace.
pub fn minify_rust(src: &str) -> String {
    let mut out = String::with_capacity(src.len() / 2);
    let mut chars = src.chars().peekable();
    let mut in_line_comment = false;
    let mut in_block_comment = false;
    let mut in_string = false;
    let mut prev_was_space = true;

    while let Some(c) = chars.next() {
        if in_line_comment {
            if c == '\n' {
                in_line_comment = false;
                if !prev_was_space && !out.is_empty() {
                    out.push(' ');
                    prev_was_space = true;
                }
            }
            continue;
        }
        if in_block_comment {
            if c == '*' && chars.peek() == Some(&'/') {
                chars.next();
                in_block_comment = false;
                if !prev_was_space && !out.is_empty() {
                    out.push(' ');
                    prev_was_space = true;
                }
            }
            continue;
        }
        if in_string {
            if c == '\\' {
                out.push(c);
                if let Some(esc) = chars.next() {
                    out.push(esc);
                }
                prev_was_space = false;
                continue;
            }
            if c == '"' {
                in_string = false;
            }
            out.push(c);
            prev_was_space = false;
            continue;
        }
        if c == '"' {
            in_string = true;
            out.push(c);
            prev_was_space = false;
            continue;
        }
        if c == '/' {
            match chars.peek() {
                Some(&'/') => {
                    chars.next();
                    // preserve doc comments `///`
                    if chars.peek() == Some(&'/') {
                        out.push_str("//");
                        // copy rest of line as-is
                        while let Some(&nc) = chars.peek() {
                            if nc == '\n' {
                                break;
                            }
                            out.push(nc);
                            chars.next();
                        }
                        prev_was_space = false;
                    } else {
                        in_line_comment = true;
                    }
                    continue;
                }
                Some(&'*') => {
                    chars.next();
                    in_block_comment = true;
                    continue;
                }
                _ => {}
            }
        }
        if c.is_ascii_whitespace() {
            if !prev_was_space && !out.is_empty() {
                out.push(' ');
                prev_was_space = true;
            }
        } else {
            out.push(c);
            prev_was_space = false;
        }
    }
    out.trim_end().to_owned()
}

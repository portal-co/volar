#!/usr/bin/env bash
# guarded-run.sh: run a command under a tight-interval memory watchdog that
# kills it (and its whole process group) before a runaway `rustc` can
# swap-thrash the machine into unresponsiveness.
#
# Background: a real-scale compile attempt made the machine hang hard
# enough to need a manual power reset. A prior monitoring loop polled RSS
# every 120s, which is far too coarse -- rustc's RSS can go from a few GB
# to 90GB+ within ~15-20 minutes once it starts blowing up, and by the
# time a 120s sample would have caught it, macOS was already thrashing
# swap badly enough that the OS itself stopped responding (not just the
# offending process -- so even a clean OOM-kill signal from the kernel
# never got a chance to run). This script polls every few seconds instead,
# and kills on ANY of three signals: a single rustc process's own RSS, the
# combined RSS of all rustc processes, or the system's free+inactive
# memory dropping too low -- whichever fires first.
#
# A kill here is meant to be a LOUD, unambiguous failure, not a silent
# abort: every poll is logged, and the specific breach reason (which
# metric, which PID, what value) is logged before the kill happens, so a
# post-mortem never has to guess whether the run failed or was killed, or
# why.
#
# Usage:
#   scripts/guarded-run.sh <log_dir> [rss_limit_gb] [free_floor_gb] [swap_limit_gb] -- <command...>
#
# Defaults (tuned for a 32GB machine, leaving real headroom before the
# whole box becomes unresponsive -- adjust for other machines):
#   rss_limit_gb=20    kill if any single rustc process RSS >= this
#   free_floor_gb=3    kill if free+inactive memory <= this
#   swap_limit_gb=8    kill if swap used >= this (heavy swap = thrashing)
#
# Exit code: 137 (128+SIGKILL) if the guard killed the command; otherwise
# the wrapped command's own real exit code.
#
# Log files, under <log_dir>:
#   command.log   stdout+stderr of the wrapped command
#   guard.log     one line per poll (timestamp, top rustc RSS, free mem,
#                 swap used) plus a loud multi-line block if/when it kills

set -uo pipefail
# Monitor mode: puts each backgrounded job in its OWN process group instead
# of inheriting this script's own PGID. Without this, `kill -9 -- "-$PGID"`
# below kills the guard script's own shell along with the target (confirmed
# by testing -- the guard was committing suicide on every kill).
set -m

usage() {
  echo "usage: guarded-run.sh <log_dir> [rss_limit_gb] [free_floor_gb] [swap_limit_gb] -- <command...>" >&2
  exit 2
}

[ $# -ge 1 ] || usage
LOG_DIR="$1"; shift

RSS_LIMIT_GB=20
FREE_FLOOR_GB=3
SWAP_LIMIT_GB=8

# Optional positional numeric overrides, in order, before the mandatory `--`.
while [ $# -gt 0 ] && [ "$1" != "--" ]; do
  if [ -z "${__rss_set:-}" ]; then RSS_LIMIT_GB="$1"; __rss_set=1;
  elif [ -z "${__free_set:-}" ]; then FREE_FLOOR_GB="$1"; __free_set=1;
  elif [ -z "${__swap_set:-}" ]; then SWAP_LIMIT_GB="$1"; __swap_set=1;
  else usage; fi
  shift
done
[ "${1:-}" = "--" ] || usage
shift
[ $# -ge 1 ] || usage

mkdir -p "$LOG_DIR"
GUARD_LOG="$LOG_DIR/guard.log"
CMD_LOG="$LOG_DIR/command.log"
POLL_INTERVAL=5

log() { echo "[guard] $(date '+%H:%M:%S') $*" | tee -a "$GUARD_LOG"; }

log "starting: $* (rss_limit=${RSS_LIMIT_GB}GB free_floor=${FREE_FLOOR_GB}GB swap_limit=${SWAP_LIMIT_GB}GB poll=${POLL_INTERVAL}s)"

"$@" > "$CMD_LOG" 2>&1 &
CMD_PID=$!
PGID=$(ps -o pgid= -p "$CMD_PID" 2>/dev/null | tr -d ' ')
log "launched pid=$CMD_PID pgid=${PGID:-unknown}"

PAGE_SIZE=$(vm_stat | head -1 | grep -oE '[0-9]+' | head -1)
PAGE_SIZE=${PAGE_SIZE:-16384}

killed=0
reason=""
while kill -0 "$CMD_PID" 2>/dev/null; do
  # All processes literally named `rustc` (exact match) -- their individual
  # and combined RSS. There can be more than one (build scripts, parallel
  # linking); either a single one or the sum going too high is dangerous.
  rustc_pids=$(pgrep -x rustc 2>/dev/null || true)
  top_rustc_pid=""
  top_rustc_rss_gb="0"
  sum_rustc_rss_gb="0"
  if [ -n "$rustc_pids" ]; then
    while read -r pid; do
      [ -n "$pid" ] || continue
      rss_kb=$(ps -o rss= -p "$pid" 2>/dev/null | tr -d ' ')
      [ -n "$rss_kb" ] || continue
      rss_gb=$(awk -v k="$rss_kb" 'BEGIN{printf "%.2f", k/1024/1024}')
      sum_rustc_rss_gb=$(awk -v a="$sum_rustc_rss_gb" -v b="$rss_gb" 'BEGIN{printf "%.2f", a+b}')
      if awk -v a="$rss_gb" -v b="$top_rustc_rss_gb" 'BEGIN{exit !(a>b)}'; then
        top_rustc_rss_gb="$rss_gb"; top_rustc_pid="$pid"
      fi
    done <<< "$rustc_pids"
  fi

  free_pages=$(vm_stat | awk '/Pages free/{gsub("[.]","");print $3}')
  inactive_pages=$(vm_stat | awk '/Pages inactive/{gsub("[.]","");print $3}')
  free_gb=$(awk -v f="${free_pages:-0}" -v i="${inactive_pages:-0}" -v p="$PAGE_SIZE" 'BEGIN{printf "%.2f", (f+i)*p/1024/1024/1024}')

  swap_used_gb=$(sysctl -n vm.swapusage 2>/dev/null | grep -oE 'used = [0-9.]+M' | grep -oE '[0-9.]+' )
  if [ -n "$swap_used_gb" ]; then swap_used_gb=$(awk -v m="$swap_used_gb" 'BEGIN{printf "%.2f", m/1024}'); else swap_used_gb="0"; fi

  log "rustc_pids=$(echo "$rustc_pids" | tr '\n' ',') top_rustc_rss_gb=$top_rustc_rss_gb sum_rustc_rss_gb=$sum_rustc_rss_gb free_gb=$free_gb swap_used_gb=$swap_used_gb"

  reason=""
  if awk -v r="$top_rustc_rss_gb" -v lim="$RSS_LIMIT_GB" 'BEGIN{exit !(r>=lim)}'; then
    reason="single rustc process RSS ${top_rustc_rss_gb}GB >= limit ${RSS_LIMIT_GB}GB (pid=$top_rustc_pid)"
  elif awk -v r="$sum_rustc_rss_gb" -v lim="$RSS_LIMIT_GB" 'BEGIN{exit !(r>=lim*1.5)}'; then
    reason="combined rustc RSS ${sum_rustc_rss_gb}GB >= 1.5x limit ($(awk -v l="$RSS_LIMIT_GB" 'BEGIN{printf "%.1f", l*1.5}')GB)"
  elif awk -v f="$free_gb" -v floor="$FREE_FLOOR_GB" 'BEGIN{exit !(f<=floor)}'; then
    reason="system free+inactive memory ${free_gb}GB <= floor ${FREE_FLOOR_GB}GB"
  elif awk -v s="$swap_used_gb" -v lim="$SWAP_LIMIT_GB" 'BEGIN{exit !(s>=lim)}'; then
    reason="swap used ${swap_used_gb}GB >= limit ${SWAP_LIMIT_GB}GB (thrashing)"
  fi

  if [ -n "$reason" ]; then
    log "*** GUARD KILL *** reason: $reason"
    log "*** GUARD KILL *** killing pgid=${PGID:-unknown}, all rustc pids ($rustc_pids), and cmd pid=$CMD_PID"
    if [ -n "${PGID:-}" ]; then kill -9 -- "-$PGID" 2>>"$GUARD_LOG" || true; fi
    if [ -n "$rustc_pids" ]; then
      while read -r pid; do [ -n "$pid" ] && kill -9 "$pid" 2>>"$GUARD_LOG" || true; done <<< "$rustc_pids"
    fi
    kill -9 "$CMD_PID" 2>>"$GUARD_LOG" || true
    killed=1
    break
  fi
  sleep "$POLL_INTERVAL"
done

wait "$CMD_PID" 2>/dev/null
exit_code=$?

if [ "$killed" = "1" ]; then
  log "command was KILLED by guard -- reason: $reason"
  log "see $CMD_LOG for output up to the kill, and above for the RSS/memory trend leading up to it"
  exit 137
else
  log "command finished on its own, exit_code=$exit_code"
  exit "$exit_code"
fi

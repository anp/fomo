use std::time::{SystemTime, UNIX_EPOCH};

use chrono::{DateTime, Local, TimeZone};

pub fn system_time_to_date_time(t: SystemTime) -> DateTime<Local> {
  let (sec, nsec) = match t.duration_since(UNIX_EPOCH) {
    Ok(dur) => (dur.as_secs() as i64, dur.subsec_nanos()),
    Err(e) => {
      // unlikely but should be handled
      let dur = e.duration();
      let (sec, nsec) = (dur.as_secs() as i64, dur.subsec_nanos());
      if nsec == 0 {
        (-sec, 0)
      } else {
        (-sec - 1, 1_000_000_000 - nsec)
      }
    }
  };
  Local.timestamp(sec, nsec)
}

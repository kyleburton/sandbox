#[macro_use]
extern crate log;
extern crate env_logger;
use log::{LogLevel};

mod myns;

fn main() {
    env_logger::init().unwrap();

    trace!("logging at trace");
    debug!("logging at debug");
    info!("logging at info");
    warn!("logging at warn");
    error!("logging at error");

    log!(LogLevel::Trace, "logging at trace");
    log!(LogLevel::Debug, "logging at debug");
    log!(LogLevel::Info,  "logging at info");
    log!(LogLevel::Warn,  "logging at warn");
    log!(LogLevel::Error, "logging at error");

    if log_enabled!(LogLevel::Error) {
        error!("Error level is enabled");
    }

    myns::callme();
}

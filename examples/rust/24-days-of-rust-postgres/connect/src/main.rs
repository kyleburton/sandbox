extern crate rustc_serialize;
extern crate r2d2;
extern crate r2d2_postgres;
extern crate postgres;
extern crate openssl;
use std::thread;
use r2d2_postgres::{SslMode, PostgresConnectionManager};
use std::fs::File;
use std::io::Read;
use rustc_serialize::json::Json;

use openssl::ssl::SslMethod;
use openssl::ssl::SslContext;

fn get_config() -> Json {
    let mut file = File::open("db-config.json").unwrap();
    let mut str = String::new();
    file.read_to_string(&mut str).unwrap();
    return Json::from_str(&str).unwrap();
}

fn main() {
    let db_config_json = get_config();
    let db_config = db_config_json.as_object().unwrap();
    let config = r2d2::Config::builder()
        .pool_size(8)
        .build();
    let conn_str = format!("postgres://{}:{}@{}:{}/{}",
                          db_config.get("user").unwrap().as_string().unwrap(),
                          db_config.get("pass").unwrap().as_string().unwrap(),
                          db_config.get("host").unwrap().as_string().unwrap(),
                          db_config.get("port").unwrap().as_string().unwrap(),
                          db_config.get("db").unwrap().as_string().unwrap()
                          );
    println!("connecting, conn_str={}", conn_str);

    // let manager = PostgresConnectionManager::new("postgres://postgres@localhost",
    //                                              SslMode::None).unwrap();
    //let manager = PostgresConnectionManager::new(conn_str.as_str(), SslMode::None).unwrap();

    let ctx = SslContext::new(SslMethod::Sslv23).unwrap();
    let boxctx = Box::new(ctx);
    let sslmode = SslMode::Prefer(boxctx);
    let manager = PostgresConnectionManager::new(conn_str.as_str(), sslmode).unwrap();
    println!("connected, manager={:?}", manager);

    let pool = r2d2::Pool::new(config, manager).unwrap();
    println!("connected, pool={:?}", pool);

    let conn = pool.get().unwrap();
    let res = conn.query("select now();", &[]);
    println!("res={:?}", res);

    /*
    for i in 0..10i32 {
        let pool = pool.clone();
        println!("[i={}] cloned pool, spawning thread", i);
        thread::spawn(move || {
            let conn = pool.get().unwrap();
            println!("[thread={}] inserting", i);
            let res = conn.execute("INSERT INTO foo (bar) VALUES ($1)", &[&i]).unwrap();
            println!("res={}", res);
        });
    }
    */
}

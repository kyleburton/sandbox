use std::collections::HashMap;

// extern crate rustc_serialize;
// use rustc_serialize::json;
extern crate serde_json;

// http://localhost.localdomain.com:5080/tix/js/version.js
// http://localhost.localdomain.com:5080/version.js

#[tokio::main]
async fn test1() -> Result<(), Box<dyn std::error::Error>> {
    // let url = "http://localhost.localdomain.com:5080/tix/js/version.js";
    let url = "https://httpbin.org/ip";
    let resp = reqwest::get(url)
        .await?
        .json::<HashMap<String, String>>()
        .await?;
    println!("{:#?}", resp);
    Ok(())
}

#[tokio::main]
async fn test2(url: &'static str) -> Result<std::collections::HashMap<String, String>, Box<dyn std::error::Error>> {
    let resp = reqwest::get(url)
        .await?
        .json::<HashMap<String, String>>()
        .await?;
    println!("{:#?}", resp);
    Ok(resp)
}

#[tokio::main]
async fn test3(url: &'static str) -> Result<std::collections::HashMap<String, String>, Box<dyn std::error::Error>> {
    let res1 = reqwest::get(url).await?;
    println!("test3({:#?}) status={:#?}", url, res1.status());
    let resp = res1.json::<HashMap<String, String>>().await?;
    println!("{:#?}", resp);
    Ok(resp)
}

#[tokio::main]
async fn get_as_text(url: &'static str) -> Result<String, Box<dyn std::error::Error>> {
    let resp = reqwest::Client::builder().build()?.get(url).send().await?;
    return Ok(resp.text().await?)
}

fn parse_json_to_map(data: & std::string::String) -> serde_json::Value {
    return serde_json::from_str::<serde_json::Value>(data).unwrap()
}

fn main () {
    let res1 = test1();
    println!("test1: res={:#?}", res1);
    let res2 = test2("http://localhost.localdomain.com:5080/tix/js/version.js");
    println!("test2: res={:#?}", res2);

    println!("should 404    res={:#?}", test2("http://localhost.localdomain.com:5080/version.js"));
    println!("should 404    res={:#?}", test3("http://localhost.localdomain.com:5080/version.js"));

    let res3 = get_as_text("http://localhost.localdomain.com:5080/tix/js/version.js");
    println!("get_as_text={:#?}", res3);
    let body1 = match res3 {
        Ok(text) => text,
        Err(err) => {
            println!("error: {:#?}", err);
            "error!".to_string()
        },
    };
    println!("    text={:#?}", body1);
    println!("    json={:#?}", parse_json_to_map(&body1));

}

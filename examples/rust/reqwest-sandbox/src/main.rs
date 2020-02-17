use std::collections::HashMap;

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

fn main () {
    let res1 = test1();
    println!("test1: res={:#?}", res1);
    let res2 = test2("http://localhost.localdomain.com:5080/tix/js/version.js");
    println!("test2: res={:#?}", res2);

    println!("should 404    res={:#?}", test2("http://localhost.localdomain.com:5080/version.js"));
    println!("should 404    res={:#?}", test3("http://localhost.localdomain.com:5080/version.js"));
}

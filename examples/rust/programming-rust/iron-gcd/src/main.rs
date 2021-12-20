extern crate iron;
// NB: #[macro_use] is an 'attribute', this speific one imports (?)
// macros from the crate
// Q: what other attributes are there?
// Q: this attributes applies to the 'extern crate' statement, what
//    other statements can atributes apply to?
// Q: if macro_use imports (?) macros, what namespace does it pull
//    them from?  Is the crates name also a namespace?  or how does
//    the crate declare it's default exports?
// Q: can we implement our own attributes?
#[macro_use]
extern crate mime;
extern crate router;
extern crate urlencoded;
extern crate hellolib;

use iron::prelude::*;
use iron::status;
use router::Router;
use std::str::FromStr;
use urlencoded::UrlEncodedBody;


fn main() {
    let server_port = "localhost:3000";

    let mut router = Router::new();
    router.get("/", get_form);
    router.post("/gcd", post_gcd);

    println!("Serving on {}...", server_port);
    Iron::new(router).http(server_port).unwrap();
}


#[allow(unused_variables)]
fn get_form(request: &mut Request) -> IronResult<Response> {
    let mut response = Response::new();

    response.set_mut(status::Ok);
    response.set_mut(mime!(Text/Html; Charset=Utf8));
    response.set_mut(r#"
<html>
  <head>
    <title>GCD Calculator</title>
  </head>
  <body>
    <form action="/gcd" method="post">
      <input type="text" name="n" />
      <input type="text" name="n" />
      <button type="submit">Compute GCD</button>
    </form>
  </body>
  </html>
"#);

    Ok(response)
}

fn post_gcd(request: &mut Request) -> IronResult<Response> {
    let mut response = Response::new();

    let hashmap;
    match request.get_ref::<UrlEncodedBody>() {
        Err(e) => {
            response.set_mut(status::BadRequest);
            response.set_mut(format!("Error parsing form data: {:?}\n", e));
            return Ok(response);
        }

        Ok(map) => {
            hashmap = map;
        }
    }

    let unparsed_numbers;
    match hashmap.get("n") {
        None => {
            response.set_mut(status::BadRequest);
            response.set_mut(format!("Error: form-data has no 'n' parameter!\n"));
            return Ok(response);
        }
        Some(nums) => {
            unparsed_numbers = nums;
        }
    }

    let mut numbers = Vec::new();
    for unparsed in unparsed_numbers {
        match u64::from_str(&unparsed) {
            Err(ee) => {
                response.set_mut(status::BadRequest);
                response.set_mut(format!("Value for 'n' parameter ({}) not a number! {:?}\n",
                                         unparsed,
                                         ee));
                return Ok(response);
            }
            Ok(nn) => {
                numbers.push(nn);
            }
        }
    }

    let mut dd = numbers[0];
    for mm in &numbers[1..] {
        dd = hellolib::gcd(dd, *mm);
    }

    response.set_mut(status::Ok);
    response.set_mut(mime!(Text/Html; Charset = Utf8));
    response.set_mut(format!(r#"
<html>
<head>
</head>
<body>
  <h1>Result</h1>
  <p>The greatest common divisor of the numbers {:?} is <b>{}</b></p>
</body>
</html>
"#,
                             numbers,
                             dd));

    Ok(response)
}

#![allow(dead_code)]

#[derive(Debug)]
struct Person {
    name: String,
    age: u8,
}

struct Unit;
struct Pair(i32, f32);

#[derive(Debug)]
struct Point {
    x: f32,
    y: f32,
}

#[derive(Debug)]
struct Rectangle {
    top_left: Point,
    bottom_right: Point,
}

fn rect_area(rect: &Rectangle) -> f32 {
    let Point { x: x1, y: y1 } = rect.top_left;
    let Point { x: x2, y: y2 } = rect.bottom_right;

    return ((x2 - x1) * (y2 - y1)).abs();
}

fn square(pt: Point, len: f32) -> Rectangle {
    return Rectangle {
        top_left: Point { ..pt },
        bottom_right: Point { x: pt.x + len, y: pt.y + len },
    }
}

fn main() {
    let name = String::from("Peter");
    let age = 27;
    let peter = Person { name, age };
    let paul = Person { name: String::from("paul"), age: 32 };

    println!("peter: {:?}", peter);
    println!("paul: {:?}", paul);

    let point: Point = Point { x: 10.3, y: 0.4 };
    println!("point coordinates: ({}, {})", point.x, point.y);

    let bottom_right = Point { x: 5.2, ..point };
    println!("second point: ({}, {})", bottom_right.x, bottom_right.y);

    let Point { x: left_edge, y: top_edge } = point;

    let _rectangle = Rectangle {
        top_left: Point { x: left_edge, y: top_edge },
        bottom_right: bottom_right,
    };

    let _unit = Unit;

    let pair = Pair(1, 0.1);

    println!("pair contains {:?} and {:?}", pair.0, pair.1);

    let Pair(integer, decimal) = pair;

    println!("pair contains {:?} and {:?}", integer, decimal);

    let rect2 = Rectangle {
        top_left: Point {x: 0.0, y: 0.0},
        bottom_right: Point {x: 10.0, y: 12.0},
    };

    println!("area of rect2({:?}): {}", rect2, rect_area(&rect2));

    let sq = square(rect2.bottom_right, 17.3);
    println!("sq={:?}", sq);
}

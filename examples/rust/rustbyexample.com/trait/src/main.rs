// http://rustbyexample.com/trait.html

struct Sheep {
    naked: bool,
    name: &'static str
}

trait Animal {
    fn new(name: &'static str) -> Self;
    fn name(&self) -> &'static str;
    fn noise(&self) -> &'static str;

    fn talk(&self) {
        println!("{} says {}", self.name(), self.noise());
    }
}

impl Sheep {
    fn is_naked(&self) -> bool {
        self.naked
    }

    fn shear(&mut self) {
        if self.is_naked() {
            println!("{} is already naked!", self.name());
        } else {
            println!("{} has been sheared.", self.name());
            self.naked = true;
        }
    }
}

impl Animal for Sheep {
    fn new(name: &'static str) -> Sheep {
        Sheep { name: name, naked: false }
    }

    fn name(&self) -> &'static str {
        self.name
    }

    fn noise(&self) -> &'static str {
        if self.is_naked() {
            "baaah?"
        } else {
            "baaah!"
        }
    }
    // NB; the default trait methods can be overriden
    fn talk(&self) {
        println!("{} clears its throat and says: {}", self.name(), self.noise());
    }
}

fn main () {
    let mut shawn: Sheep = Animal::new("Shawn");
    // NOT OK: let shawn: Sheep = Animal::new("Shawn");
    // NOT OK: let mut shawn = Animal::new("Shawn");
    //     OK: let mut shawn = Sheep::new("Shawn");

    shawn.talk();
    shawn.shear();
    shawn.talk();
}

// http://eli.thegreenplace.net/2016/a-polyglots-guide-to-multiple-dispatch
#include <iostream>
#include <memory>

// g++ -std=c++11 -Wall multi.cpp

class Shape {
public:
  virtual void ComputeArea() const = 0;

  virtual std::string name() const {
    return typeid(*this).name();
  }
};

class Rectangle : public Shape {
public:
  virtual void ComputeArea() const {
    std::cout << "Rectangle: width times height\n";
  }
};

class Ellipse : public Shape {
public:
  virtual void ComputeArea() const {
    std::cout << "Ellipse: width times height times pi/4\n";
  }
};

class Triangle : public Shape {
public:
  virtual void ComputeArea() const {
    std::cout << "Triangle: width times height/2\n";
  }
};

void Intersect(const Rectangle* l, const Ellipse* r) {
  std::cout << "Rectangle x Ellipse [names l=" << l->name() << ", r=" << r->name() << "]" << std::endl;
}

void Intersect(const Rectangle* l, const Rectangle* r) {
  std::cout << "Rectangle x Rectangle [names l=" << l->name() << ", r=" << r->name() << "]" << std::endl;
}

void Intersect(const Shape* l, const Shape* r) {
  std::cout << "Shape x Shape [names l=" << l->name() << ", r=" << r->name() << "]" << std::endl;
}

int main(int argc, char** argv) {
  std::unique_ptr<Rectangle> pr(new Rectangle);
  std::unique_ptr<Ellipse>   pe(new Ellipse);
  std::unique_ptr<Triangle>  pt(new Triangle);

  pr->ComputeArea();
  pe->ComputeArea();
  pt->ComputeArea();

  std::cout << "Static type dispatch" << std::endl;
  
  Intersect(pr.get(), pe.get());
  Intersect(pr.get(), pr.get());
  Intersect(pr.get(), pt.get());

  // cool, fn() overloading

  std::unique_ptr<Shape>   pr2(new Rectangle);
  std::unique_ptr<Shape>   pr3(new Rectangle);
  std::unique_ptr<Shape>   pe2(new Ellipse);
  std::unique_ptr<Shape>   pt2(new Triangle);

  std::cout << "Dynamic type dispatch" << std::endl;

  Intersect(pr2.get(), pe2.get());
  Intersect(pr2.get(), pr3.get());
  Intersect(pr2.get(), pt2.get());
  

  // whoops, these all fell back to Intersect on two Shape's :(

  return 0;
}

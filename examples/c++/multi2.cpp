// http://eli.thegreenplace.net/2016/a-polyglots-guide-to-multiple-dispatch
#include <iostream>
#include <memory>

// g++ -std=c++11 -Wall multi2.cpp

class Rectangle;
class Ellipse;

class Shape {
public:
  virtual void ComputeArea() const = 0;
  virtual std::string name() const {
    return typeid(*this).name();
  }

  // NB: Intersect has to be virtual & can't just be implemented in Shape or the dispatch wont happen.
  virtual void Intersect(const Shape*) const = 0;
  virtual void IntersectWith(const Shape*) const = 0;
  virtual void IntersectWith(const Rectangle*) const = 0;
  virtual void IntersectWith(const Ellipse*) const = 0;
};

class Ellipse : public Shape {
public:
  virtual void ComputeArea() const;
  virtual void Intersect(const Shape* s) const;
  virtual void IntersectWith(const Shape* s) const;
  virtual void IntersectWith(const Rectangle* r) const;
  virtual void IntersectWith(const Ellipse* e) const;
};

class Rectangle : public Shape {
public:
  virtual void ComputeArea() const;
  virtual void Intersect(const Shape* s) const;
  virtual void IntersectWith(const Shape* s) const;
  virtual void IntersectWith(const Rectangle* r) const;
  virtual void IntersectWith(const Ellipse* e) const;
};


////////////////////////////////////////
// Ellipse >>
void Ellipse::ComputeArea() const {
  std::cout << "Ellipse: width times height times pi/4\n";
}

void Ellipse::Intersect(const Shape* s) const {
  s->IntersectWith(this);
}

void Ellipse::IntersectWith(const Shape* s) const {
  std::cout << "Ellipse x Shape [names this=" << this->name() << ", s=" << s->name() << "]" << std::endl;
}
  
void Ellipse::IntersectWith(const Rectangle* r) const {
  std::cout << "Ellipse x Rectangle [names this=" << this->name() << ", r=" << r->name() << "]" << std::endl;
}
  
void Ellipse::IntersectWith(const Ellipse* e) const {
  std::cout << "Ellipse x Ellipse [names this=" << this->name() << ", e=" << e->name() << "]" << std::endl;
}
// << Ellipse
////////////////////////////////////////

////////////////////////////////////////
// Rectangle >>
void Rectangle::ComputeArea() const {
  std::cout << "Rectangle: width times height\n";
}

void Rectangle::Intersect(const Shape* s) const {
  s->IntersectWith(this);
}

void Rectangle::IntersectWith(const Shape* s) const {
  std::cout << "Rectangle x Shape [names this=" << this->name() << ", s=" << s->name() << "]" << std::endl;
}

void Rectangle::IntersectWith(const Rectangle* r) const {
  std::cout << "Rectangle x Rectangle [names this=" << this->name() << ", r=" << r->name() << "]" << std::endl;
}

void Rectangle::IntersectWith(const Ellipse* e) const {
  std::cout << "Rectangle x Ellipse [names this=" << this->name() << ", e=" << e->name() << "]" << std::endl;
}
// << Rectangle
////////////////////////////////////////


int main(int argc, char** argv) {
  std::unique_ptr<Shape>   pr1(new Rectangle);
  std::unique_ptr<Shape>   pr2(new Rectangle);
  std::unique_ptr<Shape>   pe1(new Ellipse);

  std::cout << "Dynamic type dispatch" << std::endl;

  pr1->Intersect(pe1.get());
  pr1->Intersect(pr2.get());
  
  return 0;
}

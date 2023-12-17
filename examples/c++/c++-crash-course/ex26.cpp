#include <memory>
#include <iostream>

struct Foundation {
  const char* founder;

  ~Foundation () {
    std::cout << "Foundation[" << this->founder << "] has collapsed!" << std::endl;
  }
};

int main () {
  std::unique_ptr<Foundation> second_foundation { new Foundation{} };
  second_foundation->founder = "Wanda";
}

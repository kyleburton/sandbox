#include <vector>
#include <algorithm>
#include <iostream>

template <typename T>
T add(T x, T y, T z) {
  return x + y + z;
}

struct Hal {
  Hal() : version{ 9000 } {
    std::cout << "I'm completly operational (ver=" << this->version << ")." << std::endl;
  }

  Hal(const int ver) : version{ ver } {
    std::cout << "I'm completly operational (ver=" << this->version << "." << std::endl;
  }

  ~Hal() {
    std::cout << "Stop, Dave (ver=" << this->version << ")." << std::endl;
  }

  const int version;
};

int main () {
  std::vector<int> x { 0, 1, 8, 13, 5, 2, 3 };
  x[0] = 21;
  x.push_back(1);
  std::sort(x.begin(), x.end());
  std::cout << "Printing " << x.size() << " Fibionacci numbers." << std::endl;
  for( auto num : x ) {
    std::cout << num << std::endl;
  }


  auto n_evens = std::count_if(
      x.begin(),
      x.end(),
      [] (auto num) { return num % 2 == 0; });
  std::cout << "n_evens = " << n_evens << std::endl;

  auto a = add(1,   2,   3);      // a  is an int
  auto b = add(1L,  2L,  3L);     // b  is a a long
  auto c = add(1.F, 2.F, 3.F);    // c  is a a float
  std::cout << "a=" << a << std::endl;
  std::cout << "b=" << b << std::endl;
  std::cout << "c=" << c << std::endl;


  Hal h1;
  Hal h2(9001);
  auto h3 = new Hal{ 2001 };
  delete h3;
}

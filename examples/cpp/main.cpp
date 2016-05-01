// http://preshing.com/20141202/cpp-has-become-more-pythonic/
#include <iostream>
#include <bitset>
#include <regex>
#include <sstream>
#include <unordered_map>

// using namespace std;

template <class T>
std::string vector_to_string(std::vector<T> vec) {
  std::stringstream ss;
  for(size_t i = 0; i < vec.size(); ++i) {
      if(i != 0)
            ss << ",";
        ss << vec[i];
  }
  return ss.str();
}


auto adder(int amount) {
//typedef int (*intfp)(int);
//intfp adder(int amount) {
  return [=](int x){ return x + amount; };
}

int main (int argc, char **argv) {
  std::cout << "test out c++14" << std::endl;
  std::cout << "argc=" << argc << std::endl;
  std::cout << "argv=" << argv << std::endl;
  static const unsigned long primes = 0b10100000100010100010100010101100;
  std::cout << "primes=0b" << std::bitset<32>(primes) << std::endl;
  const char* path = R"(c:\this\string\has\backslashes)";
  std::cout << "path=" << path << std::endl;
  const char* pattern = R"((\d{4})[-/ ](\d{2})[-/ ](\d{2}))";
  std::cout << "pattern=" << pattern << std::endl;

  std::string dates[] = {
    "2016-04-30",
    "2000-01-01",
  };

  std::regex r (pattern);
  std::smatch m;

  for (auto s: dates) {
    std::cout << "date=" << s << std::endl;
    while (std::regex_search (s, m, r)) {
      for (auto x: m) {
        std::cout << x << " ";
      }
      std::cout << std::endl;
      s = m.suffix().str();
    }
  }

  auto tup = std::make_tuple(1, 2, 3, 4);
  // std::cout << "tuple=" << tup << std::endl;
  std::cout << "  0 => " << std::get<0>(tup) << std::endl;
  std::cout << "  1 => " << std::get<1>(tup) << std::endl;
  std::cout << "  2 => " << std::get<2>(tup) << std::endl;
  std::cout << "  3 => " << std::get<3>(tup) << std::endl;
  // std::cout << "  4 => " << std::get<4>(tup) << std::endl;
  int aa, bb, cc, dd;
  std::tie(aa, bb, cc, dd) = tup;
  std::cout << "  aa => " << aa << std::endl;
  std::cout << "  bb => " << bb << std::endl;
  std::cout << "  cc => " << cc << std::endl;
  std::cout << "  dd => " << dd << std::endl;


  auto myList = std::vector<int>{ 6, 3, 7, 8 };
  myList.push_back(5);
  std::cout << "myList=" << vector_to_string(myList) << std::endl;

  auto myDict = std::unordered_map<int, const char*>{ { 5, "foo" }, { 6, "bar" } };
  std::cout << "myDict[5]=" << myDict[5] << std::endl;


  std::sort(
      myList.begin(), 
      myList.end(),
      [](int x, int y){
        return std::abs(x) < std::abs(y);
      }
  );
  std::cout << "myList(sorted)=" << vector_to_string(myList) << std::endl;

  std::cout << adder(5)(5) << std::endl;
  auto fn1 = adder(5);
  for (auto ii = 0; ii < 10; ++ii) {
    std::cout << "fn1(" << ii << ")" << fn1(ii) << std::endl;
  }

  // ... stopped at Standard Algorithms

  return 0;
}

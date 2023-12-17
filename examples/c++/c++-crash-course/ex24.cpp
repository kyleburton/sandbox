#include <system_error>
#include <cstdio>
#include <iostream>

struct File {
  File(const char* fname, bool write) {
    auto file_mode = write ? "w" : "r";
    file_pointer = fopen(fname, file_mode);
    if (!file_pointer) {
      throw std::system_error(errno, std::system_category());
    }
  }

  ~File() {
    fclose(file_pointer);
  }

  FILE* file_pointer;
};

int main () {
  File f1("Bakefile", false);
  File f2("no-such-file", false);
  return 0;
}

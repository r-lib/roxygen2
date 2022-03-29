#include <cpp11/integers.hpp>
#include <cpp11/strings.hpp>

int leadingSpacesOne(std::string line) {
  int n = line.size();
  for(int i = 0; i < n; ++i) {
    char cur = line[i];

    if (cur != ' ') return(i);
  }

  return n;
}

[[cpp11::register]]
cpp11::integers leadingSpaces(cpp11::strings lines) {
  int n = lines.size();
  cpp11::writable::integers out(n);

  for(int i = 0; i < n; ++i) {
    out[i] = leadingSpacesOne(lines[i]);
  }
  return out;
}


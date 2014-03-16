#include <Rcpp.h>
using namespace Rcpp;

int leadingSpacesOne(std::string line) {
  int n = line.size();
  for(int i = 0; i < n; ++i) {
    char cur = line[i];

    if (cur != ' ') return(i);
  }

  return n;
}

// [[Rcpp::export]]
IntegerVector leadingSpaces(CharacterVector lines) {
  int n = lines.size();
  IntegerVector out(n);

  for(int i = 0; i < n; ++i) {
    String cur = lines[i];
    out[i] = leadingSpacesOne(cur);
  }
  return out;
}


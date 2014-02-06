#include <Rcpp.h>
using namespace Rcpp;

// From http://developer.r-project.org/parseRd.pdf:  The characters \, %, {,
// and } have special meaning in almost all parts of an Rd file.
// The algorithm currently returns false for "\\code{ '{' }" when it should
// return true. This seems like a rather unlikely edge cases so is ignored
// for now.
// [[Rcpp::export]]
bool rdComplete(std::string string) {
  int n = string.length();

  bool in_escape = false;
  bool in_comment = false;
  int braces = 0;

  for(int i = 0; i < n; i++) {
    char cur = string[i];

    if (in_comment) {
      if (cur == '\n') {
        in_comment = false;
      }
      continue;
    }

    if (in_escape) {
      in_escape = false;
      continue;
    }

    if (cur == '{') {
      braces++;
    } else if (cur == '}') {
      braces--;
    } else if (cur == '\\') {
      in_escape = true;
    } else if (cur == '%') {
      in_comment = true;
    }
  }

  return braces == 0 && !in_escape;
}

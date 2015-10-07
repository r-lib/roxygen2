#include <Rcpp.h>
using namespace Rcpp;

// From http://developer.r-project.org/parseRd.pdf:  The characters \, %, {,
// and } have special meaning in almost all parts of an Rd file. In code,
// strings must also match
// [[Rcpp::export]]
bool rdComplete(std::string string, bool is_code = true) {
  int n = string.length();

  char in_string = '\0';
  bool in_escape = false;
  bool in_comment = false;
  int braces = 0;

  for(int i = 0; i < n; i++) {
    char cur = string[i];

    if (in_string != '\0') {
      if (cur == in_string) {
        in_string = false;
      }
      continue;
    }

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

    switch(cur) {
    case '{':  braces++; break;
    case '}':  braces--; break;
    case '\\': in_escape = true; break;
    case '%':  in_comment = true; break;
    case '\'': if (is_code) in_string = '\''; break;
    case '"':  if (is_code) in_string = '"'; break;
    }
  }

  return braces == 0 && !in_escape && !in_string;
}

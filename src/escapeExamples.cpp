#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::string escapeExamples(std::string x) {
  std::string out;
  out.reserve(x.length() * 1.1);

  char in_string = '\0';
  bool in_escape = false;

  std::string::const_iterator cur, end = x.end();
  for (cur = x.begin(); cur != end; cur++) {
    if (in_string == '\0') {
      if (*cur == '\'' || *cur == '"' || *cur == '`') {
        in_string = *cur;
      }
    } else {
      if (in_escape) {
        in_escape = false;
        if (*cur == 'l' || *cur == 'v') {
          out += '\\';
        } else if (*cur == '\\') {
          out += "\\\\";
        }
      } else {
        if (*cur == in_string) {
          in_string = '\0';
        } else if (*cur == '\\') {
          in_escape = true;
        }
      }
    }

    if (*cur == '%') {
      out += '\\';
    }

    out += *cur;
  }

  return out;
}

#include <Rcpp.h>
using namespace Rcpp;

// From http://developer.r-project.org/parseRd.pdf:  The characters \, %, {,
// and } have special meaning in almost all parts of an Rd file. In code,
// strings must also match, except in comments.
// [[Rcpp::export]]
bool rdComplete(std::string string, bool is_code = false) {
  int n = string.length();

  char in_string = '\0';
  bool in_escape = false;
  bool in_r_comment = false;
  bool in_latex_comment = false;
  int braces = 0, r_braces = 0;

  for(int i = 0; i < n; i++) {
    char cur = string[i];

    if (in_escape) {
      // Swallow escaped characters
      in_escape = false;
    } else if (in_string != '\0') {
      // Look for end of string
      if (cur == in_string) {
        in_string = false;
      } else if (cur == '\\') {
        in_escape = true;
      }
    } else if (in_r_comment) {
      // Inside R comments, braces must match.
      // R comments are terminated by newline or } not matched by {
      if (cur == '\n') {
        in_r_comment = false;
        r_braces = 0;
      } else if (cur == '{') {
        braces++;
        r_braces++;
      } else if (cur == '}') {
        braces--;
        r_braces--;
        if (r_braces == 0)
          in_r_comment = false;
      }
    } else if (in_latex_comment) {
      if (cur == '\n') {
        in_latex_comment = false;
      }
    } else {
      switch(cur) {
      case '{':  braces++; break;
      case '}':  braces--; break;
      case '\\': in_escape = true; break;
      case '#':  if (is_code) in_r_comment = true; break;
      case '%':  in_latex_comment = true; break;
      case '\'': if (is_code) in_string = '\''; break;
      case '"':  if (is_code) in_string = '"'; break;
      }
    }

  }

  return braces == 0 && !in_escape && !in_string;
}

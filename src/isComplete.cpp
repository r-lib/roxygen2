#include <Rcpp.h>
using namespace Rcpp;

// From http://developer.r-project.org/parseRd.pdf:  The characters \, %, {,
// and } have special meaning in almost all parts of an Rd file. In code,
// strings must also match, except in comments.

// The two functions are very similar, so we use a common
// implementation and select the functionality via the
// mode argument:
// mode == 0: rdComplete
// mode == 1: findEndOfTag

int roxygen_parse_tag(std::string string, bool is_code = false,
		      int mode = 0) {
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

    if (mode == 1) {
      bool complete = braces == 0 && !in_escape && !in_string;
      if (complete && i + 1 < n && string[i + 1] != '{') {
	return i;
      }
    }

  }

  bool complete = braces == 0 && !in_escape && !in_string;

  if (mode == 0) {
    if (complete) return 1; else return 0;

  } else {
    if (complete) return n - 1; else return -1;
  }
}

// [[Rcpp::export]]
int findEndOfTag(std::string string, bool is_code = false) {
  return roxygen_parse_tag(string, is_code, 1);
}

// [[Rcpp::export]]
bool rdComplete(std::string string, bool is_code = false) {
  return roxygen_parse_tag(string, is_code, 0) == 1 ? true : false;
}

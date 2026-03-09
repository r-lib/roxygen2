#include <cpp11/R.hpp>
#include <string>

// From http://developer.r-project.org/parseRd.pdf:  The characters \, %, {,
// and } have special meaning in almost all parts of an Rd file. In code,
// strings must also match, except in comments.

enum class State { Rd, RdEscape, String, StringEscape, RComment, RdComment };

bool is_complete(int braces, State state) {
  return braces == 0 && (state == State::Rd || state == State::RComment ||
                         state == State::RdComment);
}

// The two functions are very similar, so we use a common
// implementation and select the functionality via the
// mode argument:
// mode == 0: rdComplete
// mode == 1: findEndOfTag

int roxygen_parse_tag(std::string string, bool is_code = false, int mode = 0) {
  int n = string.length();

  State state = State::Rd;
  char string_delim = '\0';
  int braces = 0, r_braces = 0;

  for (int i = 0; i < n; i++) {
    char cur = string[i];

    switch (state) {
    case State::Rd:
      switch (cur) {
      case '{':
        braces++;
        break;
      case '}':
        braces--;
        break;
      case '\\':
        state = State::RdEscape;
        break;
      case '#':
        if (is_code)
          state = State::RComment;
        break;
      case '%':
        state = State::RdComment;
        break;
      case '\'':
        if (is_code) {
          state = State::String;
          string_delim = '\'';
        }
        break;
      case '"':
        if (is_code) {
          state = State::String;
          string_delim = '"';
        }
        break;
      }
      break;

    case State::RdEscape:
      state = State::Rd;
      break;

    case State::String:
      if (cur == string_delim) {
        state = State::Rd;
        string_delim = '\0';
      } else if (cur == '\\') {
        state = State::StringEscape;
      }
      break;

    case State::StringEscape:
      state = State::String;
      break;

    case State::RComment:
      // We don't trace braces in comment independently to make it possible to
      // close a multi-line Rd expression in a comment. This is a hack that 
      // needed to support @examplesIf
      if (cur == '\n') {
        state = State::Rd;
      } else if (cur == '{') {
        braces++;
      } else if (cur == '}') {
        braces--;
      }
      break;

    case State::RdComment:
      if (cur == '\n') {
        state = State::Rd;
      }
      break;
    }

    if (mode == 1) {
      bool complete = is_complete(braces, state);
      if (complete && i + 1 < n && string[i + 1] != '{') {
        return i;
      }
    }
  }

  bool complete = is_complete(braces, state);
  if (mode == 0) {
    return complete ? 1 : 0;
  } else {
    return complete ? n - 1 : -1;
  }
}

[[cpp11::register]]
int findEndOfTag(std::string string, bool is_code) {
  return roxygen_parse_tag(string, is_code, 1);
}

[[cpp11::register]]
bool rdComplete(std::string string, bool is_code) {
  return roxygen_parse_tag(string, is_code, 0) == 1 ? true : false;
}

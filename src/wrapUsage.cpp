#include <cpp11/strings.hpp>
#include <vector>
#include <string>

std::vector<std::string> splitByWhitespace(std::string string) {
  std::vector<std::string> out;

  std::string acc = "";
  char in_string = '\0';
  int in_escape = 0;

  std::string::const_iterator cur = string.begin(), end = string.end();

  while(cur != end) {
    if (in_string != '\0') {
      acc += *cur;

      if (in_escape) {
        in_escape--;
      } else if (*cur == '\\' && cur + 1 != end && *(cur + 1) == '\\') {
        in_escape = 2;
      } else if (*cur == in_string) {
        // String terminates
        in_string = '\0';
      }

    } else if (*cur == ' ' || *cur == '\t' || *cur == '\n') {
      out.push_back(acc);
      acc = "";
    } else if (*cur == '"' || *cur == '\'') {
      in_string = *cur;
      acc += *cur;
    } else {
      acc += *cur;
    }

    cur++;
  }

  out.push_back(acc);

  return out;
}

[[cpp11::register]]
std::string wrapUsage(std::string string, int width, int indent) {
  std::vector<std::string> pieces = splitByWhitespace(string);
  int n = pieces.size();
  int cur_width = 0;

  std::string out;

  for (int i = 0; i < n; ++i) {
    int piece_width = pieces[i].size();

    // Need to include space for a space
    if (piece_width + cur_width + 1 < width) {
      cur_width += piece_width;
      if (i != 0) {
        out += " ";
        cur_width++;
      }
    } else {
      cur_width = piece_width + indent;
      out += "\n" + std::string(indent, ' ');
    }
    out += pieces[i];
  }

  return out;
}

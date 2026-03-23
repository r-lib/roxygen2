#include <cpp11/R.hpp>
#include <cpp11/list.hpp>
#include <cpp11/strings.hpp>
#include <string>
#include <unordered_set>
#include <vector>

// Rd tags that are "fragile" and must be protected from markdown parsing.
// If you update this list, also update the hardcoded list in vignettes/rd-formatting.Rmd.
// clang-format off
static const std::unordered_set<std::string> fragile_tags = {
  "\\acronym", "\\code", "\\command", "\\CRANpkg",
  "\\deqn", "\\doi", "\\dontrun", "\\dontshow", "\\donttest",
  "\\email", "\\env", "\\eqn",
  "\\figure", "\\file",
  "\\if", "\\ifelse",
  "\\kbd",
  "\\link", "\\linkS4class",
  "\\method", "\\mjeqn", "\\mjdeqn", "\\mjseqn", "\\mjsdeqn",
  "\\mjteqn", "\\mjtdeqn",
  "\\newcommand",
  "\\option", "\\out",
  "\\packageAuthor", "\\packageDescription", "\\packageDESCRIPTION",
  "\\packageIndices", "\\packageMaintainer", "\\packageTitle",
  "\\pkg", "\\PR", "\\preformatted",
  "\\renewcommand",
  "\\S3method", "\\S4method", "\\samp", "\\special",
  "\\testonly",
  "\\url",
  "\\var", "\\verb"
};
// clang-format on

static const std::string placeholder_id = "ROXYGEN-PLACEHOLDER";

// Double-escape a backslash into the output buffer, except \[ and \]
static void double_escape_char(std::string& out, const std::string& text, int& i) {
  int n = text.length();
  if (i + 1 < n && (text[i + 1] == '[' || text[i + 1] == ']')) {
    out += '\\';
    out += text[i + 1];
    i++;
  } else {
    out += '\\';
    out += '\\';
  }
}

// Single-pass escape: scans text once, double-escaping normal text and
// replacing top-level fragile Rd tags with placeholders.
[[cpp11::register]]
cpp11::list escape_rd_for_md_c(std::string text) {
  using namespace cpp11;

  int n = text.length();
  std::string output;
  output.reserve(n + n / 4);
  std::vector<std::string> captures;

  // State for capturing inside a fragile tag
  // - RD: normal Rd text inside a tag
  // - RD_ESCAPE: just saw \ inside a tag (skip one char for brace counting)
  // - RD_COMMENT: inside a % comment (braces not counted)
  enum class Sub { RD, RD_ESCAPE, RD_COMMENT };
  bool capturing = false;
  std::string capture_buf;
  int braces = 0;
  Sub sub = Sub::RD;

  int i = 0;
  while (i < n) {
    if (!capturing) {
      // NORMAL mode: double-escape text, detect fragile tags
      if (text[i] == '\\' && i + 1 < n && std::isalpha(text[i + 1])) {
        // Read tag name
        int tag_start = i;
        int j = i + 1;
        while (j < n && std::isalnum(text[j])) {
          j++;
        }
        std::string tag_name = text.substr(tag_start, j - tag_start);

        if (fragile_tags.count(tag_name)) {
          // Start capturing this fragile tag
          capturing = true;
          braces = 0;
          sub = Sub::RD;
          capture_buf = tag_name;
          i = j;

          // Check if the tag has arguments (next char must be '{')
          if (i >= n || text[i] != '{') {
            // Tag is complete with no arguments
            captures.push_back(capture_buf);
            output += placeholder_id + "-" + std::to_string(captures.size()) + "-";
            capturing = false;
          }
          continue;
        } else {
          // Not fragile: double-escape the backslash, output tag name
          double_escape_char(output, text, i);
          i++;
          // Output the rest of the tag name (alpha chars after the \)
          while (i < n && std::isalnum(text[i])) {
            output += text[i];
            i++;
          }
          continue;
        }
      } else if (text[i] == '\\') {
        double_escape_char(output, text, i);
        i++;
        continue;
      } else {
        output += text[i];
        i++;
        continue;
      }
    } else {
      // CAPTURE mode: accumulate text, track Rd state machine
      capture_buf += text[i];

      switch (sub) {
      case Sub::RD:
        if (text[i] == '{') {
          braces++;
        } else if (text[i] == '}') {
          braces--;
        } else if (text[i] == '\\') {
          sub = Sub::RD_ESCAPE;
        } else if (text[i] == '%') {
          sub = Sub::RD_COMMENT;
        }
        break;
      case Sub::RD_ESCAPE:
        sub = Sub::RD;
        break;
      case Sub::RD_COMMENT:
        if (text[i] == '\n') {
          sub = Sub::RD;
        }
        break;
      }

      // Check if the tag is complete
      bool complete = braces == 0 && (sub == Sub::RD || sub == Sub::RD_COMMENT);
      if (complete && (i + 1 >= n || text[i + 1] != '{')) {
        captures.push_back(capture_buf);
        output += placeholder_id + "-" + std::to_string(captures.size()) + "-";
        capturing = false;
      }

      i++;
    }
  }

  // If we were still capturing at end of string (incomplete tag), flush as-is
  if (capturing) {
    // Double-escape the capture buffer since it wasn't a complete tag
    for (int k = 0; k < (int)capture_buf.length(); k++) {
      if (capture_buf[k] == '\\') {
        double_escape_char(output, capture_buf, k);
      } else {
        output += capture_buf[k];
      }
    }
  }

  writable::strings tag_texts(captures.size());
  for (size_t j = 0; j < captures.size(); j++) {
    tag_texts[j] = captures[j];
  }

  writable::list result({
    "text"_nm = output,
    "tags"_nm = tag_texts
  });
  return result;
}

[[cpp11::register]]
std::string unescape_rd_for_md_c(std::string rd_text, cpp11::strings tags) {
  if (tags.size() == 0) {
    return rd_text;
  }

  int id_len = placeholder_id.length();
  int n = rd_text.length();
  std::string output;
  output.reserve(n);

  int i = 0;
  while (i < n) {
    // Check if we're at the start of a placeholder
    if (rd_text[i] == placeholder_id[0] && i + id_len < n &&
        rd_text.compare(i, id_len, placeholder_id) == 0 &&
        rd_text[i + id_len] == '-') {
      // Parse the number between the two dashes: ROXYGEN-PLACEHOLDER-NUMBER-
      int j = i + id_len + 1;
      int num = 0;
      while (j < n && rd_text[j] >= '0' && rd_text[j] <= '9') {
        num = num * 10 + (rd_text[j] - '0');
        j++;
      }
      // Check for trailing dash and valid tag index
      if (j < n && rd_text[j] == '-' && num >= 1 && num <= tags.size()) {
        output += std::string(tags[num - 1]);
        i = j + 1;
        continue;
      }
    }
    output += rd_text[i];
    i++;
  }

  return output;
}

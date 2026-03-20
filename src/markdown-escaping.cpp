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
//
// Uses a simple state machine for the Rd parser (is_code=false):
// - RD: normal Rd text inside a tag
// - RD_ESCAPE: just saw \ inside a tag (skip one char for brace counting)
// - RD_COMMENT: inside a % comment (braces not counted)
[[cpp11::register]]
cpp11::list escape_rd_for_md_c(std::string text) {
  using namespace cpp11;

  int n = text.length();
  std::string output;
  output.reserve(n + n / 4);
  std::vector<std::string> captures;
  std::string id = "ROXYGEN-PLACEHOLDER";

  // State for capturing inside a fragile tag
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
            output += id + "-" + std::to_string(captures.size()) + "-";
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
        output += id + "-" + std::to_string(captures.size()) + "-";
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
    "id"_nm = captures.empty() ? "" : id,
    "tags"_nm = tag_texts
  });
  return result;
}

[[cpp11::register]]
std::string unescape_rd_for_md_c(std::string rd_text, std::string id,
                                  cpp11::strings tags) {
  for (R_xlen_t i = 0; i < tags.size(); i++) {
    std::string ph = id + "-" + std::to_string(i + 1) + "-";
    std::string replacement(tags[i]);
    size_t pos = rd_text.find(ph);
    if (pos != std::string::npos) {
      rd_text.replace(pos, ph.length(), replacement);
    }
  }
  return rd_text;
}

#include <cpp11/R.hpp>
#include <cpp11/list.hpp>
#include <cpp11/strings.hpp>
#include <algorithm>
#include <string>
#include <unordered_set>
#include <vector>

// Declared in isComplete.cpp
int roxygen_parse_tag(std::string string, bool is_code, int mode);

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

struct TagInfo {
  std::string tag;
  int start;  // 1-based, start of tag (the backslash)
  int end;    // 1-based, end of tag name
  int argend; // 1-based, end of arguments
};

// Find all \tagname patterns in text (where tagname matches [a-zA-Z][a-zA-Z0-9]*)
static std::vector<TagInfo> find_tag_names(const std::string& text) {
  std::vector<TagInfo> tags;
  int n = text.length();

  for (int i = 0; i < n; i++) {
    if (text[i] == '\\' && i + 1 < n && std::isalpha(text[i + 1])) {
      int start = i; // 0-based start
      int j = i + 1;
      while (j < n && std::isalnum(text[j])) {
        j++;
      }
      TagInfo info;
      info.tag = text.substr(start, j - start);
      info.start = start + 1; // Convert to 1-based
      info.end = j;           // 1-based (j-1 is 0-based end, +1 for 1-based)
      tags.push_back(info);
      i = j - 1; // Will be incremented by for loop
    }
  }

  return tags;
}

// Apply double-escaping for markdown:
// - Double all backslashes
// - But un-double \[ and \] (they should stay single-escaped)
static std::string double_escape(const std::string& text) {
  std::string result;
  result.reserve(text.length() + text.length() / 4);

  int n = text.length();
  for (int i = 0; i < n; i++) {
    if (text[i] == '\\') {
      if (i + 1 < n && (text[i + 1] == '[' || text[i + 1] == ']')) {
        // Don't double-escape \[ and \]
        result += '\\';
        result += text[i + 1];
        i++;
      } else {
        result += '\\';
        result += '\\';
      }
    } else {
      result += text[i];
    }
  }

  return result;
}

[[cpp11::register]]
cpp11::list escape_rd_for_md_c(std::string text) {
  using namespace cpp11;

  // Find all tag names
  std::vector<TagInfo> all_tags = find_tag_names(text);

  // Filter to fragile tags and find their argument ends
  std::vector<TagInfo> ftags;
  int text_len = text.length();
  for (auto& tag : all_tags) {
    if (fragile_tags.count(tag.tag)) {
      // Find end of arguments using existing C++ parser
      // end is 1-based, substr needs 0-based
      std::string tag_plus = text.substr(tag.end - 1, text_len - tag.end + 1);
      int result = roxygen_parse_tag(tag_plus, false, 1);
      tag.argend = result + tag.end;
      ftags.push_back(tag);
    }
  }

  // Remove embedded fragile tags (a fragile tag inside another fragile tag)
  std::vector<TagInfo> kept;
  for (size_t i = 0; i < ftags.size(); i++) {
    int count = 0;
    for (size_t j = 0; j < ftags.size(); j++) {
      if (ftags[j].start <= ftags[i].start &&
          ftags[j].argend >= ftags[i].argend) {
        count++;
      }
    }
    if (count == 1) {
      kept.push_back(ftags[i]);
    }
  }

  // If no fragile tags found, just double-escape and return
  if (kept.empty()) {
    std::string escaped = double_escape(text);
    writable::strings tag_texts;
    writable::list result({"text"_nm = escaped, "id"_nm = "", "tags"_nm = tag_texts});
    return result;
  }

  // Sort by start position
  std::sort(
      kept.begin(), kept.end(),
      [](const TagInfo& a, const TagInfo& b) { return a.start < b.start; });

  // Extract original text spans before replacement
  writable::strings tag_texts(kept.size());
  for (size_t i = 0; i < kept.size(); i++) {
    tag_texts[i] = text.substr(kept[i].start - 1, kept[i].argend - kept[i].start + 1);
  }

  static int counter = 0;
  std::string id = "ROXYGEN-PLACEHOLDER-" + std::to_string(counter++);

  // Replace fragile tags with placeholders (right to left to preserve indices)
  for (int i = kept.size() - 1; i >= 0; i--) {
    std::string placeholder = id + "-" + std::to_string(i + 1) + "-";
    text.replace(kept[i].start - 1, kept[i].argend - kept[i].start + 1, placeholder);
  }

  // Double-escape
  std::string escaped = double_escape(text);

  writable::list result({"text"_nm = escaped, "id"_nm = id, "tags"_nm = tag_texts});
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

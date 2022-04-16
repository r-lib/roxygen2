#include <cpp11/list.hpp>
#include <cpp11/list_of.hpp>
#include <cpp11/strings.hpp>
#include <cpp11/protect.hpp>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>

class RoxygenLine {
  std::string line_;
  const char* begin_;
  const char* end_;
  const char* cur_;

public:

  RoxygenLine(const std::string& line) : line_(line) {
    begin_ = cur_ = line_.data();
    end_ = begin_ + line_.size();
  }

  bool consumeChar(char c) {
    if (cur_ == end_ || *cur_ != c)
      return false;

    cur_++;
    return true;
  }

  int consumeWhitespace(int max = -1) {
    int i = 0;
    while (cur_ != end_ && std::isspace(*cur_)) {
      cur_++;
      i++;
      if (max > 0 && i >= max)
        break;
    }

    return i;
  }

  bool consumeRoxygenComment() {
    consumeWhitespace();

    if (!consumeChar('#'))
      return false;
    while (consumeChar('#'));

    if (!consumeChar('\''))
      return false;

    consumeWhitespace(1);

    return true;
  }

  bool consumeTag(std::string* pOut) {
    if (!consumeChar('@'))
      return false;

    while(cur_ != end_ && std::isalnum(*cur_) ) {
      pOut->push_back(*cur_);
      cur_++;
    }

    return true;
  }

  bool consumeText(std::string* pOut) {
    while (cur_ != end_) {
      if (isEscapedAt()) {
        pOut->push_back('@');
        cur_ += 2;
      } else {
        pOut->push_back(*cur_);
        cur_++;
      }
    }

    return true;
  }

  bool isEscapedAt() {
    if (cur_ == end_)
      return false;
    if (*cur_ != '@')
      return false;

    const char* next = cur_ + 1;
    if (next == end_)
      return false;

    return *next == '@';
  }

};

std::string stripTrailingNewline(std::string x) {
  if (x[x.size() - 1] == '\n') {
    x.resize(x.size() - 1);
  }

  return x;
}

[[cpp11::register]]
cpp11::list tokenise_block(cpp11::strings lines, std::string file,
                    int offset) {
  std::vector<std::string> tags, vals;
  std::vector<int> rows;

  int curRow = 0;
  std::string curTag(""), curVal("");

  for (int i = 0; i < lines.size(); ++i) {
    RoxygenLine line((std::string(lines[i])));

    if (!line.consumeRoxygenComment()) {
      // Incremenet curRow for non-roxygen comments at start of block
      if (curVal == "")
        curRow++;
      continue;
    }

    std::string tag;
    if (line.consumeTag(&tag)) {
      line.consumeWhitespace(1);

      if (curVal != "" || curTag != "") {
        rows.push_back(curRow);
        tags.push_back(curTag);
        vals.push_back(curVal);
      }

      curRow = i;
      curTag.assign(tag);
      curVal.assign("");
    }

    line.consumeText(&curVal);
    curVal.push_back('\n');
  }

  if (curVal != "" || curTag != "") {
    rows.push_back(curRow);
    tags.push_back(curTag);
    vals.push_back(curVal);
  }

  // Convert to a list
  R_xlen_t n = rows.size();
  cpp11::writable::list out(n);

  using namespace cpp11::literals;

  for (R_xlen_t i = 0; i < n; ++i) {
    cpp11::writable::list x({
        "file"_nm = file,
        "line"_nm = rows[i] + offset,
        "tag"_nm = tags[i],
        "raw"_nm = stripTrailingNewline(vals[i]),
        "val"_nm = R_NilValue
        });
    std::string tag("roxy_tag_");
    tag += tags[i];
    x.attr("class") = {tag.c_str(), "roxy_tag"};
    out[i] = x;
  }
  return out;
}

[[cpp11::register]]
cpp11::strings find_includes(std::string path) {
  std::vector<std::string> includes;

  std::string path_native = Rf_translateChar(cpp11::r_string(path));
  std::ifstream file(path_native.c_str());
  if (!file.good())
    cpp11::stop("Failed to open %s", path.c_str());

  std::string rawline;
  while (std::getline(file, rawline)) {
    RoxygenLine line(rawline);
    if (!line.consumeRoxygenComment())
      continue;

    std::string tag, value;
    if (!line.consumeTag(&tag))
      continue;
    if (tag != "include")
      continue;

    line.consumeWhitespace(1);

    // Split value by whitespace
    // http://stackoverflow.com/questions/236129/split-a-string-in-c
    line.consumeText(&value);

    std::istringstream words(value);
    copy(
      std::istream_iterator<std::string>(words),
      std::istream_iterator<std::string>(),
      back_inserter(includes)
    );
  }

  return cpp11::as_sexp(includes);
}

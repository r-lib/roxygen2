#include <Rcpp.h>
using namespace Rcpp;
#include <fstream>
#include <sstream>

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

// [[Rcpp::export]]
List tokenise_preref(CharacterVector lines, std::string file = "",
                     int offset = 0) {
  std::vector<std::string> tags, vals;
  std::vector<int> rows;

  int curRow = 0;
  std::string curTag(""), curVal("");

  for (int i = 0; i < lines.size(); ++i) {
    RoxygenLine line((std::string(lines[i])));

    if (!line.consumeRoxygenComment())
      continue;

    std::string tag;
    if (line.consumeTag(&tag)) {
      line.consumeWhitespace(1);

      if (curVal != "" || curTag != "") {
        rows.push_back(curRow);
        tags.push_back(curTag);
        vals.push_back(curVal);
      }

      curRow = i + offset;
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
  int n = rows.size();
  ListOf<List> out(n);

  for (int i = 0; i < n; ++i) {
    out[i] = List::create(
      _["file"] = file,
      _["line"] = rows[i] + 1,
      _["tag"] = tags[i],
      _["val"] = stripTrailingNewline(vals[i])
    );
    out[i].attr("class") = "roxygen_tag";
  }
  return out;
}

// [[Rcpp::export]]
CharacterVector find_includes(std::string path) {
  std::vector<std::string> includes;

  std::ifstream file(path.c_str());
  if (!file.good())
    stop("Failed to open %s", path);

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

  return wrap(includes);
}

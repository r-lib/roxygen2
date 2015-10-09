#include <Rcpp.h>
using namespace Rcpp;

class RoxygenLine {
  const char* begin_;
  const char* end_;
  const char* cur_;

public:

  RoxygenLine(std::string line) {
    begin_ = cur_ = line.data();
    end_ = begin_ + line.size();
  }

  size_t position() {
    return cur_ - begin_;
  }

  bool consumeChar(char c) {
    if (cur_ == end_ || *cur_ != c)
      return false;

    cur_++;
    return true;
  }

  bool consumeWhitespace() {
    while (cur_ != end_ && std::isspace(*cur_))
      cur_++;

    return true;
  }

  bool consumeRoxygenComment() {
    consumeWhitespace();

    if (!consumeChar('#'))
      return false;

    if (!consumeChar('\''))
      return false;

    consumeWhitespace();

    return true;
  }

  bool consumeTag(std::string* pOut) {
    if (!consumeChar('@'))
      return false;

    while(cur_ != end_ && std::isalpha(*cur_)) {
      pOut->push_back(*cur_);
      cur_++;
    }

    return true;
  }

  bool consumeText(std::string* pOut) {
    if (cur_ == end_)
      return false;

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

// [[Rcpp::export]]
List parse_block(CharacterVector lines, int offset = 0) {
  std::vector<std::string> tags, vals;
  std::vector<int> rows, cols;

  int curRow = 0, curCol = 0;
  std::string curTag(""), curVal("");

  for (int i = 0; i < lines.size(); ++i) {
    RoxygenLine line((std::string(lines[i])));

    if (!line.consumeRoxygenComment())
      continue;

    std::string tag;
    if (line.consumeTag(&tag)) {
      rows.push_back(curRow);
      cols.push_back(curCol);
      tags.push_back(curTag);
      vals.push_back(curVal);

      curRow = i + offset;
      curCol = line.position();
      curTag = tag;
      curVal = "";
    }

    line.consumeWhitespace();
    line.consumeText(&curVal);
  }

  if (curTag != "") {
    rows.push_back(curRow);
    cols.push_back(curCol);
    tags.push_back(curTag);
    vals.push_back(curVal);
  }

  // Convert to a list
  int n = rows.size();
  ListOf<List> out(n);

  for (int i = 0; i < n; ++i) {
    out[i] = List::create(
      _["row"] = rows[i],
      _["col"] = cols[i],
      _["tag"] = tags[i],
      _["val"] = vals[i]
    );
    out[i].attr("class") = "roxygen_tag";
  }
  return out;
}

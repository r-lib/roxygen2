#include "roxygen.h"
using namespace Rcpp;

namespace roxygen {

class BlockParser
{

public:

  explicit BlockParser(std::string const& input)
  {
    stripRoxygenDelimitersAndStore(input);
    filePath_ = std::string();
    row_ = 0;
  }

  BlockParser(std::string const& input,
              std::string const& filePath,
              int row)
  {
    stripRoxygenDelimitersAndStore(input);
    filePath_ = filePath;
    row_ = row;
  }

  List parse()
  {

    ListBuilder outputBuilder;

    // The initial row for this block
    int startRow = row_;

    // Find the location of the first opening tag -- everything
    // before that is wrapped into the 'introduction'
    int firstTagIndex = findFirstTagIndex();
    int index = firstTagIndex;

    LOG("First tag found at index '" << index << "'");

    if (firstTagIndex > 0)
    {
      LOG("Filling introduction:");
      LOG(substring(content_, 0, firstTagIndex));

      // NOTE: 'add' protects any added SEXP
      CharacterVector introduction = substring(content_, 0, firstTagIndex);
      addFileName(introduction, filePath_);
      addRow(introduction, startRow + 1);
      outputBuilder.add("introduction", introduction);
      LOG("Introduction ended at row: '" << row_ << "'");
    }

    while (index < n_)
    {
      // Lines should start with an '@'
      PARSE_CHECK('@', content_[index]);
      ++index;
      int tagStart = index;

      // Move over alphabetical characters (these constitute the tag)
      while (isAlpha(content_[index]))
        ++index;

      int tagEnd = index;
      std::string tag = substring(content_, tagStart, tagEnd);
      LOG("Handling tag '" << tag << "'");

      // NOTE: 'add' protects any added SEXP
      LOG("row_ before 'getRoxygenTagValue': '" << row_ << "'");
      CharacterVector tagValue = wrap(getRoxygenTagValue(index));
      LOG("row_ after 'getRoxygenTagValue': '" << row_ << "'");

      addFileName(tagValue, filePath_);
      addRow(tagValue, row_ + 1);
      outputBuilder.add(tag, tagValue);

      index = findNextTagOrEnd(index, &row_);

    }

    List output(outputBuilder);
    output.attr("delimiter") = roxyDelim_;
    return output;

  }

private:

  template <typename T>
  void addFileName(T const& x, std::string const& fileName)
  {
    Shield<SEXP> wrapped(wrap(fileName));
    Rf_setAttrib(x, Rf_install("file"), wrapped);
  }

  template <typename T>
  void addRow(T const& x, int row)
  {
    Shield<SEXP> wrapped(wrap(row));
    Rf_setAttrib(x, Rf_install("row"), wrapped);
  }

  std::string getRoxygenDelimiter(std::string const& input)
  {
    int index = 0;
    PARSE_CHECK('#', input[index]);
    while (input[index] == '#')
    {
      ++index;
    }

    PARSE_CHECK('\'', input[index]);
    ++index;

    while (isWhitespace(input[index]))
    {
      ++index;
    }
    return substring(input, 0, index);
  }

  void stripRoxygenDelimitersAndStore(std::string const& input)
  {
    // Store the roxygen delimiter format
    roxyDelim_ = getRoxygenDelimiter(input);

    // Strip out roxygen delimiters
    int n = input.length();
    content_.reserve(n);

    // Initialize some indices
    int start = 0;
    int end = 0;

    // Loop over all lines and strip
    while (true)
    {
      // Move 'start' over the roxygen delimiter
      PARSE_CHECK('#', input[start]);
      while (input[start] == '#')
      {
        ++start;
      }

      // Move over the following '\''
      PARSE_CHECK('\'', input[start]);
      ++start;

      // Move over whitespace (not newlines)
      while (input[start] == ' ' ||
             input[start] == '\t' ||
             input[start] == '\v')
      {
        ++start;
      }

      // We now have the start -- put end at the following newline
      end = input.find('\n', start);
      if (end == std::string::npos || end == n - 1)
      {
        LOG("Adding: '" << substring(input, start, n) << "' as final string");
        content_.append(substring(input, start, n));
        break;
      }
      else
      {
        LOG("Adding: '" << substring(input, start, end) << "'");
        content_.append(substring(input, start, end + 1));
      }

      // update start, end state
      start = end + 1;
      end = start;
    }

    n_ = content_.length();
    LOG("Content after strip:\n");
    LOG(content_);
  }

  int findNextTagOrEnd(int index)
  {
    while (index < n_ - 2)
    {
      if (content_[index] == '\n')
      {
        if (content_[index + 1] == '@')
        {
          break;
        }
      }
      ++index;
    }

    return index + 1;
  }

  int findNextTagOrEnd(int index, int* row)
  {
    while (index < n_ - 2)
    {
      if (content_[index] == '\n')
      {
        ++*row;
        if (content_[index + 1] == '@')
        {
          break;
        }
      }
      ++index;
    }

    return index + 1;
  }


  // Get the parameter value
  // index represents the location just past e.g. '@foo'
  std::string getRoxygenTagValue(int start)
  {
    int end = findNextTagOrEnd(start);

    // Move backwards over newlines and whitespace to trim
    while (end > start &&
           (content_[end - 1] == ' ' ||
            content_[end - 1] == '\t' ||
            content_[end - 1] == '\n'))
      --end;

    // If we moved back to the start, just return empty string
    if (end == start)
    {
      return std::string();
    }

    // Similarily, for the start
    fwdOverWhitespace(content_, &start);

    if (end <= start)
    {
      return std::string();
    }

    return substring(content_, start, end);
  }

  int findFirstTagIndex()
  {
    // If the first character is a '@', we already have Roxygen params
    if (content_[0] == '@')
      return 0;

    // Find the first '@' starting a new line
    int index = 0;
    do {
      if (content_[index] == '\n')
      {
        ++row_;
        if (content_[index + 1] == '@')
        {
          break;
        }
      }
    } while (++index < n_ - 1);

    return index + 1;
  }

  std::string content_;
  std::string filePath_;
  std::string roxyDelim_;
  int row_;
  int n_;

};


class FileParser
{

public:

  static List parse(std::string const& filePath)
  {

    // The file connection
    std::ifstream conn(filePath.c_str());

    // Memory for the blocks
    std::vector<List> allBlocks;

    // Memory for the current block we're attempting to parse
    std::string thisBlockString;
    thisBlockString.reserve(1024);

    // The line we just read in
    std::string line;

    // The number of lines read in (so we can track the row)
    int blockStartRow = 0;
    int currentRow = 0;

    while (std::getline(conn, line))
    {
      if (isRoxygenDelimited(line))
      {
        thisBlockString.append(line).append("\n");
      }
      else
      {
        if (thisBlockString.length() > 0)
        {
          allBlocks.push_back(
            BlockParser(
              thisBlockString,
              filePath,
              blockStartRow
            ).parse()
          );
        }

        thisBlockString.clear();
        thisBlockString.reserve(1024);
        blockStartRow = currentRow + 1;
      }
      ++currentRow;
    }

    List output = wrap(allBlocks);
    return output;

  }

};

} // namespace parser

// [[Rcpp::export]]
List preparse_block(std::string x)
{
  return roxygen::BlockParser(x).parse();
}

// [[Rcpp::export]]
List preparse_file(std::string filePath)
{
  return roxygen::FileParser::parse(filePath);
}

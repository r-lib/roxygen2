#ifndef ROXYGEN_H
#define ROXYGEN_H

#include <fstream>
#include <Rcpp.h>

// Debugging functions -- set DEBUG_LEVEL to 0 to turn off
#define DEBUG_LEVEL 0

#if defined(DEBUG_LEVEL) && DEBUG_LEVEL > 0

// Utility macro functions
#define ROXYGEN_STOP(EXPRESSION) {                                             \
  std::stringstream SS__;                                                      \
  SS__ << EXPRESSION;                                                          \
  ::Rcpp::stop(SS__.str());                                                    \
}

#define LOG(EXPRESSION) std::cout << EXPRESSION << std::endl

#define PARSE_ERROR(EXPECTED, GOT) {                                           \
  std::stringstream STRINGSTREAM__;                                            \
  STRINGSTREAM__ << "Parse error: expected '"                                  \
                 << EXPECTED                                                   \
                 << "', got '"                                                 \
                 << GOT                                                        \
                 << "'"                                                        \
                 << " (line " << __LINE__ << ")";                              \
  stop(STRINGSTREAM__.str());                                                  \
}

#define PARSE_CHECK(EXPECTED, GOT) {                                           \
  if (EXPECTED != GOT) {                                                       \
    PARSE_ERROR(EXPECTED, GOT)                                                 \
  }                                                                            \
}

#else

#define LOG(EXPRESSION)
#define PARSE_ERROR(EXPECTED, GOT)
#define PARSE_CHECK(EXPECTED, GOT)

#endif

// Utility functions

namespace roxygen {

bool isRoxygenDelimited(std::string const& line)
{
  if (line[0] != '#') return false;
  int index = 0;
  while (line[index] == '#') ++index;
  return line[index] == '\'';
}

template <typename T>
inline bool contains(std::vector<T> const& self, T const& other)
{
  return std::find(self.begin(), self.end(), other) != self.end();
}

std::vector<char> initWhitespaceChars()
{
  std::vector<char> kWhitespaceChars;
  kWhitespaceChars.push_back('\v');
  kWhitespaceChars.push_back('\n');
  kWhitespaceChars.push_back('\r');
  kWhitespaceChars.push_back('\t');
  kWhitespaceChars.push_back(' ');
  return kWhitespaceChars;
}

static const std::vector<char> kWhitespaceChars = initWhitespaceChars();

inline bool isAlpha(char x)
{
  return (x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z');
}

inline bool isNumeric(char x)
{
  return x >= '0' && x <= '9';
}

inline bool isAlphaNumeric(char x)
{
  return isAlpha(x) || isNumeric(x);
}

inline bool isWhitespace(char x)
{
  return contains(kWhitespaceChars, x);
}

inline std::string substring(std::string const& self, int from, int to)
{
#if DEBUG_LEVEL > 0
  if (from > to)
  {
    ROXYGEN_STOP("substring error: 'from' > 'to' (" << from << ", " << to << ")");
  }
#endif
  return self.substr(from, to - from);
}

inline void fwdOverWhitespace(std::string const& content, int* index)
{
  while (contains(kWhitespaceChars, content[*index]))
  {
    ++*index;
  }
}

inline void bwdOverWhitespace(std::string const& content, int* index)
{
  while (contains(kWhitespaceChars, content[*index - 1]))
  {
    --*index;
  }
}

class ListBuilder {

public:

  ListBuilder() {};
  ~ListBuilder() {};

  inline ListBuilder& add(std::string const& name, SEXP x)
  {
    names_.push_back(name);
    elements_.push_back(PROTECT(x));
    return *this;
  }

  inline operator Rcpp::List() const
  {
    int n = elements_.size();
    Rcpp::List result(n);
    for (int i = 0; i < n; ++i) {
      result[i] = elements_[i];
    }
    result.attr("names") = Rcpp::wrap(names_);
    UNPROTECT(n);
    return result;
  }

  inline operator Rcpp::DataFrame() const
  {
    int n = elements_.size();
    Rcpp::List result = static_cast<Rcpp::List>(*this);
    result.attr("class") = "data.frame";
    result.attr("row.names") =
        Rcpp::IntegerVector::create(NA_INTEGER, XLENGTH(elements_[0]));
    UNPROTECT(n);
    return result;
  }

private:

  std::vector<std::string> names_;
  std::vector<SEXP> elements_;

  // Prevent copying
  ListBuilder(ListBuilder const&);
  ListBuilder& operator=(ListBuilder const&);

};

} // end namespace roxygen

#endif // ROXYGEN_H

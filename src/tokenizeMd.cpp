#include <cpp11/list.hpp>
#include <cpp11/strings.hpp>
#include <cpp11/integers.hpp>
#include <set>
#include <string>
#include <vector>

using namespace cpp11::literals;

// Tokenizer for the combined markdown/Rd grammar used in roxygen comments.
//
// The only overlap between the two languages is the backslash: everything
// backslash-initiated belongs to Rd, everything else belongs to markdown.
// So we scan once, left to right, and replace every backslash-initiated
// construct with an inert placeholder "<index>" (private-use-area
// sentinels around a 1-based index into the returned token vector).
// Placeholders are plain text to commonmark in every context (text, code
// spans, fenced blocks, link labels, tables), so the sanitized text can be
// parsed as markdown without any escaping. The grammar:
//
//   bs_token := "\" NAME braces*   NAME in `verbatim` -> tag + all brace args
//             | "\" NAME           other NAME (args remain markdown)
//             | "\" PUNCT          two-character escape, e.g. \% \\ \{
//             | "\"                lone backslash (also before a backtick,
//                                  so code-span delimiters are never eaten)
//
// Two exceptions, because \[ and \] are markdown bracket escapes: "\[" and
// "\]" are passed through for commonmark to handle, and "\\[" / "\\]" are
// single three-character tokens (the trailing bracket must not become an
// active markdown bracket).
//
// All scanning is over UTF-8 bytes; multibyte characters can never match
// ASCII specials, so they pass through untouched.

// Scan a brace group starting at s[start] == '{'. Returns true and sets
// *end to the byte index of the matching '}', or returns false if the
// group is incomplete. Follows Rd rules: \ escapes the next character,
// % starts a comment that runs to the end of the line.
static bool scan_brace_group(const std::string& s, int start, int* end) {
  int n = s.size();
  int braces = 0;
  bool escape = false;
  bool comment = false;

  for (int i = start; i < n; i++) {
    char c = s[i];
    if (escape) {
      escape = false;
    } else if (comment) {
      if (c == '\n')
        comment = false;
    } else {
      switch (c) {
      case '\\':
        escape = true;
        break;
      case '%':
        comment = true;
        break;
      case '{':
        braces++;
        break;
      case '}':
        braces--;
        if (braces == 0) {
          *end = i;
          return true;
        }
        break;
      }
    }
  }

  return false;
}

static bool is_ascii_alpha(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static bool is_ascii_alnum(char c) {
  return is_ascii_alpha(c) || (c >= '0' && c <= '9');
}

static bool is_ascii_punct(char c) {
  return (c >= '!' && c <= '/') || (c >= ':' && c <= '@') ||
         (c >= '[' && c <= '`') || (c >= '{' && c <= '~');
}

[[cpp11::register]]
cpp11::writable::list tokenizeMd(std::string text,
                                 cpp11::strings verbatim) {
  std::set<std::string> vtags;
  for (R_xlen_t v = 0; v < verbatim.size(); v++) {
    vtags.insert(std::string(verbatim[v]));
  }

  static const std::string OPEN = "\xEE\x80\x80";  // U+E000
  static const std::string CLOSE = "\xEE\x80\x81"; // U+E001

  int n = text.size();
  std::string out;
  out.reserve(n);
  std::vector<std::string> tokens;
  std::vector<std::string> types;
  int n_stripped = 0;

  for (int i = 0; i < n; i++) {
    char c = text[i];

    // Strip pre-existing sentinel characters so placeholders can't be forged
    if ((unsigned char)c == 0xEE && i + 2 < n &&
        (unsigned char)text[i + 1] == 0x80 &&
        ((unsigned char)text[i + 2] == 0x80 ||
         (unsigned char)text[i + 2] == 0x81)) {
      n_stripped++;
      i += 2;
      continue;
    }

    if (c != '\\') {
      out += c;
      continue;
    }

    int end = i; // inclusive last byte of this token
    std::string type = "backslash";
    if (i + 1 < n && is_ascii_alpha(text[i + 1])) {
      int j = i + 2;
      while (j < n && is_ascii_alnum(text[j]))
        j++;
      end = j - 1;

      std::string name = text.substr(i + 1, j - i - 1);
      if (vtags.count(name)) {
        type = "verbatim";
        // Consume all complete brace groups; an incomplete group is left
        // to markdown, like the brace groups of non-verbatim tags
        while (end + 1 < n && text[end + 1] == '{') {
          int gend;
          if (!scan_brace_group(text, end + 1, &gend))
            break;
          end = gend;
        }
      } else {
        type = "tag";
      }
    } else if (i + 1 < n && (text[i + 1] == '[' || text[i + 1] == ']')) {
      // Markdown bracket escape: the only backslash construct that
      // commonmark must see, so that \[ suppresses link parsing
      out += c;
      out += text[i + 1];
      i++;
      continue;
    } else if (i + 1 < n && is_ascii_punct(text[i + 1]) &&
               text[i + 1] != '`') {
      type = "escape";
      end = i + 1;
      // \\[ renders as a backslash + literal bracket, so the bracket
      // must be hidden from the markdown parser along with the backslashes
      if (text[i + 1] == '\\' && i + 2 < n &&
          (text[i + 2] == '[' || text[i + 2] == ']')) {
        end = i + 2;
      }
    }

    tokens.push_back(text.substr(i, end - i + 1));
    types.push_back(type);
    out += OPEN;
    out += std::to_string(tokens.size());
    out += CLOSE;
    i = end;
  }

  cpp11::writable::strings rtokens(tokens.size());
  cpp11::writable::strings rtypes(types.size());
  for (size_t t = 0; t < tokens.size(); t++) {
    rtokens[t] = tokens[t];
    rtypes[t] = types[t];
  }

  return cpp11::writable::list(
      {"text"_nm = cpp11::writable::strings({out}),
       "tokens"_nm = rtokens,
       "types"_nm = rtypes,
       "stripped"_nm = cpp11::writable::integers({n_stripped})});
}

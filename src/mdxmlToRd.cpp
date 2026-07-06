#include <cpp11/function.hpp>
#include <cpp11/list.hpp>
#include <cpp11/strings.hpp>
#include <cstdlib>
#include <string>
#include <vector>

using namespace cpp11::literals;

// Render the commonmark XML representation of a roxygen comment as Rd.
//
// This is a C++ port of the old R tree walk over xml2 nodes; it exists
// purely for speed and must produce byte-identical output. The input is
// the string returned by commonmark::markdown_xml(), which uses a small,
// regular subset of XML: elements, double-quoted attributes, and
// character data escaped with the four entities &lt; &gt; &amp; &quot;
// (other characters are raw UTF-8). All text content lives inside leaf
// elements marked xml:space="preserve"; whitespace between structural
// tags is pretty-printing and is dropped, like xml2's NOBLANKS.
//
// Backslash constructs were replaced by "<index>" placeholders (see
// tokenizeMd.cpp) before the markdown parse, so the walk restores each
// token as it lands in its final Rd context:
//
// * text: regular Rd text. Verbatim tags come back as live Rd; the
//   escaped bracket escapes "\\[" and "\\]" drop one backslash (matching
//   what the markdown escape "\[" does to a bare bracket); everything
//   else comes back as typed.
// * verb: inside \verb{}, \code{} or \preformatted{}. Everything
//   renders literally, so token text is Rd-escaped -- except verbatim Rd
//   tags, which are inserted as typed: the Rd parser keeps unknown
//   macros in verbatim contexts as literal text.
// * raw: unprocessed output, e.g. the body of a generated \Sexpr{}.
//
// Link resolution, R code detection, and warnings need package state and
// so stay in R; they are supplied as callbacks.

static const std::string OPEN = "\xEE\x80\x80";  // U+E000
static const std::string CLOSE = "\xEE\x80\x81"; // U+E001

// XML parsing ----------------------------------------------------------

struct MdNode {
  std::string name;
  std::string text; // direct character data, entities decoded
  std::vector<std::pair<std::string, std::string>> attrs;
  std::vector<int> children; // element children only

  const std::string* attr(const char* name) const {
    for (size_t i = 0; i < attrs.size(); i++) {
      if (attrs[i].first == name) {
        return &attrs[i].second;
      }
    }
    return NULL;
  }
};

// Append the UTF-8 encoding of a code point
static void append_utf8(std::string& out, unsigned long cp) {
  if (cp <= 0x7F) {
    out += (char)cp;
  } else if (cp <= 0x7FF) {
    out += (char)(0xC0 | (cp >> 6));
    out += (char)(0x80 | (cp & 0x3F));
  } else if (cp <= 0xFFFF) {
    out += (char)(0xE0 | (cp >> 12));
    out += (char)(0x80 | ((cp >> 6) & 0x3F));
    out += (char)(0x80 | (cp & 0x3F));
  } else {
    out += (char)(0xF0 | (cp >> 18));
    out += (char)(0x80 | ((cp >> 12) & 0x3F));
    out += (char)(0x80 | ((cp >> 6) & 0x3F));
    out += (char)(0x80 | (cp & 0x3F));
  }
}

// Decode s[start, end) into out, replacing XML entities
static void decode_chardata(const std::string& s, size_t start, size_t end,
                            std::string& out) {
  for (size_t i = start; i < end; i++) {
    if (s[i] != '&') {
      out += s[i];
      continue;
    }
    size_t semi = s.find(';', i + 1);
    if (semi == std::string::npos || semi > i + 10) {
      out += '&';
      continue;
    }
    std::string ent = s.substr(i + 1, semi - i - 1);
    if (ent == "lt") {
      out += '<';
    } else if (ent == "gt") {
      out += '>';
    } else if (ent == "amp") {
      out += '&';
    } else if (ent == "quot") {
      out += '"';
    } else if (ent == "apos") {
      out += '\'';
    } else if (ent.size() > 1 && ent[0] == '#') {
      unsigned long cp = (ent[1] == 'x' || ent[1] == 'X')
                             ? strtoul(ent.c_str() + 2, NULL, 16)
                             : strtoul(ent.c_str() + 1, NULL, 10);
      append_utf8(out, cp);
    } else {
      out += '&';
      continue;
    }
    i = semi;
  }
}

static bool is_blank(const std::string& s) {
  for (size_t i = 0; i < s.size(); i++) {
    char c = s[i];
    if (c != ' ' && c != '\t' && c != '\r' && c != '\n') {
      return false;
    }
  }
  return true;
}

static bool is_name_char(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
         (c >= '0' && c <= '9') || c == '_' || c == '-' || c == ':';
}

// Parse commonmark's XML output into nodes; returns the root index or -1
static int parse_xml(const std::string& s, std::vector<MdNode>& nodes) {
  size_t i = 0, n = s.size();
  int root = -1;
  std::vector<int> stack;

  while (i < n) {
    if (s[i] != '<') {
      size_t start = i;
      while (i < n && s[i] != '<') {
        i++;
      }
      if (!stack.empty()) {
        std::string text;
        decode_chardata(s, start, i, text);
        MdNode& top = nodes[stack.back()];
        if (!is_blank(text) || top.attr("xml:space") != NULL) {
          top.text += text;
        }
      }
      continue;
    }

    if (i + 1 < n && (s[i + 1] == '?' || s[i + 1] == '!')) {
      // <?xml ...?> or <!DOCTYPE ...>
      while (i < n && s[i] != '>') {
        i++;
      }
      i++;
    } else if (i + 1 < n && s[i + 1] == '/') {
      while (i < n && s[i] != '>') {
        i++;
      }
      i++;
      if (!stack.empty()) {
        stack.pop_back();
      }
    } else {
      i++;
      MdNode node;
      while (i < n && is_name_char(s[i])) {
        node.name += s[i++];
      }
      bool self_closing = false;
      while (i < n) {
        while (i < n && (s[i] == ' ' || s[i] == '\t' || s[i] == '\r' ||
                         s[i] == '\n')) {
          i++;
        }
        if (i < n && s[i] == '/') {
          self_closing = true;
          i++;
          continue;
        }
        if (i >= n || s[i] == '>') {
          i++;
          break;
        }
        std::string name;
        while (i < n && is_name_char(s[i]) && s[i] != '=') {
          name += s[i++];
        }
        std::string value;
        if (i < n && s[i] == '=') {
          i++;
          char quote = s[i];
          size_t vstart = ++i;
          while (i < n && s[i] != quote) {
            i++;
          }
          decode_chardata(s, vstart, i, value);
          i++;
        }
        node.attrs.push_back(std::make_pair(name, value));
      }

      int id = nodes.size();
      nodes.push_back(node);
      if (!stack.empty()) {
        nodes[stack.back()].children.push_back(id);
      } else if (root < 0) {
        root = id;
      }
      if (!self_closing) {
        stack.push_back(id);
      }
    }
  }

  return root;
}

// Rd rendering ---------------------------------------------------------

// How the characters around placeholders are escaped, and how restored
// tokens are rendered, in each Rd context (see tokenizeMd.cpp)
enum class Esc { None, Comment, Verb, Braces };
enum class Mode { Text, Verb, Raw };

class Walker {
public:
  Walker(const std::vector<MdNode>& nodes, cpp11::strings tokens,
         cpp11::strings types, bool has_sections, std::string section_tag,
         bool restrict_images, cpp11::function resolve_link,
         cpp11::function is_r_code, cpp11::function warn)
      : nodes_(nodes),
        has_sections_(has_sections),
        section_tag_(section_tag),
        restrict_images_(restrict_images),
        resolve_link_(resolve_link),
        is_r_code_(is_r_code),
        warn_(warn) {
    for (R_xlen_t t = 0; t < tokens.size(); t++) {
      tokens_.push_back(std::string(tokens[t]));
      verbatim_.push_back(std::string(types[t]) == "verbatim");
    }
  }

  std::string walk(int root) {
    std::string out = children_to_rd(root);
    out += close_sections(1);
    // Tokens can also enter the output through the resolved links, e.g.
    // the destination of [\code{x}] contains a placeholder, so finish
    // with a text-mode restore over the assembled Rd
    out = restore_text(out);
    // trimws()
    size_t from = out.find_first_not_of(" \t\r\n");
    size_t to = out.find_last_not_of(" \t\r\n");
    if (from == std::string::npos) {
      return "";
    }
    return out.substr(from, to - from + 1);
  }

  std::string restore_text(const std::string& s) {
    return emit(s, Esc::None, Mode::Text);
  }

  std::vector<std::string> titles;

private:
  const std::vector<MdNode>& nodes_;
  std::vector<std::string> tokens_;
  std::vector<bool> verbatim_;
  bool has_sections_;
  std::string section_tag_;
  bool restrict_images_;
  cpp11::function resolve_link_;
  cpp11::function is_r_code_;
  cpp11::function warn_;

  bool inlink_ = false;
  bool in_link_code_ = false;
  std::vector<int> sections_;

  std::string token_rd(int idx, Mode mode) {
    if (idx < 1 || idx > (int)tokens_.size()) {
      // Can't happen: placeholders are generated alongside the tokens
      return OPEN + std::to_string(idx) + CLOSE;
    }
    const std::string& src = tokens_[idx - 1];
    switch (mode) {
    case Mode::Raw:
      return src;
    case Mode::Text:
      // \\[ renders as \[, matching the markdown escape \[
      if (src == "\\\\[" || src == "\\\\]") {
        return src.substr(1);
      }
      return src;
    case Mode::Verb:
    default:
      if (verbatim_[idx - 1]) {
        return src;
      }
      std::string out;
      for (size_t i = 0; i < src.size(); i++) {
        switch (src[i]) {
        case '\\':
          out += "\\\\";
          break;
        case '%':
          out += "\\%";
          break;
        case '{':
          out += "\\{";
          break;
        case '}':
          out += "\\}";
          break;
        default:
          out += src[i];
        }
      }
      return out;
    }
  }

  // Escape s for an Rd context and restore the tokens that land in it
  std::string emit(const std::string& s, Esc esc, Mode mode) {
    std::string out;
    out.reserve(s.size());
    size_t n = s.size();

    for (size_t i = 0; i < n; i++) {
      char c = s[i];
      if ((unsigned char)c == 0xEE && i + 2 < n &&
          (unsigned char)s[i + 1] == 0x80 && (unsigned char)s[i + 2] == 0x80) {
        size_t j = i + 3;
        int idx = 0;
        while (j < n && s[j] >= '0' && s[j] <= '9') {
          idx = idx * 10 + (s[j] - '0');
          j++;
        }
        if (j > i + 3 && j + 2 < n && (unsigned char)s[j] == 0xEE &&
            (unsigned char)s[j + 1] == 0x80 &&
            (unsigned char)s[j + 2] == 0x81) {
          out += token_rd(idx, mode);
          i = j + 2;
          continue;
        }
      }
      switch (c) {
      case '%':
        out += (esc == Esc::Comment || esc == Esc::Verb) ? "\\%" : "%";
        break;
      case '{':
        out += (esc == Esc::Verb || esc == Esc::Braces) ? "\\{" : "{";
        break;
      case '}':
        out += (esc == Esc::Verb || esc == Esc::Braces) ? "\\}" : "}";
        break;
      default:
        out += c;
      }
    }
    return out;
  }

  // All descendant character data, like xml2::xml_text()
  void text_of(int id, std::string& out) {
    const MdNode& nd = nodes_[id];
    out += nd.text;
    for (size_t i = 0; i < nd.children.size(); i++) {
      text_of(nd.children[i], out);
    }
  }

  std::string text_of(int id) {
    std::string out;
    text_of(id, out);
    return out;
  }

  std::string children_to_rd(int id) {
    const MdNode& nd = nodes_[id];
    std::string out;
    for (size_t i = 0; i < nd.children.size(); i++) {
      out += node_to_rd(nd.children[i]);
    }
    return out;
  }

  std::string children_to_rd_inlink(int id) {
    bool old = inlink_;
    inlink_ = true;
    std::string out = children_to_rd(id);
    inlink_ = old;
    return out;
  }

  std::string node_to_rd(int id) {
    const MdNode& nd = nodes_[id];
    const std::string& name = nd.name;

    if (name == "html" || name == "document" || name == "unknown") {
      return children_to_rd(id);
    } else if (name == "paragraph") {
      return "\n\n" + children_to_rd(id);
    } else if (name == "text") {
      return in_link_code_ ? emit(nd.text, Esc::Verb, Mode::Verb)
                           : emit(nd.text, Esc::Comment, Mode::Text);
    } else if (name == "emph") {
      return "\\emph{" + children_to_rd(id) + "}";
    } else if (name == "strong") {
      return "\\strong{" + children_to_rd(id) + "}";
    } else if (name == "softbreak" || name == "linebreak") {
      return inlink_ ? " " : "\n";
    } else if (name == "code") {
      return code_to_rd(nd);
    } else if (name == "code_block") {
      return code_block_to_rd(nd);
    } else if (name == "table") {
      return table_to_rd(id);
    } else if (name == "list") {
      return list_to_rd(id);
    } else if (name == "item") {
      return item_to_rd(id);
    } else if (name == "link") {
      return link_to_rd(id);
    } else if (name == "image") {
      return image_to_rd(nd);
    } else if (name == "heading") {
      return heading_to_rd(id);
    } else if (name == "html_block") {
      return "\\if{html}{\\out{\n" + emit(nd.text, Esc::Braces, Mode::Text) +
             "}}\n";
    } else if (name == "html_inline") {
      return "\\if{html}{\\out{" + emit(nd.text, Esc::Braces, Mode::Text) +
             "}}";
    } else if (name == "block_quote") {
      warn_("unsupported", "block quotes");
      return emit(text_of(id), Esc::Comment, Mode::Text);
    } else if (name == "thematic_break") {
      warn_("unsupported", "horizontal rules");
      return emit(text_of(id), Esc::Comment, Mode::Text);
    } else {
      warn_("unknown", name);
      return emit(text_of(id), Esc::Comment, Mode::Text);
    }
  }

  std::string code_to_rd(const MdNode& nd) {
    // Decide what the code is based on its original source
    std::string raw_code = emit(nd.text, Esc::None, Mode::Raw);

    if (raw_code.compare(0, 3, "Rd ") == 0) {
      return "\\Sexpr[stage=render,results=rd]{" + raw_code.substr(3) + "}";
    } else if (cpp11::as_cpp<bool>(is_r_code_(raw_code))) {
      // See escaping details at
      // https://cran.rstudio.com/doc/manuals/r-devel/R-exts.html#Insertions
      return "\\code{" + emit(nd.text, Esc::Comment, Mode::Verb) + "}";
    } else {
      return "\\verb{" + emit(nd.text, Esc::Verb, Mode::Verb) + "}";
    }
  }

  std::string code_block_to_rd(const MdNode& nd) {
    const std::string* info = nd.attr("info");
    std::string out = "\n\n\\if{html}{\\out{<div class=\"sourceCode";
    if (info != NULL && !info->empty()) {
      out += " " + *info;
    }
    out += "\">}}\\preformatted{";
    out += emit(nd.text, Esc::Verb, Mode::Verb);
    out += "}\\if{html}{\\out{</div>}}";
    return out;
  }

  std::string table_to_rd(int id) {
    const MdNode& nd = nodes_[id];
    if (nd.children.empty()) {
      return "";
    }

    std::string align;
    const MdNode& head = nodes_[nd.children[0]];
    for (size_t i = 0; i < head.children.size(); i++) {
      const std::string* a = nodes_[head.children[i]].attr("align");
      align += (a != NULL && !a->empty()) ? (*a)[0] : 'l';
    }

    std::string body;
    for (size_t i = 0; i < nd.children.size(); i++) {
      const MdNode& row = nodes_[nd.children[i]];
      if (row.name != "table_row" && row.name != "table_header") {
        continue;
      }
      std::string cells;
      for (size_t j = 0; j < row.children.size(); j++) {
        if (nodes_[row.children[j]].name != "table_cell") {
          continue;
        }
        if (!cells.empty()) {
          cells += " \\tab ";
        }
        cells += children_to_rd(row.children[j]);
      }
      body += "   " + cells + " \\cr\n";
    }

    return "\\tabular{" + align + "}{\n" + body + "}\n";
  }

  std::string list_to_rd(int id) {
    const MdNode& nd = nodes_[id];
    const std::string* type = nd.attr("type");
    if (type != NULL && *type == "ordered") {
      return "\n\\enumerate{" + children_to_rd(id) + "\n}";
    } else {
      return "\n\\itemize{" + children_to_rd(id) + "\n}";
    }
  }

  std::string item_to_rd(int id) {
    // Remove the first paragraph tag, to avoid an empty line at the
    // beginning of the first item
    const MdNode& nd = nodes_[id];
    std::string cnts;
    if (!nd.children.empty() &&
        nodes_[nd.children[0]].name == "paragraph") {
      cnts = children_to_rd(nd.children[0]);
      for (size_t i = 1; i < nd.children.size(); i++) {
        cnts += node_to_rd(nd.children[i]);
      }
    } else {
      cnts = children_to_rd(id);
    }
    return "\n\\item " + cnts;
  }

  std::string link_to_rd(int id) {
    const MdNode& nd = nodes_[id];
    const std::string* d = nd.attr("destination");
    std::string dest = d != NULL ? *d : "";

    if (dest.compare(0, 2, "R:") == 0) {
      // A [topic] or [text][topic] link; resolution needs package state,
      // so it happens in R
      bool is_code =
          nd.children.size() == 1 && nodes_[nd.children[0]].name == "code";
      std::string contents_text;
      bool has_nontext = false;
      std::string rendered;
      if (is_code) {
        // A [`code`][topic] link: the \code becomes the outermost layer,
        // so the link text is the rendered content of the code span
        contents_text = nodes_[nd.children[0]].text;
        rendered = emit(contents_text, Esc::Verb, Mode::Verb);
      } else {
        contents_text = text_of(id);
        for (size_t i = 0; i < nd.children.size(); i++) {
          if (nodes_[nd.children[i]].name != "text") {
            has_nontext = true;
          }
        }
        rendered = children_to_rd_inlink(id);
      }
      cpp11::strings out(
          resolve_link_(dest, contents_text, has_nontext, is_code, rendered));
      return std::string(out[0]);
    }

    std::string txt = text_of(id);
    if (dest.empty() || dest == txt) {
      return "\\url{" + emit(txt, Esc::Comment, Mode::Text) + "}";
    } else {
      return "\\href{" + emit(dest, Esc::Comment, Mode::Text) + "}{" +
             children_to_rd_inlink(id) + "}";
    }
  }

  std::string image_to_rd(const MdNode& nd) {
    const std::string* d = nd.attr("destination");
    const std::string* t = nd.attr("title");
    std::string dest = d != NULL ? *d : "";
    std::string title = t != NULL ? *t : "";

    // pdf can't display svg and html can't display pdf
    std::string fmt = "all";
    if (restrict_images_) {
      std::string lower = dest;
      for (size_t i = 0; i < lower.size(); i++) {
        if (lower[i] >= 'A' && lower[i] <= 'Z') {
          lower[i] += 'a' - 'A';
        }
      }
      bool html = has_extension(lower, "jpg") || has_extension(lower, "jpeg") ||
                  has_extension(lower, "gif") || has_extension(lower, "png");
      bool pdf = html;
      html = html || has_extension(lower, "svg");
      pdf = pdf || has_extension(lower, "pdf");
      fmt = (html && pdf) ? "all" : html ? "html" : pdf ? "pdf" : "all";
    }

    std::string out;
    if (fmt == "html") {
      out += "\\if{html}{";
    }
    if (fmt == "pdf") {
      out += "\\if{pdf}{";
    }
    out += "\\figure{" + emit(dest, Esc::None, Mode::Text) + "}";
    if (!title.empty()) {
      out += "{" + emit(title, Esc::None, Mode::Text) + "}";
    }
    if (fmt == "html" || fmt == "pdf") {
      out += "}";
    }
    return out;
  }

  static bool has_extension(const std::string& path, const std::string& ext) {
    if (path.size() < ext.size() + 1) {
      return false;
    }
    return path[path.size() - ext.size() - 1] == '.' &&
           path.compare(path.size() - ext.size(), ext.size(), ext) == 0;
  }

  std::string heading_to_rd(int id) {
    const MdNode& nd = nodes_[id];
    const std::string* l = nd.attr("level");
    int level = l != NULL ? atoi(l->c_str()) : 0;

    if (!has_sections_ && level == 1) {
      warn_("heading", "");
      return emit(text_of(id), Esc::Comment, Mode::Text);
    }

    std::string txt = children_to_rd(id);
    if (level == 1) {
      titles.push_back(txt);
    }
    std::string head = close_sections(level) + "\n";
    if (level == 1) {
      head += section_tag_;
    } else {
      head += "\\subsection{" + txt + "}{";
    }
    sections_.push_back(level);
    return head;
  }

  std::string close_sections(int upto) {
    if (upto < 2) {
      upto = 2;
    }
    std::string out;
    while (!sections_.empty() && sections_.back() >= upto) {
      out += "\n}\n";
      sections_.pop_back();
    }
    return out;
  }
};

[[cpp11::register]]
cpp11::writable::list mdxmlToRd(std::string xml, cpp11::strings tokens,
                                cpp11::strings types, bool has_sections,
                                std::string section_tag, bool restrict_images,
                                cpp11::function resolve_link,
                                cpp11::function is_r_code,
                                cpp11::function warn) {
  std::vector<MdNode> nodes;
  int root = parse_xml(xml, nodes);

  std::string rd;
  Walker walker(nodes, tokens, types, has_sections, section_tag,
                restrict_images, resolve_link, is_r_code, warn);
  if (root >= 0) {
    rd = walker.walk(root);
  }

  cpp11::writable::strings rtitles(walker.titles.size());
  for (size_t t = 0; t < walker.titles.size(); t++) {
    rtitles[t] = walker.restore_text(walker.titles[t]);
  }

  return cpp11::writable::list(
      {"rd"_nm = cpp11::writable::strings({rd}),
       "titles"_nm = rtitles});
}

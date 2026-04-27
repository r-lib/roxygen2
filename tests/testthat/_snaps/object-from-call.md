# recommends use of _PACKAGE

    Code
      out <- parse_text(block)[[1]]
    Message
      x <text>:3: `@docType "package"` is deprecated.
      i Please document "_PACKAGE" instead.

# S7 method with special classes any and missing

    Code
      s7_class_name(S7::class_any, block)
    Output
      [1] "any"

---

    Code
      s7_class_name(S7::class_missing, block)
    Output
      [1] "missing"

# S7 method with unknown class type warns

    Code
      s7_class_name(42L, block)
    Message
      x test.R:1: Unknown S7 class type.
    Output
      [1] "42L"


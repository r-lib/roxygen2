# R6 edge cases, class without (documented) fields

    Code
      topic_add_r6_methods(rd, block, environment())
    Condition
      Warning:
      [<text>:7] Undocumented R6 field: undocumented_field

# warning if no method comes after the docs

    Code
      topic_add_r6_methods(rd, block, environment())
    Condition
      Warning:
      [<text>:10] @description Cannot find matching R6 method

# integration test

    Code
      res <- roclet_process(roc, blocks = blocks, env = env, base_path = test_path())
    Condition
      Warning:
      [roxygen-block-3.R:13] Must use one @param for each argument
      x $meth3(duplicate) is documented multiple times
      Warning:
      [roxygen-block-3.R:13] Must use one @param for each argument
      x $meth3(missing) is not documented
      Warning:
      [roxygen-block-3.R:13] Must use one @return per R6 method
      Warning:
      [roxygen-block-3.R:92] Undocumented R6 method: undocumented_method
      Warning:
      [roxygen-block-3.R:92] Undocumented R6 fields: field2 and undocumented_field
      Warning:
      [roxygen-block-3.R:92] R6 field documented multiple times: duplicatefield
      Warning:
      [roxygen-block-3.R:92] Unknown R6 field: nosuchfield
      Warning:
      [roxygen-block-3.R:92] Undocumented R6 active binding: undocumented_binding
      Warning:
      [roxygen-block-3.R:92] R6 active binding documented multiple times: duplicate_binding


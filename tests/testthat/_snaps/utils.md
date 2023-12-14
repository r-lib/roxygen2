# write_if_different produces informative messages

    Code
      write_if_different(path, "a <- 2")
    Message
      Writing 'test.R'
    Output
      [1] TRUE

---

    Code
      write_if_different(path, "a <- 2")
    Message
      x Skipping '+.R'
      i Invalid file name
    Output
      [1] FALSE


# look up string

    Code
      find_load_strategy("blahblahb")
    Condition
      Error:
      ! Unknown value of `load` option: "blahblahb".

# informative errors for bad inputs

    Code
      find_load_strategy(1)
    Condition
      Error:
      ! `load_code` must be a single string, not the number 1.
    Code
      find_load_strategy(NULL, list())
    Condition
      Error:
      ! roxygen2 `load` option must be a single string, not an empty list.


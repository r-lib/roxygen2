# useful error if no DESCRIPTION

    Code
      roxygen_setup(path)
    Condition
      Error in `roxygen_setup()`:
      ! `package.dir` ('<path>') does not contain a DESCRIPTION

# informs about initial setup

    Code
      roxygen_setup(path, cur_version = "8.0.0")
    Message
      First time using roxygen2. Upgrading automatically...
      i Setting RoxygenNote to "8.0.0"
    Output
      [1] TRUE

# warns about non UTF-8 encoding

    Code
      roxygen_setup(path, cur_version = "8.0.0")
    Message
      x roxygen2 requires "Encoding: UTF-8"
      i Current encoding is "latin1"
    Output
      [1] FALSE

# warns if roxygen version is too new

    Code
      roxygen_setup(path, cur_version = "8.0.0")
    Message
      x Installed roxygen2 is older than the version used with this package
      i You have "8.0.0" but you need "10.0.0"
    Output
      [1] FALSE

# informs about major changes in 7.0.0

    Code
      roxygen_setup(path, cur_version = "8.0.0")
    Message
      --------------------------------------------------------------------------------
      Changes in roxygen2 7.0.0:
      * `%` is now escaped automatically in Markdown mode.
      Please carefully check .Rd files for changes
      --------------------------------------------------------------------------------
      i Setting RoxygenNote to "8.0.0"
    Output
      [1] FALSE


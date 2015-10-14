test_that("doxygen_init creates the configuration file with INPUT folder and clean.doxygen cleans up",{
          doxy_file <- "inst/doxygen/Doxyfile"
          # clean up before
          clean.doxygen(base_path=".")
          doxygen_init(doxy_file)
          config <- readLines(doxy_file)
          expect_equal(length(grep("INPUT .* src/",config)),1)
          expect_true(file.exists(doxy_file))
})

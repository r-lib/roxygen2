context("Rbuildignore")

test_that("roxygen ignores files with matching pattern in .Rbuildignore", {
    test_pkg <- temp_copy_pkg("testRbuildignore")
    on.exit(unlink(test_pkg, recursive = TRUE))

    no_rbuildignore_file <- package_files(test_pkg)

    writeLines("^R/ignore_me.R$", file.path(test_pkg, ".Rbuildignore"))
    with_rbuildignore_file <- package_files(test_pkg)

    expect_equal(basename(no_rbuildignore_file), c("a.R", "ignore_me.R"))
    expect_equal(basename(with_rbuildignore_file), "a.R")

})

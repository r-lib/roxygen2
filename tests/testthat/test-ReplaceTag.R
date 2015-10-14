test_that("Fine if one matching", {
          fakeFileStrings <- c('Hello = world','SURE\t= indeed','Hello = you')
          newFake <- ReplaceTag(fakeFileStrings,'SURE','me')
          expect_equal(length(newFake), length(fakeFileStrings))
          expect_equal(length(grep('SURE',newFake)), 1)
          expect_equal(length(grep('me',newFake)), 1)
})
test_that("Test warning if multiple lines matching the tag", {
          fakeFileStrings <- c('Hello = world','SURE\t= indeed','Hello = you')
          expect_warning(ReplaceTag(fakeFileStrings,'Hello','me'))
})
test_that("Add a line if no matching tag", {
          fakeFileStrings <- c('Hello = world','SURE\t= indeed','Hello = you')
          newFake <- ReplaceTag(fakeFileStrings,'Bouh','frightened?')
          expect_equal(length(newFake), length(fakeFileStrings)+1)
          expect_equal(length(grep('Bouh',newFake)), 1)
          expect_equal(length(grep('frightened?',newFake)), 1) 
})

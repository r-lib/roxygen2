test_that(
  "roxygen builds cran proof documentation for S4 classes including links to sub- and super-classes",
  {
    testPkgPath<- createExamplePkgDirs("S4ClassInheritance",'pkg')
    checkExamplePkg( testPkgPath)#,roclets=c("collate", "namespace", "rd")
  }
)
test_that(
  "roxygen builds cran proof documentation for S4 classes including links to 
  methods of sub- and super-classes",
  {
    testPkgPath<- createExamplePkgDirs("S4ClassInheritanceWithMethods",'pkg')
    checkExamplePkg(testPkgPath)#,roclets=c("collate", "namespace", "rd"))
  }
)
test_that(
  "roxygen builds cran proof documentation for S4 generics and methods even when not all parameters have a @param tag",
  {
    testPkgPath<- createExamplePkgDirs("S4GenericsAndMethodsWithUndocumentedParameters",'pkg')
    checkExamplePkg(testPkgPath)#,roclets=c("collate", "namespace", "rd"))
  }
)

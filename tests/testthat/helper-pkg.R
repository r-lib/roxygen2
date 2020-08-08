temp_copy_pkg <- function(pkg) {
  file.copy(normalizePath(pkg), tempdir(), recursive = TRUE)
  normalizePath(file.path(tempdir(), basename(pkg)))
}

writeDescriptionFile<-function(Imports=NULL,Depends=NULL,pkgName="ExamplePackage",pkgDir="."){
	desc <-paste("Package: ",pkgName,"\n",
"Title: EXAMPLEPACKAGE TO TEST s4tags
Encoding: UTF-8
Version:0.1
Date: ",Sys.Date(),"
Author:  Markus Mueller <mamueller@bgc-jena.mpg.de>
Maintainer: Markus Mueller <mamueller@bgc-jena.mpg.de>
Description: This package contains functions whose automatic documentation is tested.
License: GPL-3
"
        ,ifelse(
          is.null(Depends)
          ,""
          ,paste(
              "Depends:"
              ,toString(Depends)
              ,"\n"
              ,sep=""
          )
        )
        ,ifelse(
          is.null(Imports)
          ,"\n"
          ,paste(
              "Imports: "
              ,toString(Imports)
              ,""
              ,sep=""
          )
        )
        , "\n"
        ,sep=""
    )
	descFilePath=file.path(pkgDir,"DESCRIPTION")
	cat(desc,file=descFilePath)
}

cpDir <-  function (sourceDirPath, targetDirPath) 
{
    all_entries <- list.files(sourceDirPath)
    all_dirs <- all_entries[as.logical(lapply(all_entries, function(entry) {
        dir.exists(file.path(sourceDirPath, entry))
    }))]
    all_files <- setdiff(all_entries, all_dirs)
    if (!dir.exists(targetDirPath)) {
        dir.create(targetDirPath)
    }
    lapply(all_files, function(fp) {
        file.copy(file.path(sourceDirPath, fp), file.path(targetDirPath, 
            fp))
    })
    lapply(all_dirs, function(subDirName) {
        cpDir(file.path(sourceDirPath, subDirName), file.path(targetDirPath, 
            subDirName))
    })
}

createExamplePkgDirs <- function(targetPkgName,dirname){
  s4dir <- "S4PackageTests"
  exampleDir<-test_path(s4dir,"test_resources","example_packages")
  sourceDir=file.path(exampleDir,targetPkgName)
  outDir<-test_path(s4dir,"output",targetPkgName)
  
  if (dir.exists(outDir)){
    unlink(outDir,recursive=TRUE)
  }
  targetDirOrg <- file.path(outDir,dirname)
  dir.create(targetDirOrg,recursive=TRUE)
  cpDir(sourceDir,targetDirOrg)
  #
  #if necessarry add a default DESCRIPTION file
  if (!file.exists(file.path(targetDirOrg,"DESCRIPTION"))){ 
    writeDescriptionFile(
      Imports="methods",
      pkgName=targetPkgName,
      pkgDir=targetDirOrg
    )
  }
  return(targetDirOrg)
}
checkExamplePkg <- function(testPkgPath,roclets=NULL){
  document <- TRUE
  if (!is.null(roclets)){
    roxygenize(testPkgPath)
    document <- FALASE
  }
  l<-devtools::check(
    testPkgPath,
    document=document,
    quiet=FALSE,
    cran=TRUE,
    check_dir=dirname(testPkgPath)
  )
  expect_equal(length(l$errors),0)
  expect_equal(length(l$warnings),0)
  expect_equal(length(l$notes),0)
}

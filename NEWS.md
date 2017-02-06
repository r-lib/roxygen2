# roxygen2 6.0.1

* Allowing empty lines in .Rbuildignore. Previously, empty lines caused all 
  files to be ignored. (#572, @jakob-r)

* Automatically generating a usage section for an infix function containing "<-"
  no longer removes "<-" from the function name (#554).

# roxygen2 6.0.0

## Markdown

* Most fields can now be written using Markdown markup instead of the
  traditional Rd language. You can turn on Markdown globally by adding
  `Roxygen: list(markdown = TRUE)` to `DESCRIPTION`. The `@md` / `@noMd`
  tags turn Markdown parsing on / off for the given block. See
  `vignette("markdown")` for more details (#364, #431, #499, #506, #507),
  by @gaborcsardi

## Improved inheritance

* New `@inheritDotParams` allows you to automatically generate parameter 
  documentation for `...` for the common case where you pass `...` on to 
  another function (#512). Because you often override some arguments, it 
  comes with a flexible specification for argument selection:
  
    * `@inheritDotParams foo` takes all parameters from `foo()`
    * `@inheritDotParams foo a b e:h` takes parameters `a`, `b`, and all 
       parameters between `e` and `h`
    * `@inheritDotParams foo -x -y` takes all parameters except for `x` and `y`.
    
    The documentation generated is similar to the style used in `?plot`
    and will eventually be incorporated in to RStudio's autocomplete.

* New `@inherit` generalises `@inheritParams`, and allows to you inherit
  parameters, return, references, title, description, details, sections, and
  seealso.  The default `@inherit my_fun` will inherit all, you can document
  an object entirely by specifying only the `@inherit` tag.  Alternatively, 
  you can select specific tags to inherit with `@inherit my_fun return params`
  (#384).

* New `@inheritSection fun title` allows you to inherit the contents of 
  a single section from another topic (#513).

* `@inheritParams` now works recursively, so that you can inherit parameters
  from a function that inherited its paramters from somewhere else.  It
  also better handles `\dots` as an alias for `...` (#504).

## Minor improvements and bug fixes

### Tags

* `@aliases` are no longer sorted alphabetically, but instead match the 
  order of their usage. This gives you more control in pkgdown.

* `@describeIn` now escapes special characters in function names (#450).

* `@family` see alsos are added in the same order they appear, not 
  alphabetically (#315). Fixed an issue where `.`s were sometimes added 
  between words within a `@family` tag (#477, @kevinushey).

* `@author` is rendered after `@seealso`.

* `@example` gives a nice warning message if you accidentally use it instead 
  of `@examples` (#494). Multiple `@examples` sections are merged (#472, @krlmlr).

* Roxygen will no longer write out topics that don't have a name or title,
  and will instead generate a warning. This makes it easier to detect if
  you've accidentally used `@rdname` with an incorrect value (#474).

### S3

* Non-primitive, internal S3 generics (e.g. 'rbind', 'cbind') are now properly
  detected as S3 generics. (#488, @kevinushey)

* Ensure that `functions` with S3 class are still treated as functions (#455).

* S3 method declarations via `R.methodS3::setMethodS3()` and function
  declarations via `R.oo::setConstructorS3()` are now supported 
  (@HenrikBengtsson, #525).

### S4

* You can now document `setClassUnion()`s (#514).

* The default alias for S4 method now re-addeds trailing ANY signatures
  that are sometimes dropped (#460).

* Back references are now wrapped over multiple lines, if long
  (#493, @LiNk-NY).

### Other

* `"_PACKAGE"` documentation now generates a default `@seealso` combining
  the `URL` and `BugReport` fields, and a default `@author` field generated
  from the `Authors@R` field (#527). It now works from `roxygenise()`; before
  it only worked from `devtools::document()` (#439, @krlmlr).

* Manually created `NAMESPACE` or documentation files are never overwritten, 
  even if using `roxygen2` for the first time (@krlmlr, #436).

* Changes to DESCRIPTION (i.e. `Collate:` and `RoxygenNote`) now use
  the desc package. This will minimise spurious changes (#430).

* `default_data_format()` has been renamed to `object_format()`.

* New `roclet_find()` provides a more flexible way to specify roclets:
  as roclet name (e.g. "rd_roclet"), in an package ("foo::roclet_bar"),
  or with options ("foo::roclet_bar(baz = TRUE)").

* The usage of replacement functions uses non-breaking spaces so that `<-`
  will never get put on its own line (#484).

* Roxygen now parses nonASCII documentation correctly (as long as UTF-8 
  encoded or specified Encoding in DESCRIPTION) (#532, @shrektan),
  and ignores files listed in `.Rbuildignore` (#446, @fmichonneau).

## Extending roxygen2

* Deprecated `register.preref.parser()` and `register.preref.parsers()`
  have been removed. `register_tags()` has also been removed in favour of
  a new `roclet_tags()` generic.
  
* `roclet()` (the constructor), `roclet_tags()`, `roclet_process()`
  `roclet_output()`, `roc_clean()` and now exported making it possible
  to create roclets in other packages.  Helper functions `roxy_tag()` and
  `roxy_tag_warning()` are also exported.

* `new_roclet()` is no longer exported - use `roclet()` instead.

# roxygen2 5.0.1

* Use `ls()`, not `names()` to list elements of environment: fixes R 3.1.0
  incompatibility (#422, @kevinushey).

* `@export` again allows trailing new line (#415).

* Fixed bug in `@noRd`, where usage would cause error (#418).

# roxygen2 5.0.0

## New features

* Roxygen now records its version in a single place: the `RoxygenNote`
  field in the `DESCRIPTION` (#338). This will be the last time an roxygen2
  upgrade changes every file in `man/`.

*   You can now easily re-export functions that you've imported from another 
    package:

    ```R
    #' @export
    magrittr::`%>%`
    ```
    
    All imported-and-re-exported functions will be documented in the same
    file (`rexports.Rd`), containing a brief descrption and links to the 
    original documentation (#376).

*   You can more easily generate package documentation by documenting the 
    special string "_PACKAGE" (@krlmlr, #349):
    
    ```R
    #' @details Details
    "_PACKAGE" 
    ```
    
    The title and description will be automatically filled in from the
    `DESCRIPTION`.

* New tags `@rawRd` and `@rawNamespace` allow you to insert raw (unescaped) 
  in Rd and the `NAMESPACE` (this is useful for conditional imports).
  `@evalRd()` is similar, but instead of literal Rd, you give it R code that 
  produces literal Rd code when run. This should make it easier to experiment 
  with new types of output (#385). 

* Roxygen2 now parses the source code files in the order specified in the
  `Collate` field in `DESCRIPTION`. This improves the ordering of the generated 
  documentation when using `@describeIn` and/or `@rdname` split across several 
  `.R` files, as often happens when working with S4 (#323, #324).

## Minor features and bug fixes

* The contents of documented functions are now also parsed for roxygen comments. 
  This allows, e.g., documenting a parameter's type close to where this type is 
  checked, or documenting implementation details close to the source, and 
  simplifies future extensions such as the documentation of R6 classes 
  (#397, @krlmlr).

* Data objects get a simpler default `@format` that describes only the
  object's class and dimensions.  The former default, generated by generated by 
  `str()`, didn't usually produce useful output and was quite slow. The new S3 
  generic `default_data_format()` generates the format and can be overridden to 
  generate a custom format (#410, @krlmlr).

* The roxygen parsers has been completely rewritten in C++ (#295). This gives a 
  nice performance boost and gives:

  * Better error messages: you now get the exact the line number of the 
    tag, not just the start of the block.
    
  * The parser has been simplified a little: tags now must always start
    on a new line. This is recommended practice anyway, and it means
    that escaping inline `@` (with `@@`) is now optional. (#235)
    
  * Unknown tags now emit a warning, rather than an error.

* `@examples` no longer complains about non-matching braces inside 
  strings (#329).

* `@family` now cross-links each manual page only once, instread of linking
  to all aliases (@gaborcsardi, #283, #367).

* The special `@include` parser has also been rewritten in C++, giving
  a performance boost for larger packages (#401). This is particularly 
  important because it's also called from `devtools::load_all()`.
  Additionally, a space before `@include` is no longer necessary 
  (@krlmlr, #342).

* `@inheritParams foo::bar` ensures that `%` remains escaped (#313). 

* If you document multiple arguments with one `@param`, (e.g. `@param a,b,c`)
  each parameter will get a space after it so it can be wrapped in the 
  generated Rd file (#373).

* `@section`s with identical titles are now merged together, just like 
  `@description` and `@details`. This is useful in conjunction with the 
  `@rdname` tag. (@krlmlr, #300).

* Automatic `@usage` is now correctly generated for functions with string 
  arguments containing `"\""` (#265).

* `load_options()` is now exported so `devtools::document()` doesn't have to
  run `update_collate()` twice (#395). 

* `update_collate()` only rewrites the `Collate` entry in the DESCRIPTION file
  when it changes (#325, #723).

* An empty `NAMESPACE` file is written if it is maintained by `roxygen2`
  (@krlmlr, #348).

* Data that is not lazy-loaded can be documented (@krlmlr, #390).

## Internal changes

* `register.preref.parser()` and `register.preref.parsers()`  have been 
  deprecated - please use `register_tags()` instead.

* Parser callbacks registered with `register_tags()` are now called for fields 
  parsed from the "introduction" (the text before the first tag)
  (@gaborcsardi, #370).

# roxygen2 4.1.1

* Formatting of the `Authors@R` field in the DESCRIPTION file is now retained 
  (@jranke, #330).

* The collate roclet falls back to `base::strwrap()` when generating the
  collate field. This makes roxygen2 compatible with the next version of
  stringr.

* New "vignette" roclet. This vignette automatically rebuilds all out of date
  vignettes (#314).

* An off-by-one error in the C++ Roxygen preparser was fixed.

* The new `@backref` tag makes it possible to override the sourceref for 
  R code generators like `Rcpp` (@krlmlr, #291, #294).

# roxygen2 4.1.0

* The source of the documentation is added to autogenerated `.Rd` files.

* If there are no `@include` tags, roxygen2 leaves the collate field alone.
  This makes it easier to convert an existing project that uses a predefined
  collate, but if you start with `@include` and later remove them, you'll
  need to also remove the collate field (#302, #303).

* Protected a `dir()` with `sort_c()` - If you'd noticed an inconsistency in
  ordering between `devtools::document()` and `devtools::check()` this
  was the cause of that.

* Fixed broken regular expression that caused problems with stringr 1.0.0.

* The `Authors@R` field in `DESCRIPTION` is now longer wrapped(@krlmlr, #284).

* `@describeIn` with plain functions now correctly includes the function name
  and can be applied to data documentation. (@jimhester, #285, #288).

* Works again when called from `Rscript` and `methods` is not loaded
  (@krlmlr, #305).

# roxygen2 4.0.2

* If you don't use `@exports` or other namespace directives, your namespace
  file will not be touched (#276).

* Methods no longer automatically attempt to inherit parameters from 
  their generic. It's too fraught with difficulty (#261).

* Roxygen now understands what to do with `setReplaceMethod()` (#266).

* Parameter documentation is ordered according to the order of the formals, if
  possible (@krlmlr, #63).

* Export `is_s3_method()`.

* Roxygen no longer fails when run in non-UTF-8 locales on windows.

# roxygen2 4.0.1

* Explicit `updateRoxygen()` is no longer needed - `roxygenize()` does the 
  right thing the first time it is run.

* Exporting a S4 generic works (#246).

* `roxygenise()` no longer complains about absense of `wrap` field because it's
  so unlikely that anyone wants the old behaviour (#245).

# roxygen2 4.0.0

Roxygen2 4.0.0 is a major update to roxygen2 that makes provides enhanced error handling and considerably safer default behaviour. Now, roxygen2 will never overwrite a file that it did not create. This means that before you run it for the first time, you'll need to run `roxygen2::upgradeRoxygen()`. That will flag all existing files as being created by roxygen2.

## New features

* Six vignettes provide a comprehensive overview of using roxygen2 in
  practice. Run `browseVignettes("roxygen2")` to access.

* `@describeIn` makes it easier to describe multiple functions in
  one file. This is especially useful if you want to document methods with
  their generic, or with a common class, but it's also useful if you want
  to document multiple related functions in one file (#185).

* `@field` documents the fields on a reference class (#181). It works the
  same way as `@slot` for S4 classes.

* You can now document objects defined elsewhere (like datasets) by
  documenting their name as a string (#221). For example, to document an
  dataset called `mydata`, you can do:

    ```R
    #' Mydata set
    #'
    #' Some data I collected about myself
    "mydata"
    ```

* Roxygen2 now adds a comment to all generated files so that you know
  they've been generated, and should not be hand edited.

* Roxygen2 no longer wraps the text in Rd files by default, i.e. the default
  option is `wrap = FALSE` now. To override it, you have to specify a field
  `Roxygen: list(wrap = TRUE)` in `DESCRIPTION` (#178).

* Roxygenise automatically deletes out-of-date Rd files in `man/`.

## Improved error handling

* Roxygen2 will never overwrite a file that was not generated by
  roxygen2. This means that the first time you use this version of
  roxygen, you'll need to delete all existing Rd files. `roxygenise()`
  gains a clean argument that will automatically remove any files
  previously created by roxygen2.

* Parsing is stricter: many issues that were previously warnings are
  now errors. All errors should now give you the line number of the
  roxygen block associated with the error.

* Every input is now checked to make sure that you have matching braces
  (e.g. every `{` has a matching `}`). This should prevent frustrating
  errors that require careful reading of `.Rd` files (#183).

* `@section` titles and `@export` tags can now only span a single line
  to prevent common bugs.

* `@S3method` is deprecated - just use `@export` (#198).

* Namespace tags now throw parsing errors if you give them bad inputs (#220).

* Better error message if you try to document something other than NULL,
  an assignment, a class, a generic or a method (#194).

## Bug fixes and minor improvements

* Better parsing of non-syntactic function names in other packages when
  used in `@inheritParams` (#236).

* Deprecated arguments to `roxygenise()` (`roxygen.dir`, `copy.package`,
  `overwrite`, `unlink.target`) removed.

* Remove unneeded codetools and tools dependencies.

* Bump required Rcpp version to 0.11.0, and remove custom makefiles.

* Non-syntactic argument names (like `_x`) are now surrounded by back-ticks
  in the usage (#191).

* The internal parsers are no longer part of the public roxygen2 interface.

* Usage statements in generated roxygen statements non-longer contain
  non-ASCII characters and will be wrapped if long (#180).

* By default, reference classes now only document their own methods,
  not their methods of parents (#201).

* Default aliases always include the original name of the object, even if
  overridden by `@name`. This also means that `A <- setClass("A")` will get
  two aliases by default: `A` and `A-class` (#202). Use `@aliases NULL` to
  suppress default alias.

* Non-syntactic class names (like `<-`) are now escaped in the usage
  section of S4 methods (#205).

* Eliminated two more cases where wrapping occured even when `wrap = FALSE`.

# roxygen2 3.1.0

## Documentation for reference classes

It's now possible to document reference classes, using the "docstring"
convention described in `?setRefClass`. If you want to provide a short
paragraph description of what a method does, make the first component of the
message a string containing the description, e.g.:

```R
setRefClass("A", methods = list(
  f = function(a, b) {
    "Take numbers \code{a} and \code{b} and add them together"
    a + b
  }
))
```

Unlike the documentation for R functions, the documentation for methods can
be quite succinct.

Roxygen adopts the convention that documented methods are public, and will
be listed in the man page for the object. Undocumented methods are private and
will not be shown in the documentation. The methods for all superclasses are
also listed, so that you don't need to flip through multiple pages of
documentation to understand what you can do with an object. All documented
methods will be placed in a bulleted list in a section titled "Methods", the
method usage will be automatically prepended to the docstring.

## Minor fixes and improvements

* Fixes for Rcpp 0.11.0 compatibility.

* `roxygenise()` now invisible returns a list of all files generated
  by individual roclets. This is useful for tools that want to figure
  out if there are extra files in the `man/` directory.

* `is_s3_generic()` now recognises group generics (#166).

* Don't try and add parameters for data objects (#165).

* Sort output of families using C locale (#171).

* `@family` now escapes function names in references (#172).

# roxygen2 3.0.0

Roxygen2 now fully supports S4 and RC (reference classes) - you should no
longer need to manually add `@alias` or `@usage` tags for S4 classes, methods
and generics, or for RC classes.

* The default usage definitions are much better, generating the correct
  usage for data sets (#122), S3 methods (without additional `@method` tag),
  S4 generics, S4 methods, and for replacement (#119) and infix functions.
  Backslashes in function arguments in are correctly escaped. Usage statements
  also use a more sophisticated line wrapping algorithm so that they should
  cause fewer problems with the R CMD check line limit. (#89, #125).

* S4 classes, S4 methods, and RC classes are given better default topics,
  and the file names corresponding to those topics are shorter.

* S4 methods will automatically inherit parameter documentation from their
  generic.

* `@slot name description` allows you to document the slots of a S4 class.

S3 support has also been improved: roxygen2 now figures out whether a function
is a S3 method or generic. (In the rare cases it does so incorrectly, use
`@method` to manually describe the generic and class associated with a method). This means you can remove existing uses of `@method`, and can replace
`@S3method` with `@export`.

Roxygen now has support for package specific options through the `Roxygen`
field in the `DESCRIPTION`. The value of the field should be R code that
results in a list. Currently only `wrap` and `roclet` values are supported:

* Turn off Rd re-wrapping with adding `Roxygen: list(wrap = FALSE)`

* Change the default roclets by specifying
  `Roxygen: list(roclets = c("collate", "rd"))`

Roxygen 3.0 also includes a number of minor fixes and improvements:

* Infix functions are now escaped correctly in the `NAMESPACE`. (Thanks to
  @crowding, #111)

* `roxygenise()` now works more like `devtools::document()` and only ever works
  in the current directory. The arguments `roxygen.dir`, `overwrite`,
  `copy.package` and `unlink.target` have been deprecated due to potential
  data loss problems.

* The collate roclet is no longer a roclet: it processes R files using custom
  code (only statically, not dynamically) and is designed to be executed before
  the code is sourced.  Run `update_collate()` to update the Collate directive
  based on `@include` tags - if there are none present, a collate directive
  will not be generated.

* `@useDynLib` now works with more possible specifications - if you include a
  comma in the tag value, the output will be passed as is. This means that
  `@useDynLib mypackage, .registration = TRUE` will now generate
  `useDynLib(mypackage, .registration = TRUE)` in the NAMESPACE. (#124)

* `inst` directory not created by default (#56).

* Explicitly depend on `utils` and `methods` packages to make roxygen
  compatible with `Rscript` (#72). Import `digest` package instead of
  depending on it.

* Always use C locale when sorting `NAMESPACE` file or tags in `.Rd` files.
  This ensures a consistent ordering across systems (#127).

* Templates with extension `.r` are supported on case-sensitive file systems
  (#115). Template variables now actually work (#160, thanks to @bronaugh).

* Suppress default aliases, format and usage with `@aliases NULL`,
  `@format NULL` and `@usage NULL`.

# roxygen2 2.2.2

* Correctly use keyword `datasets` not `dataset` (Fixes #60)

* Reference classes no longer given incorrect docType (data).

# roxygen2 2.2.1

* Use unicode escapes in test files so tests pass on all platforms.

* Work around bug in `gsub` in C locale by manually specifying `Encoding()`.

# roxygen2 2.2

## New features

* Package docType will automatically add package alias, if needed. (Fixes #4)

* Data docType will automatically add `datasets` keyword, default usage, and
  default format. (Fixes #5). Data docType automatically added to data
  objects.

* New `@encoding` tag for manually setting non-ASCII encodings when needed.
  (Fixes #7)

## Bug fixes

* `write.description()` now tries much harder to respect
  users' original DESCRIPTION field formatting instead of forcibly
  re-wrapping certain fields at 60 characters.

* `@details` and `@description` now work correctly

* `@useDynLib` now works correctly:

       @useDynLib packageName routine1 routine2

   produces

       useDynLib(packageName, routine1)
       useDynLib(packageName, routine2)

   in the NAMESPACE file, instead of separate (wrong) useDynLib statements as
   before.

* All namespace import directives now behave in the same way as the export
  directives, producing multiple single directives instead one multiple
  directive: `@importClassesFrom pkg a b` now produces
  `importClassesFrom(pkg, a)` and `importClassesFrom(pkg, b)`

* In example files included with `@example` you can now use infix operators
  (e.g. %*%) or other things with %, because they will be preceded by a
  backslash in the Rd file. This behaviour was already in place for examples
  directly included with `@examples`.

* Aliases are no longer quoted, and % is escaped with a backslash (Fixes #24).
  Names also have % escaped (Fixes #50)

* Replacement functions (e.g. `foo<-`) now get correct usage statements:
  `foo() <- value` instead of `foo()<-value`. (Fixes #38)

* Functions with no arguments now correctly get usage statements (Fixes #35)

* Indentation in examples now preserved (Fixes #27)

* roxygen2 will replace characters that are not valid in filenames with a
  character substitute, e.g. `[]` becomes `sub`, `<-` becomes `set` (Fixes #6)

* Usage strings use non-breaking spaces to prevent string default values
  containing whitespace to be split across multiple lines. This may cause
  problems in the unlikely event that you have default value containing a
  non-breaking space (`"\uA0"')  (Fixes #21)

* Functions with quoted names now get correct usage statements (Fixes #41)

* Objects that no longer exist are not documented (Fixes #42)

* Errors now display file name and line number of roxygen block to help you
  find the problem. Thanks to code contributions from Renaud Gaujoux.
  (Fixes #13)

* Documentation with no untagged text but with `@title`, `@description` and
  `@details` tags now produces correct output.

# roxygen2 2.1

## New features

* package dependencies loaded automatically

* added support for the `@source` tag

## Bug fixes

* `NAMESPACE` file no longer needs to exist

* `Collate` field in `DESCRIPTION` no longer needs to exist

* `=` now recognised as way of assigning functions

* `x$y <- function() {...}` no longer causes error

* `@example` no longer added extra new-lines.

* Correct directory normalisation under windows fixes broken test.

* A special thanks goes to Yihui Xie who contributed all of the fixes and
  improvements (bar one) in this version!

# roxygen2 2.0

## Major changes

* now works with run-time details to give more accurate output. This requires
  that the source code that roxygen is documenting be loaded prior to
  documentation. roxygen will attempt to do so, but you need to ensure
  required packages are loaded.

    Run-time data fixes some long standing bugs where roxygen couldn't correctly
    figure out function usage. We are not aware of any cases where you still
    need to use the `@usage` tag.

* written in idiomatic R, and uses S3 instead of a homegrown class system.

* roclets build up an internal data structure instead of writing to disk
  directly. This means that you can now use the `@rdname` tag to merge
  documentation for multiple functions into one file, and that only unique
  namespace directives are written to `NAMESPACE` (which makes `@importFrom`
  much more useful).

* some features have been removed, and may or may not (based on your feedback)
  be reincluded. These include the callgraph roclet, and `R CMD roxygen`,
  which only worked on some systems.

* a templating system: use the `@template` tag to insert a `brew` template
  stored in `man-roxygen`. Template variables can be set using `@templateVar
  name value` and retrieved from within the template with `<%= name %>`

* extensive use of caching to make repeated runs as fast as possible. To clear
  caches and guarantee a complete rebuild, use `clear_caches()`.

* parsing of "introduction" (the text before the first tag) has changed. Now
  the title consists of the first paragraph (i.e. all text before the first
  empty line), the second paragraph is the description and all others are put
  in the details. Any component can be overridden with `@title`,
  `@description` and `@details` as appropriate.

## Minor changes

* `@name` is always output as an alias, even if `@aliases` are used.

* `@export` correctly uses `@method` to generate `S3method` namespace
  directive

## New tags

* `@rdname filename` sets the output filename (without extension). Use for
  functions non-alphanumeric functions (e.g. `[<-`) or if you want to document
  multiple functions in one file

* `@template templatename` includes a documentation template (see above)

* `@section Section title: contents` includes a section with any title. Don't
  forget the colon! That separates the title of the section from its contents.

* `@description` and `@details` tags allow you to specify description and
  details components in a template

* `@family family name` automatically adds see-also cross-references between
  all functions in a family. A function can belong to multiple families.

* `@inheritParams name` allows you to inherit the documentation for parameters
  from another function, either within the current package (`function`) or in
  any other installed package (`package:function`). Currently only supports
  single inheritance (i.e. you can't inherit from a function that inherits
  from another function), but you can have multiple @inheritParams tags.

* `@format` has been implemented; it existed in the roxygen package but was
  actually ignored

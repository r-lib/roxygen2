# useful warning if no topic found

    Code
      . <- find_package("doesntexist")
    Message
      x Could not resolve link to topic "doesntexist" in the dependencies or base packages.
      i If you haven't documented "doesntexist" yet, or just changed its name, this is normal. Once "doesntexist" is documented, this warning goes away.
      i Make sure that the name of the topic is spelled correctly.
      i Always list the linked package as a dependency.
      i Alternatively, you can fully qualify the link with a package name.

# gives useful warning if same name in multiple packages

    Code
      . <- find_package("pkg_env")
    Message
      x Topic "pkg_env" is available in multiple packages: pkgload and rlang.
      i Qualify topic explicitly with a package name when linking to it.


# A `RoxyTopic` is an ordered collection of unique rd_sections

A `RoxyTopic` object corresponds to a generated `.Rd` file.

## Public fields

- `sections`:

  Named list of sections. Each item must be an
  [`rd_section()`](https://roxygen2.r-lib.org/dev/reference/rd_section.md)
  object.

- `filename`:

  Path to the `.Rd` file to generate.

## Methods

### Public methods

- [`RoxyTopic$format()`](#method-RoxyTopic-format)

- [`RoxyTopic$is_valid()`](#method-RoxyTopic-is_valid)

- [`RoxyTopic$has_section()`](#method-RoxyTopic-has_section)

- [`RoxyTopic$get_section()`](#method-RoxyTopic-get_section)

- [`RoxyTopic$get_value()`](#method-RoxyTopic-get_value)

- [`RoxyTopic$get_rd()`](#method-RoxyTopic-get_rd)

- [`RoxyTopic$get_name()`](#method-RoxyTopic-get_name)

- [`RoxyTopic$inherits_from()`](#method-RoxyTopic-inherits_from)

- [`RoxyTopic$inherits_section_from()`](#method-RoxyTopic-inherits_section_from)

- [`RoxyTopic$add()`](#method-RoxyTopic-add)

- [`RoxyTopic$add_section()`](#method-RoxyTopic-add_section)

- [`RoxyTopic$clone()`](#method-RoxyTopic-clone)

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Format the `.Rd` file. It considers the sections in particular order,
even though Rd tools will reorder them again.

#### Usage

    RoxyTopic$format(...)

#### Arguments

- `...`:

  Passed to the [`format()`](https://rdrr.io/r/base/format.html) methods
  of the
  [`rd_section()`](https://roxygen2.r-lib.org/dev/reference/rd_section.md)
  objects, the sections.

#### Returns

Character string.

------------------------------------------------------------------------

### Method `is_valid()`

Check if an `.Rd` file is valid

#### Usage

    RoxyTopic$is_valid()

#### Returns

Logical flag, `TRUE` for valid `.Rd` files

------------------------------------------------------------------------

### Method `has_section()`

Check if an `.Rd` file has a certain section.

#### Usage

    RoxyTopic$has_section(type)

#### Arguments

- `type`:

  Section type, a character scalar.

#### Returns

Logical flag.

------------------------------------------------------------------------

### Method `get_section()`

Query a section.

#### Usage

    RoxyTopic$get_section(type)

#### Arguments

- `type`:

  Section type, a character scalar.

#### Returns

The [rd_section](https://roxygen2.r-lib.org/dev/reference/rd_section.md)
object representing the section, or `NULL` if the topic has no such
section.

------------------------------------------------------------------------

### Method `get_value()`

Query the value of a section. This is the value of the
[rd_section](https://roxygen2.r-lib.org/dev/reference/rd_section.md)
object.

#### Usage

    RoxyTopic$get_value(type)

#### Arguments

- `type`:

  Section type, a character scalar.

#### Returns

Value.

------------------------------------------------------------------------

### Method `get_rd()`

Get the Rd code of a section.

#### Usage

    RoxyTopic$get_rd(type)

#### Arguments

- `type`:

  Section type, a character scalar.

#### Returns

Character vector, one element per line.

------------------------------------------------------------------------

### Method `get_name()`

Get the value of the `name` section. This is the name of the Rd topic.

#### Usage

    RoxyTopic$get_name()

#### Returns

Character scalar.

------------------------------------------------------------------------

### Method `inherits_from()`

Query the topics this topic inherits `type` from.

#### Usage

    RoxyTopic$inherits_from(type)

#### Arguments

- `type`:

  Section type, a character scalar.

#### Returns

A character vector of topic names.

------------------------------------------------------------------------

### Method `inherits_section_from()`

Query the topics this topic inherits sections from.

#### Usage

    RoxyTopic$inherits_section_from()

#### Returns

A character vector of topic names.

------------------------------------------------------------------------

### Method `add()`

Add one or more sections to the topic.

#### Usage

    RoxyTopic$add(x, block = "???", overwrite = FALSE)

#### Arguments

- `x`:

  Section(s) to add. It may be another `RoxyTopic` object, all of its
  sections will be added; or an
  [rd_section](https://roxygen2.r-lib.org/dev/reference/rd_section.md)
  object; or a list of
  [rd_section](https://roxygen2.r-lib.org/dev/reference/rd_section.md)
  objects to add.

- `block`:

  Name of block to use in error messages.

- `overwrite`:

  Whether to overwrite an existing section. If `FALSE` then the two
  sections will be merged.

------------------------------------------------------------------------

### Method `add_section()`

Add a section.

#### Usage

    RoxyTopic$add_section(section, block = "???", overwrite = FALSE)

#### Arguments

- `section`:

  [rd_section](https://roxygen2.r-lib.org/dev/reference/rd_section.md)
  object to add.

- `block`:

  Name of block to use in error messages.

- `overwrite`:

  Whether to overwrite an existing section. If `FALSE` then the two
  sections will be merged.

#### Details

Ensures that each type of name (as given by its name), only appears once
in `self$sections`. This method if for internal use only.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RoxyTopic$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

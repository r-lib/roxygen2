# update_collate() checks that directory exists

    Code
      update_collate("doesn't-exist")
    Condition
      Error in `update_collate()`:
      ! 'doesn't-exist' doesn't exist

# DESCRIPTION file is re-written only if collate changes

    Code
      update_collate(path)
    Message
      Updating collate directive in '<path>/DESCRIPTION'
    Code
      # Second run should be idempotent
      update_collate(path)

# drops bad collect directives

    Code
      update_collate(".")
    Message
      x a.R: unknown path in `@include foo`.
      Updating collate directive in './DESCRIPTION'


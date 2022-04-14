# update_collate() checks that directory exists

    Code
      update_collate("doesn't-exist")
    Condition
      Error in `update_collate()`:
      ! 'doesn't-exist' doesn't exist

# DESCRIPTION file is re-written only if collate changes

    Code
      update_collate(".")
    Message
      Updating collate directive in './DESCRIPTION'
    Code
      # Second run should be idempotent
      update_collate(".")


# generates informative warnings

    Code
      tag <- roxy_test_tag()
      find_topic_filename("11papaya", "foo", tag)
    Message
      x test.R:1: @test refers to unavailable topic 11papaya::foo.
      Caused by error in `find.package()`:
      ! there is no package called '11papaya'
    Output
      [1] "foo"
    Code
      find_topic_filename("stringr", "foofofofoo", tag)
    Message
      x test.R:1: @test refers to unavailable topic stringr::foofofofoo.
    Output
      [1] "foofofofoo"


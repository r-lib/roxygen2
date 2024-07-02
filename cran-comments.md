## R CMD check results

There was one note:  Found non-API calls to R: 'SETLENGTH', 'SET_GROWABLE_BIT', 'SET_TRUELENGTH'.

These come from the upstream cpp11, and we're working to fix them there.

## revdepcheck results

I did not check revdeps since the small bug fixes I made did not cause failures on previous submission.

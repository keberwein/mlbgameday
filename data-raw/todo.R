
# Add gids list option to get_payload. Should be easy, just replace urlz with the user supplied gids list.

# Add an umpire_id dataset similar to player_id.

# Try to append starting pitcher to gids df. For internal gids we may have to loop over all the gids and grab pitcher_id from 1st inning.
# For external gids, we have have to loop over the gid after TryCatch, which would make things much longer.
# Write external piece first and see how much longer that takes, may not be worth it.
# This would allow user to search for year and starting pitcher.

## Test other data sets. Will have to write if statement in get_payload.

## Test with microbenchmark package.

## Add methods for openWAR stuff that doesn't exist in pitchRx.

## minor-league and pre-2008 have no inning all. Dependable mlb data goes back to 2005 with no inning_all.

## Look at viz options but don't go nuts.

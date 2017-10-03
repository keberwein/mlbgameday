## Getting overscope errors with DB connection where there were no games played on those days. Example below..
# innings_df <- mlbgameday::get_payload(start = "2009-01-01", end = "2009-06-30", db_con = con)
## This runs fine with no db connection though... hmmm....

## Need to close all connections on the database so we don't get this warning:
# 1: In .Internal(gc(verbose, reset)) :
# closing unused connection 8 (<-localhost:11669)

## Add methods for openWAR stuff that doesn't exist in pitchRx.

## minor-league and pre-2008 have no inning all. Dependable mlb data goes back to 2005 with no inning_all.

## Look at viz options but don't go nuts.

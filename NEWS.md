# mlbgameday 0.1.2.1

## Bug fixes

* Game ids were being cut off in the bis_boxscore dataset. Fixed the string length.

* Calculation for atbat number changed from end_tfs_zulu to start_tfs_zulu due to missing data for some games.


# mlbgameday 0.1.2

## Enhancements

* Added a `data_automation` vignette.

## Bug Fixes

* Fixed pitch count logic for the `pitch` table.

# mlbgameday 0.1.1

## Enhancements

* Added overwrite argument to the `get_payload()` function.

* Enhanced the logic that ties action nodes to atbat nodes.

## Bug Fixes

* Inconsistent column ordering in linescore dataset caused `dbWrite()` errors.

# mlbgameday 0.1.0

## Enhancements

* Added atbat num to the output of the action table of the innings_all payload.

## Bug Fixes

* Fixed mal-formed urls returned by the `make_gids()` function.

* Added error checks to the xml mapping in the `transform_payload()` function.

# mlbgameday 0.0.1

## Enhancements

* Added `transform()` function to reduce the size of returned datasets.

## Vignettes

* Added Database Connections, Parallel Processing, Pitch Plotting and Search Games vignettes.

## Testing

* Added test folder to test expected api return.


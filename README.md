<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/keberwein/blscrapeR.png?branch=master)](https://travis-ci.org/keberwein/mlbgameday) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/blscrapeR)](http://www.r-pkg.org/badges/version/mlbgameday) [![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

Why mlbgameday?
---------------

Designed to facilitate extract, transform and load for MLB “gameday” data. The package is optimized for parallel processing and dealing with data, which may be larger than memory. There are other packages in the R universe that were built to perform statistics and visualizations on these data, but `mlbgameday` is concerned primarily with fast and efficient loading of data, using modern data extraction techniques.

Install
-------

-   The latest development version from GitHub:

``` r
devtools::install_github("keberwein/mlbgameday")
```

Basic Usage
-----------

Although the package is optimized for parallel processing, it will also work without registering a parallel back-end. When only querying a single day's data, a parallel back-end may not provide much additional performance. However, parallel back-ends are suggested for larger data sets, as the process will be faster by several orders of magnitude.

``` r
library(mlbgameday)

innings_df <- get_payload(start = "2017-04-03", end = "2017-04-04")
head(innings_df$atbat, 5)
#>   num b s o start_tfs       start_tfs_zulu batter stand b_height pitcher
#> 1   1 2 2 1    170552 2017-04-03T17:05:52Z 543829     L     5-11  544931
#> 2   2 2 2 2    170754 2017-04-03T17:07:54Z 592663     R      6-1  544931
#> 3   3 0 3 3    171046 2017-04-03T17:10:46Z 592885     L      6-3  544931
#> 4   9 1 2 1    173017 2017-04-03T17:30:17Z 519317     R      6-6  544931
#> 5  10 1 2 2    173143 2017-04-03T17:31:43Z 571506     L      6-3  544931
#>   p_throws
#> 1        R
#> 2        R
#> 3        R
#> 4        R
#> 5        R
#>                                                                                                des
#> 1                                             Dee Gordon lines out to left fielder Jayson Werth.  
#> 2  J.  T.   Realmuto grounds out sharply, shortstop Trea Turner to first baseman Ryan Zimmerman.  
#> 3                                                         Christian Yelich strikes out swinging.  
#> 4          Giancarlo Stanton grounds out, shortstop Trea Turner to first baseman Ryan Zimmerman.  
#> 5 Justin Bour grounds out sharply, second baseman Daniel Murphy to first baseman Ryan Zimmerman.  
#>                                                                                                        des_es
#> 1                                         Dee Gordon batea línea de out a jardinero izquierdo Jayson Werth.  
#> 2 J.  T.   Realmuto batea rodado de out fuertemente, campo corto Trea Turner a primera base Ryan Zimmerman.  
#> 3                                                                     Christian Yelich se poncha tirándole.  
#> 4             Giancarlo Stanton batea rodado de out, campo corto Trea Turner a primera base Ryan Zimmerman.  
#> 5    Justin Bour batea rodado de out fuertemente, segunda base Daniel Murphy a primera base Ryan Zimmerman.  
#>   event_num     event        event_es home_team_runs away_team_runs inning
#> 1        11   Lineout    Línea de Out              0              0      1
#> 2        20 Groundout Roletazo de Out              0              0      1
#> 3        26 Strikeout          Ponche              0              0      1
#> 4        77 Groundout Roletazo de Out              0              0      2
#> 5        84 Groundout Roletazo de Out              0              0      2
#>   next_ inning_side
#> 1     Y         top
#> 2     Y         top
#> 3     Y         top
#> 4     Y         top
#> 5     Y         top
#>                                                                                                                      url
#> 1 http://gd2.mlb.com/components/game/mlb//year_2017/month_04/day_03/gid_2017_04_03_miamlb_wasmlb_1/inning/inning_all.xml
#> 2 http://gd2.mlb.com/components/game/mlb//year_2017/month_04/day_03/gid_2017_04_03_miamlb_wasmlb_1/inning/inning_all.xml
#> 3 http://gd2.mlb.com/components/game/mlb//year_2017/month_04/day_03/gid_2017_04_03_miamlb_wasmlb_1/inning/inning_all.xml
#> 4 http://gd2.mlb.com/components/game/mlb//year_2017/month_04/day_03/gid_2017_04_03_miamlb_wasmlb_1/inning/inning_all.xml
#> 5 http://gd2.mlb.com/components/game/mlb//year_2017/month_04/day_03/gid_2017_04_03_miamlb_wasmlb_1/inning/inning_all.xml
#>   date                    gameday_link score
#> 1 <NA> /gid_2017_04_03_miamlb_wasmlb_1  <NA>
#> 2 <NA> /gid_2017_04_03_miamlb_wasmlb_1  <NA>
#> 3 <NA> /gid_2017_04_03_miamlb_wasmlb_1  <NA>
#> 4 <NA> /gid_2017_04_03_miamlb_wasmlb_1  <NA>
#> 5 <NA> /gid_2017_04_03_miamlb_wasmlb_1  <NA>
#>                              play_guid event2 event2_es event3 event3_es
#> 1 76e23666-26f1-4339-967f-c6f759d864f4   <NA>      <NA>   <NA>      <NA>
#> 2 7dc8fa05-f5dc-47a8-a53c-72eea6ea5160   <NA>      <NA>   <NA>      <NA>
#> 3 dd676bf8-5698-40e1-8393-41d192ae7e6b   <NA>      <NA>   <NA>      <NA>
#> 4 8075f9eb-4cf8-4f39-9c29-3386f4860be5   <NA>      <NA>   <NA>      <NA>
#> 5 7bf9b0e2-6df2-4a22-9034-1bdb2c9cacdc   <NA>      <NA>   <NA>      <NA>
#>        batter_name      pitcher_name
#> 1   Devaris Gordon Stephen Strasburg
#> 2   Jacob Realmuto Stephen Strasburg
#> 3 Christian Yelich Stephen Strasburg
#> 4  Michael Stanton Stephen Strasburg
#> 5      Justin Bour Stephen Strasburg
```

Parallel Processing
-------------------

The package's internal functions are optimized to work with the `doParallel` package. By default, the R language will use one core of our CPU. The `doParallel` package enables us to use several cores, which will execute tasks simultaneously. For example, in standard regular season play for all teams, the function has to process over 2,400 individual files, which depending on your system, can take more than thirty minutes. Parallel processing speeds this process up by several times.

``` r
library(mlbgameday)
library(doParallel)

# First we need to register our parallel cluster.
# We're setting the number of cores to use as the machine's maximum number of cores minus one for background processes.
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)  
registerDoParallel(cl)

# Then run the get_payload function as normal.
innings_df <- get_payload(start = "2017-04-03", end = "2017-04-10")
head(innings_df$atbat, 5)
#>   num b s o start_tfs       start_tfs_zulu batter stand b_height pitcher
#> 1   1 2 2 1    170552 2017-04-03T17:05:52Z 543829     L     5-11  544931
#> 2   2 2 2 2    170754 2017-04-03T17:07:54Z 592663     R      6-1  544931
#> 3   3 0 3 3    171046 2017-04-03T17:10:46Z 592885     L      6-3  544931
#> 4   9 1 2 1    173017 2017-04-03T17:30:17Z 519317     R      6-6  544931
#> 5  10 1 2 2    173143 2017-04-03T17:31:43Z 571506     L      6-3  544931
#>   p_throws
#> 1        R
#> 2        R
#> 3        R
#> 4        R
#> 5        R
#>                                                                                                des
#> 1                                             Dee Gordon lines out to left fielder Jayson Werth.  
#> 2  J.  T.   Realmuto grounds out sharply, shortstop Trea Turner to first baseman Ryan Zimmerman.  
#> 3                                                         Christian Yelich strikes out swinging.  
#> 4          Giancarlo Stanton grounds out, shortstop Trea Turner to first baseman Ryan Zimmerman.  
#> 5 Justin Bour grounds out sharply, second baseman Daniel Murphy to first baseman Ryan Zimmerman.  
#>                                                                                                        des_es
#> 1                                         Dee Gordon batea línea de out a jardinero izquierdo Jayson Werth.  
#> 2 J.  T.   Realmuto batea rodado de out fuertemente, campo corto Trea Turner a primera base Ryan Zimmerman.  
#> 3                                                                     Christian Yelich se poncha tirándole.  
#> 4             Giancarlo Stanton batea rodado de out, campo corto Trea Turner a primera base Ryan Zimmerman.  
#> 5    Justin Bour batea rodado de out fuertemente, segunda base Daniel Murphy a primera base Ryan Zimmerman.  
#>   event_num     event        event_es home_team_runs away_team_runs inning
#> 1        11   Lineout    Línea de Out              0              0      1
#> 2        20 Groundout Roletazo de Out              0              0      1
#> 3        26 Strikeout          Ponche              0              0      1
#> 4        77 Groundout Roletazo de Out              0              0      2
#> 5        84 Groundout Roletazo de Out              0              0      2
#>   next_ inning_side
#> 1     Y         top
#> 2     Y         top
#> 3     Y         top
#> 4     Y         top
#> 5     Y         top
#>                                                                                                                      url
#> 1 http://gd2.mlb.com/components/game/mlb//year_2017/month_04/day_03/gid_2017_04_03_miamlb_wasmlb_1/inning/inning_all.xml
#> 2 http://gd2.mlb.com/components/game/mlb//year_2017/month_04/day_03/gid_2017_04_03_miamlb_wasmlb_1/inning/inning_all.xml
#> 3 http://gd2.mlb.com/components/game/mlb//year_2017/month_04/day_03/gid_2017_04_03_miamlb_wasmlb_1/inning/inning_all.xml
#> 4 http://gd2.mlb.com/components/game/mlb//year_2017/month_04/day_03/gid_2017_04_03_miamlb_wasmlb_1/inning/inning_all.xml
#> 5 http://gd2.mlb.com/components/game/mlb//year_2017/month_04/day_03/gid_2017_04_03_miamlb_wasmlb_1/inning/inning_all.xml
#>   date                    gameday_link score
#> 1 <NA> /gid_2017_04_03_miamlb_wasmlb_1  <NA>
#> 2 <NA> /gid_2017_04_03_miamlb_wasmlb_1  <NA>
#> 3 <NA> /gid_2017_04_03_miamlb_wasmlb_1  <NA>
#> 4 <NA> /gid_2017_04_03_miamlb_wasmlb_1  <NA>
#> 5 <NA> /gid_2017_04_03_miamlb_wasmlb_1  <NA>
#>                              play_guid event2 event2_es event3 event3_es
#> 1 76e23666-26f1-4339-967f-c6f759d864f4   <NA>      <NA>   <NA>      <NA>
#> 2 7dc8fa05-f5dc-47a8-a53c-72eea6ea5160   <NA>      <NA>   <NA>      <NA>
#> 3 dd676bf8-5698-40e1-8393-41d192ae7e6b   <NA>      <NA>   <NA>      <NA>
#> 4 8075f9eb-4cf8-4f39-9c29-3386f4860be5   <NA>      <NA>   <NA>      <NA>
#> 5 7bf9b0e2-6df2-4a22-9034-1bdb2c9cacdc   <NA>      <NA>   <NA>      <NA>
#>        batter_name      pitcher_name
#> 1   Devaris Gordon Stephen Strasburg
#> 2   Jacob Realmuto Stephen Strasburg
#> 3 Christian Yelich Stephen Strasburg
#> 4  Michael Stanton Stephen Strasburg
#> 5      Justin Bour Stephen Strasburg

# Don't forget to stop the cluster when finished.
stopImplicitCluster()
rm(cl)
```

Databases
---------

When collecting several seasons worth of data, the data may become larger than memory. If this is the case, the `mlbgameday` package includes functionality to break the data into "chunks" and load into a database. Database connections are provided by the `DBI` package, which includes connections for most modern relational databases. Below is an example that creates a SQLite database in our working directory and populates it with MLBgameday data.

``` r
library(mlbgameday)
library(doParallel)
library(DBI)
library(RSQLite)

# First we need to register our parallel cluster.
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)  
registerDoParallel(cl)

# Create the database
con <- dbConnect(RSQLite::SQLite(), dbname = "gameday.sqlite3")

# Collect all games, including pre and post-season for the 2016 season.
innings_df <- get_payload(start = "2016-01-01", end = "2017-01-01", db_con = con)

# Don't forget to stop the cluster when finished.
stopImplicitCluster()
rm(cl)
```

Gameday Data Sets
-----------------

Those familiar with Carson Sievert's `pitchRx` package probably recognize the default format returned by the `get_payload` function. The format was intentionally designed to be similar to the data returned by the `pitchRx` package for those who may be keeping persistent databases. The default data set returned is "inning\_all," however there are several more options including:

-   inning\_hit

-   bis\_boxscore

-   game\_events

-   linescore

For example, the following with query the linescore data set.

``` r
library(mlbgameday)

linescore_df <- get_payload(start = "2017-04-03", end = "2017-04-04", dataset = "linescore")
head(linescore_df$game, 5)
#>                           id                       venue game_pk time
#> 1 2017/04/03/miamlb-wasmlb-1              Nationals Park  490105 1:05
#> 2 2017/04/03/atlmlb-nynmlb-1                  Citi Field  490098 1:10
#> 3 2017/04/03/pitmlb-bosmlb-1                 Fenway Park  490108 2:05
#> 4 2017/04/03/colmlb-milmlb-1                 Miller Park  490101 2:10
#> 5 2017/04/03/tormlb-balmlb-1 Oriole Park at Camden Yards  490112 3:05
#>         time_date time_date_aw_lg time_date_hm_lg time_zone ampm
#> 1 2017/04/03 1:05 2017/04/03 1:05 2017/04/03 1:05        ET   PM
#> 2 2017/04/03 1:10 2017/04/03 1:10 2017/04/03 1:10        ET   PM
#> 3 2017/04/03 2:05 2017/04/03 2:05 2017/04/03 2:05        ET   PM
#> 4 2017/04/03 2:10 2017/04/03 2:10 2017/04/03 2:10        ET   PM
#> 5 2017/04/03 3:05 2017/04/03 3:05 2017/04/03 3:05        ET   PM
#>   first_pitch_et away_time away_time_zone away_ampm home_time
#> 1                     1:05             ET        PM      1:05
#> 2                     1:10             ET        PM      1:10
#> 3                     2:05             ET        PM      2:05
#> 4                    12:10             MT        PM      1:10
#> 5                     3:05             ET        PM      3:05
#>   home_time_zone home_ampm game_type tiebreaker_sw original_date
#> 1             ET        PM         R             N          <NA>
#> 2             ET        PM         R             N          <NA>
#> 3             ET        PM         R             N          <NA>
#> 4             CT        PM         R             N          <NA>
#> 5             ET        PM         R             N          <NA>
#>   time_zone_aw_lg time_zone_hm_lg time_aw_lg aw_lg_ampm tz_aw_lg_gen
#> 1              -4              -4       1:05         PM           ET
#> 2              -4              -4       1:10         PM           ET
#> 3              -4              -4       2:05         PM           ET
#> 4              -4              -4       2:10         PM           ET
#> 5              -4              -4       3:05         PM           ET
#>   time_hm_lg hm_lg_ampm tz_hm_lg_gen venue_id scheduled_innings
#> 1       1:05         PM           ET     3309                 9
#> 2       1:10         PM           ET     3289                 9
#> 3       2:05         PM           ET        3                 9
#> 4       2:10         PM           ET       32                 9
#> 5       3:05         PM           ET        2                 9
#>             description away_name_abbrev home_name_abbrev away_code
#> 1 Nationals home opener              MIA              WSH       mia
#> 2      Mets home opener              ATL              NYM       atl
#> 3   Red Sox home opener              PIT              BOS       pit
#> 4   Brewers home opener              COL              MIL       col
#> 5   Orioles home opener              TOR              BAL       tor
#>   away_file_code away_team_id away_team_city away_team_name away_division
#> 1            mia          146          Miami        Marlins             E
#> 2            atl          144        Atlanta         Braves             E
#> 3            pit          134     Pittsburgh        Pirates             C
#> 4            col          115       Colorado        Rockies             W
#> 5            tor          141        Toronto      Blue Jays             E
#>   away_league_id away_sport_code home_code home_file_code home_team_id
#> 1            104             mlb       was            was          120
#> 2            104             mlb       nyn            nym          121
#> 3            104             mlb       bos            bos          111
#> 4            104             mlb       mil            mil          158
#> 5            103             mlb       bal            bal          110
#>   home_team_city home_team_name home_division home_league_id
#> 1     Washington      Nationals             E            104
#> 2        NY Mets           Mets             E            104
#> 3         Boston        Red Sox             E            103
#> 4      Milwaukee        Brewers             C            104
#> 5      Baltimore        Orioles             E            103
#>   home_sport_code day gameday_sw double_header_sw game_nbr tbd_flag
#> 1             mlb MON          P                N        1        N
#> 2             mlb MON          P                N        1        N
#> 3             mlb MON          P                N        1        N
#> 4             mlb MON          P                N        1        N
#> 5             mlb MON          P                N        1        N
#>   away_games_back home_games_back away_games_back_wildcard
#> 1               -               -                        -
#> 2             1.5             0.5                      1.5
#> 3             1.5             1.5                      1.0
#> 4               -             1.0                     <NA>
#> 5             6.5               -                      5.0
#>   home_games_back_wildcard venue_w_chan_loc       location
#> 1                        -         USDC0001 Washington, DC
#> 2                      0.5         USNY0504   Flushing, NY
#> 3                        -         USMA0046     Boston, MA
#> 4                      0.5         USWI0455  Milwaukee, WI
#> 5                     <NA>         USMD0018  Baltimore, MD
#>                 gameday_link away_win away_loss home_win home_loss
#> 1 2017_04_03_miamlb_wasmlb_1        0         1        1         0
#> 2 2017_04_03_atlmlb_nynmlb_1        0         1        1         0
#> 3 2017_04_03_pitmlb_bosmlb_1        0         1        1         0
#> 4 2017_04_03_colmlb_milmlb_1        1         0        0         1
#> 5 2017_04_03_tormlb_balmlb_1        0         1        1         0
#>                                                             game_data_directory
#> 1 /components/game/mlb/year_2017/month_04/day_03/gid_2017_04_03_miamlb_wasmlb_1
#> 2 /components/game/mlb/year_2017/month_04/day_03/gid_2017_04_03_atlmlb_nynmlb_1
#> 3 /components/game/mlb/year_2017/month_04/day_03/gid_2017_04_03_pitmlb_bosmlb_1
#> 4 /components/game/mlb/year_2017/month_04/day_03/gid_2017_04_03_colmlb_milmlb_1
#> 5 /components/game/mlb/year_2017/month_04/day_03/gid_2017_04_03_tormlb_balmlb_1
#>   league top_inning inning_state                             note status
#> 1     NN          Y                                                Final
#> 2     NN          Y                                                Final
#> 3     NA          Y                                                Final
#> 4     NN          N                                                Final
#> 5     AA          N              Two out when winning run scored.  Final
#>   ind is_perfect_game is_no_hitter inning balls strikes outs
#> 1   F               N            N      9     0       0    3
#> 2   F               N            N      9     0       0    3
#> 3   F               N            N      9     0       0    3
#> 4   F               N            N      9     0       0    3
#> 5   F               N            N     11     0       0    2
#>   away_team_runs home_team_runs away_team_hits home_team_hits
#> 1              2              4              6              9
#> 2              0              6              7              7
#> 3              3              5              8             10
#> 4              7              5             10              7
#> 5              2              3             11              9
#>   away_team_errors home_team_errors
#> 1                0                0
#> 2                0                0
#> 3                1                1
#> 4                0                3
#> 5                0                0
#>                                                                wrapup_link
#> 1 /mlb/gameday/index.jsp?gid=2017_04_03_miamlb_wasmlb_1&mode=wrap&c_id=mlb
#> 2 /mlb/gameday/index.jsp?gid=2017_04_03_atlmlb_nynmlb_1&mode=wrap&c_id=mlb
#> 3 /mlb/gameday/index.jsp?gid=2017_04_03_pitmlb_bosmlb_1&mode=wrap&c_id=mlb
#> 4 /mlb/gameday/index.jsp?gid=2017_04_03_colmlb_milmlb_1&mode=wrap&c_id=mlb
#> 5 /mlb/gameday/index.jsp?gid=2017_04_03_tormlb_balmlb_1&mode=wrap&c_id=mlb
#>                                                             home_preview_link
#> 1 /mlb/gameday/index.jsp?gid=2017_04_03_miamlb_wasmlb_1&mode=preview&c_id=mlb
#> 2 /mlb/gameday/index.jsp?gid=2017_04_03_atlmlb_nynmlb_1&mode=preview&c_id=mlb
#> 3 /mlb/gameday/index.jsp?gid=2017_04_03_pitmlb_bosmlb_1&mode=preview&c_id=mlb
#> 4 /mlb/gameday/index.jsp?gid=2017_04_03_colmlb_milmlb_1&mode=preview&c_id=mlb
#> 5 /mlb/gameday/index.jsp?gid=2017_04_03_tormlb_balmlb_1&mode=preview&c_id=mlb
#>                                                             away_preview_link
#> 1 /mlb/gameday/index.jsp?gid=2017_04_03_miamlb_wasmlb_1&mode=preview&c_id=mlb
#> 2 /mlb/gameday/index.jsp?gid=2017_04_03_atlmlb_nynmlb_1&mode=preview&c_id=mlb
#> 3 /mlb/gameday/index.jsp?gid=2017_04_03_pitmlb_bosmlb_1&mode=preview&c_id=mlb
#> 4 /mlb/gameday/index.jsp?gid=2017_04_03_colmlb_milmlb_1&mode=preview&c_id=mlb
#> 5 /mlb/gameday/index.jsp?gid=2017_04_03_tormlb_balmlb_1&mode=preview&c_id=mlb
#>                                                                       preview
#> 1 /mlb/gameday/index.jsp?gid=2017_04_03_miamlb_wasmlb_1&mode=preview&c_id=mlb
#> 2 /mlb/gameday/index.jsp?gid=2017_04_03_atlmlb_nynmlb_1&mode=preview&c_id=mlb
#> 3 /mlb/gameday/index.jsp?gid=2017_04_03_pitmlb_bosmlb_1&mode=preview&c_id=mlb
#> 4 /mlb/gameday/index.jsp?gid=2017_04_03_colmlb_milmlb_1&mode=preview&c_id=mlb
#> 5 /mlb/gameday/index.jsp?gid=2017_04_03_tormlb_balmlb_1&mode=preview&c_id=mlb
#>                       tv_station
#> 1                   MASN, WUSA 9
#> 2 SNY, ESPN (out-of-market only)
#> 3                           NESN
#> 4                           FSWI
#> 5                    MASN 2, WJZ
#>                                                             home_recap_link
#> 1 /mlb/gameday/index.jsp?gid=2017_04_03_miamlb_wasmlb_1&mode=recap&c_id=mlb
#> 2 /mlb/gameday/index.jsp?gid=2017_04_03_atlmlb_nynmlb_1&mode=recap&c_id=mlb
#> 3 /mlb/gameday/index.jsp?gid=2017_04_03_pitmlb_bosmlb_1&mode=recap&c_id=mlb
#> 4 /mlb/gameday/index.jsp?gid=2017_04_03_colmlb_milmlb_1&mode=recap&c_id=mlb
#> 5 /mlb/gameday/index.jsp?gid=2017_04_03_tormlb_balmlb_1&mode=recap&c_id=mlb
#>                                                             away_recap_link
#> 1 /mlb/gameday/index.jsp?gid=2017_04_03_miamlb_wasmlb_1&mode=recap&c_id=mlb
#> 2 /mlb/gameday/index.jsp?gid=2017_04_03_atlmlb_nynmlb_1&mode=recap&c_id=mlb
#> 3 /mlb/gameday/index.jsp?gid=2017_04_03_pitmlb_bosmlb_1&mode=recap&c_id=mlb
#> 4 /mlb/gameday/index.jsp?gid=2017_04_03_colmlb_milmlb_1&mode=recap&c_id=mlb
#> 5 /mlb/gameday/index.jsp?gid=2017_04_03_tormlb_balmlb_1&mode=recap&c_id=mlb
#>   reason
#> 1   <NA>
#> 2   <NA>
#> 3   <NA>
#> 4   <NA>
#> 5   <NA>
```

Visualization
-------------

The `mlbgameday` package is data-centric and does not provide any built-in visualization tools. However, there are several excellent visualization packages available for the R language. Below is a short example of what can be done with `ggplot2`. For more examples, please see the [package vignettes](https://github.com/keberwein/mlbgameday/tree/master/vignettes).

First, get the data.

``` r
library(mlbgameday)
library(dplyr)

# Grap some Gameday data. We're specifically looking for Jake Arrieta's no-hitter.
gamedat <- get_payload(start = "2016-04-21", end = "2016-04-21")

# Subset that atbat table to only Arrieta's pitches and join it with the pitch table.
pitches <- inner_join(gamedat$pitch, gamedat$atbat, by = c("num", "url")) %>%
    subset(pitcher_name == "Jake Arrieta")
```

``` r
library(ggplot2)

# basic example
ggplot() +
    geom_point(data=pitches, aes(x=px, y=pz, shape=type, col=pitch_type)) +
    coord_equal() + geom_path(aes(x, y), data = mlbgameday::kzone)
```

![](https://github.com/keberwein/keberwein.github.io/blob/master/images/mlbgameday/gamedaysingle.png?raw=true)

``` r
library(ggplot2)

# basic example with stand.
ggplot() +
    geom_point(data=pitches, aes(x=px, y=pz, shape=type, col=pitch_type)) +
    facet_grid(. ~ stand) + coord_equal() +
    geom_path(aes(x, y), data = mlbgameday::kzone)
```

![](https://github.com/keberwein/keberwein.github.io/blob/master/images/mlbgameday/gamedaystand.png?raw=true)

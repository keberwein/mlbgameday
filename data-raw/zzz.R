urlz <- "http://gd2.mlb.com/components/game/mlb/year_2017/month_05/day_11/gid_2017_05_11_minmlb_chamlb_1/"
#file <- xml2::read_xml(urlz)
#
#used
@importFrom dplyr mutate select rename left_join, full_join
@import xml2
@import purrr map_chr map_dfr

## make_gids() is called by get_payload. Probably create a method in make_gids for this that just makes the base gid.
## After that, we can just append the correct suffix here.
## Need to go through each one and make sure we're getting all of Ben's info...especially inning and inning_side.

atbat_urlz <- urlz %>% purrr::map_chr(~ paste0(., "/inning/inning_all.xml"))
file <- xml2::read_xml(atbat_urlz)
atbat_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat"), 
                 xml2::xml_find_all(file, "./inning/bottom/atbat"))

# inning_all/atbat
atbat <- purrr::map_dfr(atbat_nodes, function(x) {
    out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
    out$inning_num <- as.numeric(xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num"))
    out$inning_side <- xml2::xml_name(xml2::xml_parent(x))
    
    out
})

# inning_all/atbat/runner
runner <- purrr::map_dfr(atbat_nodes, function(x) {
    out <- data.frame(t(xml2::xml_attrs(xml2::xml_child(x, "/runner"))), stringsAsFactors=FALSE)
    out$inning_num <- as.numeric(xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num"))
    out$inning_side <- xml2::xml_name(xml2::xml_parent(x))
    out
})

inning_all <- dplyr::left_join(atbat, runner, by = c("event_num", "event", "inning_num", "inning_side", "score")) %>%
    dplyr::select(inning_num, num, start_tfs_zulu, batter, stand, pitcher, p_throws, event, start, end)
# need to coalesce start/end into a column called "runnerMovement"


# game

game_urlz <- urlz %>% purrr::map_chr(~ paste0(., "/game.xml"))
file <- xml2::read_xml(game_urlz)

game_nodes <- xml2::xml_find_all(file, "/game/team")
ven_nodes <- xml2::xml_find_all(file, "/game/stadium")

game <- purrr::map_dfr(game_nodes, function(x) {
    out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
    out$venueId <- as.numeric(xml2::xml_attr(ven_nodes[1], "id"))
    out$stadium <- xml2::xml_attr(ven_nodes[1], "name")
    out$game_type <- xml2::xml_parent(x) %>% xml2::xml_attr("type")
    out}) %>% mutate(home_team = ifelse(type=="home", name_full, NA), away_team = ifelse(type=="away", name_full, NA),
           home_teamId = ifelse(type=="home", id, NA), away_teamId = ifelse(type=="away", id, NA),
           home_lg = ifelse(type=="home", league, NA), away_lg = ifelse(type=="away", league, NA)) %>%
    dplyr::select(game_type, home_team, home_teamId, home_lg, away_team, away_teamId, away_lg, venueId, stadium)

# Need to figure out a way to coalece both thise rows into one row.



# game_events
# 
gamevents_urlz <- urlz %>% purrr::map_chr(~ paste0(., "/game_events.xml"))
file <- xml2::read_xml(gamevents_urlz)
# game_events/atbat
eventab_nodes <- c(xml2::xml_find_all(file, "/game/inning/top/atbat"), 
                 xml2::xml_find_all(file, "/game/inning/bottom/atbat"))
events_ab <- purrr::map_dfr(eventab_nodes, function(x) {
    out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
    out$inning_num <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
    out$inning_side <- xml2::xml_name(xml2::xml_parent(x))
    out}) %>% dplyr::select(inning_num, inning_side, b, s, o, start_tfs_zulu, event, pitcher, des) %>%
    dplyr::rename(inning=inning_num, half=inning_side, balls=b, strikes=s, endOuts=o, timestamp=start_tfs_zulu, actionId=pitcher, description=des)

# game_events/action
eventact_nodes <- c(xml2::xml_find_all(file, "/game/inning/top/action"), 
                   xml2::xml_find_all(file, "/game/inning/bottom/action")) 
events_act <- purrr::map_dfr(eventact_nodes, function(x) {
    out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
    out$inning_num <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num")
    out$inning_side <- xml2::xml_name(xml2::xml_parent(x))
    out}) %>% dplyr::select(inning_num, inning_side, b, s, o, tfs_zulu, event, player, des) %>%
    dplyr::rename(inning=inning_num, half=inning_side, balls=b, strikes=s, endOuts=o, timestamp=tfs_zulu, actionId=player, description=des)

game_events <- dplyr::full_join(events_ab, events_act, 
                                by = c("inning", "half", "balls", "strikes", "endOuts", "timestamp", "event", "actionId", "description"))


# inning_hit
hit_urlz <- urlz %>% purrr::map_chr(~ paste0(., "/inning/inning_hit.xml"))
file <- xml2::read_xml(hit_urlz)
hip_nodes <- xml2::xml_find_all(file, "/hitchart/hip")
inning_hit <- purrr::map_dfr(hip_nodes, function(x) {
    out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
    out
}) %>% dplyr::select(inning, type, batter, pitcher, x, y, event) %>%
    dplyr::rename(batterid=batter, pitcherId=pitcher)



# bis_boxscore_batting
bis_urlz <- urlz %>% purrr::map_chr(~ paste0(., "/bis_boxscore.xml"))
file <- xml2::read_xml(bis_urlz)
bat_nodes <- xml2::xml_find_all(file, "/boxscore/batting/batter")

bis_batting <- purrr::map_dfr(bat_nodes, function(x) {
        out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
        out$team_flag <- xml2::xml_parent(x) %>% xml2::xml_attr("team_flag")
        out$home_id <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("home_id")
        out$away_id <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("away_id")
        out
    }) %>% dplyr::mutate(teamId = ifelse(team_flag=="home", home_id, away_id)) %>%
    dplyr::select(teamId, id, name, pos, bo) %>% dplyr::rename(playerId=id, playerName=name)

# bis_boxscore_pitching
pitch_nodes <- xml2::xml_find_all(file, "/boxscore/pitching/pitcher")
bis_pitching <- purrr::map_dfr(pitch_nodes, function(x) {
        out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
        out$team_flag <- xml2::xml_parent(x) %>% xml2::xml_attr("team_flag")
        out$home_id <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("home_id")
        out$away_id <- xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("away_id")
        out
    }) %>% dplyr::mutate(teamId = ifelse(team_flag=="home", home_id, away_id)) %>%
    dplyr::select(teamId, id, name, pos) %>% dplyr::rename(playerId=id, playerName=name)

bis_boxscore <- dplyr::full_join(bis_batting, bis_pitching, by = c("teamId", "playerId", "playerName", "pos"))






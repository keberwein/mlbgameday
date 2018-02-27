#' Method for payload objects.
#' @param payload_obj An object returned by \code{get_payload()}.
#' @param ... additional arguments
#' @keywords internal
#' @export
#' 

transform_pload <- function(payload_obj, ...) UseMethod("transform_pload", payload_obj)

#' @rdname transform_pload
#' @importFrom dplyr mutate select
#' @method transform_pload list_bis_boxscore
#' @export

transform_pload.list_bis_boxscore <- function(payload_obj, ...) {
    payload_obj$batting %<>% dplyr::mutate(id=as.numeric(id), bo=as.numeric(bo), ab=as.numeric(ab), po=as.numeric(po),
                                           r=as.numeric(r), a=as.numeric(a), bb=as.numeric(bb), sac=as.numeric(sac),
                                           t=as.numeric(t), sf=as.numeric(sf), h=as.numeric(h), e=as.numeric(e),
                                           d=as.numeric(d), hbp=as.numeric(hbp), so=as.numeric(so), hr=as.numeric(hr),
                                           rbi=as.numeric(rbi), lob=as.numeric(lob), fldg=as.double(fldg), avg=as.double(avg),
                                           go=as.numeric(go), ao=as.numeric(ao), gidp=as.numeric(gidp))
    
    payload_obj$pitching %<>% dplyr::mutate(id=as.numeric(id), out=as.numeric(out), bf=as.numeric(bf), er=as.numeric(er),
                                            r=as.numeric(r), h=as.numeric(h), so=as.numeric(so), hr=as.numeric(hr),
                                            bb=as.numeric(bb), np=as.numeric(np), s=as.numeric(s), w=as.numeric(w),
                                            era=as.double(era))
    
    return(payload_obj)
}

#' @rdname transform_pload
#' @importFrom dplyr mutate
#' @method transform_pload df_game_events
#' @export

transform_pload.df_game_events <- function(payload_obj, ...) {
    # Hack to get dplyr to mutate a generic object class.
    payload_obj <- structure(payload_obj, class="data.frame")
    
    payload_obj %<>% dplyr::mutate(start_speed=as.double(start_speed), num=as.numeric(num),
                                   b=as.numeric(b), s=as.numeric(s), o=as.numeric(o), start_tfs=as.numeric(start_tfs),
                                   batter=as.numeric(batter), pitcher=as.numeric(pitcher), event_num=as.numeric(event_num),
                                   home_team_runs=as.numeric(home_team_runs), away_team_runs=as.numeric(away_team_runs),
                                   inning=as.numeric(inning))
    # Revert object to the old class for the load functions.
    return(payload_obj)
}

#' @rdname transform_pload
#' @importFrom dplyr mutate
#' @method transform_pload df_inning_hit
#' @export

transform_pload.df_inning_hit <- function(payload_obj, ...) {
    # Hack to get dplyr to mutate a generic object class.
    payload_obj <- structure(payload_obj, class="data.frame")
    payload_obj %<>% dplyr::mutate(x=as.double(x), y=as.double(y), batter=as.numeric(batter), pitcher=as.numeric(pitcher),
                                   inning=as.numeric(inning))
    
    return(payload_obj)
}


#' @rdname transform_pload
#' @importFrom dplyr mutate rename
#' @method transform_pload list_inning_all
#' @export

transform_pload.list_inning_all <- function(payload_obj, ...) {
    payload_obj$atbat %<>%
        # Data prior to 2015 is missing several fields. Add those as null so the database is consistant.
        dplyr::mutate(play_guid = if (exists('play_guid', where = payload_obj$atbat)) play_guid else NA,
                      event2 = if (exists('event2', where = payload_obj$atbat)) event2 else NA,
                      event2_es = if (exists('event2_es', where = payload_obj$atbat)) event2_es else NA,
                      #event3 = if (exists('event3', where = payload_obj$atbat)) event3 else NA,
                      #event3_es = if (exists('event3_es', where = payload_obj$atbat)) event3_es else NA,
                      des_es = if (exists('des_es', where = payload_obj$atbat)) des_es else NA) %>%
        
        dplyr::mutate(num=as.numeric(num), b=as.numeric(b), s=as.numeric(s), o=as.numeric(o),
                      batter=as.numeric(batter), pitcher=as.numeric(pitcher), date=as.factor(date)) %>%
        # Rename a couple columns to fit with the pitchRx schema.
        dplyr::rename(atbat_des = des, atbat_des_es = des_es) %>%
        
        # Column order gets crossed up in some cases, which makes it difficult to "chunk" into a database. Order manually for now.
        dplyr::select(pitcher, batter, num, b, s, o, start_tfs, start_tfs_zulu, end_tfs_zulu, stand, b_height, p_throws, atbat_des, 
                      atbat_des_es, event_num, event, event_es, play_guid, home_team_runs, away_team_runs, url, inning_side, 
                      inning, next_, score, event2, event2_es, batter_name, pitcher_name, gameday_link, date)
    
    payload_obj$action %<>% 
        # Add columns that may not exist.
        dplyr::mutate(play_guid = if (exists('play_guid', where = payload_obj$action)) play_guid else NA,
                      event2 = if (exists('event2', where = payload_obj$action)) event2 else NA,
                      event2_es = if (exists('event2_es', where = payload_obj$action)) event2_es else NA,
                      des_es = if (exists('des_es', where = payload_obj$action)) des_es else NA,
                      score = if (exists('score', where = payload_obj$action)) score else NA) %>%
        
        dplyr::mutate(b=as.numeric(b), s=as.numeric(s), o=as.numeric(o), player=as.numeric(player), pitch=as.numeric(pitch),
                      num=as.character(num)) %>%
    
    dplyr::select(b, s, o, des, des_es, event, event_es, tfs, tfs_zulu, player, pitch, event_num, play_guid, home_team_runs,
        away_team_runs, url, inning_side, inning, next_, num, score, event2, event2_es, gameday_link)
    
    payload_obj$pitch %<>%
        # Add columns that may not exist.
        dplyr::mutate(play_guid = if (exists('play_guid', where = payload_obj$pitch)) play_guid else NA,
                      des_es = if (exists('des_es', where = payload_obj$pitch)) des_es else NA,
                      event2 = if (exists('event2', where = payload_obj$pitch)) event2 else NA,
                      event2_es = if (exists('event2_es', where = payload_obj$pitch)) event2_es else NA,
                      # tfs and tfs_zulu columns may be blank for older data sets. If blank, set them to NA.
                      tfs = ifelse(tfs == "", NA, tfs), tfs_zulu = ifelse(tfs_zulu == "", NA, tfs_zulu)) %>%
        
        dplyr::mutate(id=as.numeric(id), x=as.numeric(x), y=as.numeric(y), start_speed=as.numeric(start_speed), 
                      end_speed=as.numeric(end_speed), sz_top=as.numeric(sz_top), sz_bot=as.numeric(sz_bot),
                      pfx_x=as.numeric(pfx_x), pfx_z=as.numeric(pfx_z), px=as.numeric(px), pz=as.numeric(pz),
                      x0=as.numeric(x0), y0=as.numeric(y0), z0=as.numeric(z0), vx0=as.numeric(vx0), 
                      vy0=as.numeric(vy0), vz0=as.numeric(vz0), ax=as.numeric(ax), ay=as.numeric(ay),
                      zone=as.numeric(zone), break_length=as.numeric(break_length), type_confidence=as.numeric(type_confidence),
                      nasty=as.numeric(nasty), spin_dir=as.numeric(spin_dir), spin_rate=as.numeric(spin_rate),
                      on_1b=as.numeric(on_1b), on_2b=as.numeric(on_2b), on_3b=as.numeric(on_3b), count=as.factor(count)) %>%
        
        dplyr::select(des, des_es, id, type, code, tfs, tfs_zulu, x, y, event_num, sv_id, play_guid, start_speed, end_speed, sz_top,         
                      sz_bot, pfx_x, pfx_z, px, pz, x0, y0, z0, vx0, vy0, vz0, ax, ay, az, break_y, break_angle, break_length, pitch_type, 
                      type_confidence, zone, nasty, spin_dir, spin_rate, cc, mt, url, inning_side, inning, next_, num, on_1b, on_2b,
                      on_3b, gameday_link, count)
    
    payload_obj$runner %<>% dplyr::mutate(id=as.numeric(id), num=as.numeric(num)) %>%
        dplyr::select(id, start, end, event, event_num, url, inning_side, inning, next_, num, score, rbi, earned, gameday_link)
    
    payload_obj$po %<>% 
        # Add columns that may not exist.
        dplyr::mutate(play_guid = if (exists('play_guid', where = payload_obj$po)) play_guid else NA,
                      catcher = if (exists('catcher', where = payload_obj$po)) catcher else NA,
                      des_es = if (exists('des_es', where = payload_obj$po)) des_es else NA,
                      event_num = if (exists('event_num', where = payload_obj$po)) event_num else NA) %>%
        
        dplyr::select(des, des_es, event_num, play_guid, url, inning_side, inning, next_, num, catcher, gameday_link)
    
    return(payload_obj)
}

#' @rdname transform_pload
#' @importFrom dplyr mutate
#' @method transform_pload list_linescore
#' @export

transform_pload.list_linescore <- function(payload_obj, ...) {
    # TO DO: Need to convert the date/time fields to a 24-hour clock and to a date/time data format.
    # The way they are now are going to mess up any database we try to load them in to.
    
    payload_obj$game %<>% dplyr::mutate(game_pk=as.numeric(game_pk), original_date=as.Date(original_date, format="Y/m/d"),
                                        venue_id=as.numeric(venue_id), scheduled_innings=as.numeric(scheduled_innings),
                                        away_team_id=as.numeric(away_team_id), away_league_id=as.numeric(away_league_id),
                                        home_team_id=as.numeric(home_team_id), home_league_id=as.numeric(home_league_id),
                                        game_nbr=as.numeric(game_nbr), away_win=as.numeric(away_win), away_loss=as.numeric(away_loss),
                                        home_win=as.numeric(home_win), home_loss=as.numeric(home_loss),
                                        inning=as.numeric(inning), balls=as.numeric(balls), strikes=as.numeric(strikes),
                                        outs=as.numeric(outs), away_team_runs=as.numeric(away_team_runs), home_team_runs=as.numeric(home_team_runs),
                                        away_team_hits=as.numeric(away_team_hits), home_team_hits=as.numeric(home_team_hits), 
                                        away_team_errors=as.numeric(away_team_errors), home_team_errors=as.numeric(home_team_errors))
    return(payload_obj)
}


#' @rdname transform_pload
#' @importFrom dplyr mutate
#' @method transform_pload df_game
#' @export

transform_pload.df_game <- function(payload_obj, ...) {
    payload_obj <- structure(payload_obj, class="data.frame")
    
    payload_obj %<>% dplyr::mutate(id=as.numeric(id), w=as.numeric(w), l=as.numeric(l), league_id=as.numeric(league_id),
                                   game_pk=as.numeric(game_pk))
    
    return(payload_obj)
}


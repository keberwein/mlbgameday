#' An internal function to set data types to various paylaod types.
#' @param dataset The data to be transformed.
#' @param convert The playload type to be transformed.
#' @param ... additional arguments
#' @keywords internal
#' @export


etl_payload <- function(dataset = NULL, convert = NULL, ...) {
    if(convert == "inning_all"){
        dataset$atbat %<>% dplyr::mutate(num=as.numeric(num), b=as.numeric(b), s=as.numeric(s), o=as.numeric(o),
                                            start_tfs=as.numeric(start_tfs), batter=as.numeric(batter), pitcher=as.numeric(pitcher),
                                            event_num=as.numeric(event_num), home_team_runs=as.numeric(home_team_runs),
                                            away_team_runs=as.numeric(away_team_runs))
        
        dataset$action %<>% dplyr::mutate(b=as.numeric(b), s=as.numeric(s), o=as.numeric(o),
                                             tfs=as.numeric(tfs), player=as.numeric(player), pitch=as.numeric(pitch),
                                             event_num=as.numeric(event_num), home_team_runs=as.numeric(home_team_runs),
                                             away_team_runs=as.numeric(away_team_runs))
        
        dataset$pitch %<>% dplyr::mutate(id=as.numeric(id), tfs=as.numeric(tfs), x=as.numeric(x), y=as.numeric(y),
                                            event_num=as.numeric(event_num), start_speed=as.numeric(start_speed), 
                                            end_speed=as.numeric(end_speed), sz_top=as.numeric(sz_top), sz_bot=as.numeric(sz_bot),
                                            pfx_x=as.numeric(pfx_x), pfx_z=as.numeric(pfx_z), px=as.numeric(px), pz=as.numeric(pz),
                                            x0=as.numeric(x0), y0=as.numeric(y0), z0=as.numeric(z0), vx0=as.numeric(vx0), 
                                            vy0=as.numeric(vy0), vz0=as.numeric(vz0), ax=as.numeric(ax), ay=as.numeric(ay),
                                            az=as.numeric(az), break_y=as.numeric(break_y), break_angle=as.numeric(break_angle),
                                            break_length=as.numeric(break_length), type_confidence=as.numeric(type_confidence),
                                            nasty=as.numeric(nasty), spin_dir=as.numeric(spin_dir), spin_rate=as.numeric(spin_rate),
                                            on_1b=as.numeric(on_1b), on_2b=as.numeric(on_2b), on_3b=as.numeric(on_3b))
        
        dataset$runner %<>% dplyr::mutate(id=as.numeric(id), event_num=as.numeric(event_num))
        
        dataset$po %<>% dplyr::mutate(event_num=as.numeric(event_num))
    }
    
    return(dataset)
}

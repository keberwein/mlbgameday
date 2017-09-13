urlz <- "http://gd2.mlb.com/components/game/mlb/year_2016/month_09/day_29/gid_2016_09_29_bosmlb_nyamlb_1/inning/inning_all.xml"
file <- read_xml(urlz)

#pitch_nodes <- xml2::xml_find_all(file, "//inning/top/atbat/pitch")
#atbat_nodes <- xml2::xml_find_all(file, "//inning/top/atbat")
#action_nodes <- xml2::xml_find_all(file, "//inning/top/action")
#runner_nodes <- xml2::xml_find_all(file, "//inning/top/runner")
#po_nodes <- xml2::xml_find_all(file, "//inning/top/po")

# Try breaking pitch and atbat into top and bottom loops. Might save a copule of seconds.
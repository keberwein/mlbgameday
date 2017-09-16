library(tidygameday)
library(tidyverse)


# Grap some Gameday data. We're specifically looking for Jake Arrieta's no-hitter.
gamedat <- get_payload(start = "2016-04-21", end = "2016-04-21")

# Subset that atbat table to only Arrieta's pitches and join it with the pitch table.
pitches <- inner_join(gamedat$pitch, gamedat$atbat, by = c("num", "url")) %>%
    subset(pitcher_name == "Jake Arrieta")

# Convert relevant data to numeric.
pitches <- mutate(pitches, px = as.numeric(px), pz = as.numeric(pz), start_speed = as.numeric(start_speed))

library(plotly)
plot_ly(pitches, x = ~px, y = ~pz, mode = 'markers', 
        color = ~pitch_type, marker = list(size = 7, line = list(color = ~pitch_type, width = 1)),
        text = ~paste("Pitch Type: ", pitch_type)) %>% 
    layout(title = "Anatomy of a No-Hitter", xaxis = list(zeroline=F), yaxis = list(zeroline=F)) %>%
    # Draw a box for the strikezone. 
    layout(shapes = list(type = "rect", fillcolor = "blue", 
                         line = list(color = "blue"), opacity = 0.2,
                         x0 = -0.95, x1 = 0.95, xref = "x",
                         y0 = 3.5, y1 = 1.6, yref = "y"))


library(pitchRx)
library(ggplot2)
pitchRx::strikeFX(pitches, geom = "tile") + 
    facet_grid(pitcher_name ~ stand) +
    coord_equal() +
    theme_bw() +
    viridis::scale_fill_viridis()

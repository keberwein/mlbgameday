# Strikezone Boundreis borrowed from the book "Analyzing Baseball Data with R."

topKzone <- 3.5
botKzone <- 1.6
inKzone <- -.95
outKzone <- 0.95
kzone = data.frame(
    x = c(inKzone, inKzone, outKzone, outKzone, inKzone)
    , y = c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

devtools::use_data(kzone, overwrite = TRUE)

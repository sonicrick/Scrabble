streakCount <- function(x, streakval) {
  series <- rle(x)
  return(max(series$lengths[series$values==streakval]))
}

lastStreak <- function(x) {
  series <- rle(x)
  return(series$lengths[length(series$lengths)])
}
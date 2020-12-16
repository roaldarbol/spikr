f2s <- function(frame){
  time <- frame * (1/input$rate)
  return(time)
}

localMaxima <- function(x) which(x - data.table::shift(x, 1) > 0  & x - data.table::shift(x, 1, type='lead') > 0)
localMinima <- function(x) which(x - data.table::shift(x, 1) < 0  & x - data.table::shift(x, 1, type='lead') < 0)

smaller <- function(x, value) which.min(abs(value - replace(x, x>value, Inf)))
bigger <- function(x, value) which.max(abs(value - replace(x, x>value, Inf)))
closest <- function(x, value){
  x[which(abs(x - value) == min(abs(x - value)))] 
}

rollsd <- function(x, k){
  zoo::rollapply(x, width = floor(k), FUN = sd, na.rm = TRUE)
}

# Alternative solutions ----

# localMaxima <- function(x) {
#   # Use -Inf instead if x is numeric (non-integer)
#   y <- diff(c(-Inf, x)) > 0L
#   rle(y)$lengths
#   y <- cumsum(rle(y)$lengths)
#   y <- y[seq.int(1L, length(y), 2L)]
#   if (x[[1]] == x[[2]]) {
#     y <- y[-1]
#   }
#   return(y)
# }

# localMinima <- function(x) {
#   # Use -Inf instead if x is numeric (non-integer)
#   y <- diff(c(.Machine$integer.max, x)) > 0L
#   rle(y)$lengths
#   y <- cumsum(rle(y)$lengths)
#   y <- y[seq.int(1L, length(y), 2L)]
#   if (x[[1]] == x[[2]]) {
#     y <- y[-1]
#   }
#   y
# }
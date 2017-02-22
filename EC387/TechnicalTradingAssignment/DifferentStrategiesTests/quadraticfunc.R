#quadratic function to check if a point in is a plot y=x^2+1
quad <- function(x, y){
  l1 <- y >= x^2 + 1  #in the quadratic function , anything over the line is in the area, thus it should return true
  return(l1)
}

#quad function y=x^2-8x+12
quad <- function(x, y){
  l1 <- y >= x^2 - 8*x + 12  #in the quadratic function , anything over the line is in the area, thus it should return true
  return(l1)
}

#for y=x^3-7
quad <- function(x, y){
  l1 <- y >= x^3 - 7  #in the quadratic function , anything over the line is in the area, thus it should return true
  return(l1)
}

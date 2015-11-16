midpoint <- function(x1,y1,x2,y2,ratio1=1,ratio2=1){
  x <- ((ratio2*x1) + (ratio1*x2))/(ratio1+ratio2)
  y <- ((ratio2*y1) + (ratio1*y2))/(ratio1+ratio2)
  return(c(x,y))
}
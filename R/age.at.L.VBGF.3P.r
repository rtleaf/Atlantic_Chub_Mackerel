# Calculate age at length from VBGF given 3P 
# Robert Leaf, April 7, 2020

age.at.L.VBGF.3P <- function(Linf = 37.13, k = 0.41, t0 = -2.44, 
                             Length.in = c()) {
  
  Length.out <- t0 + log(-Length.in/Linf + 1)/(-k)  
  return(Length.out)
}

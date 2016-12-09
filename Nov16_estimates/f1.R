# practice function for lesson use

f1 <- function(pal)
{
  output <- read.csv(file=pal, header=TRUE, sep=",")
  output <- as.data.frame(output)
  return(output)
}


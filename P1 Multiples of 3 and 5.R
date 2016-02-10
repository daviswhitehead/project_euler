y <- 1
z <- 999
sum <- 0

for (i in z:y){
  if (i%%3==0){
    sum = sum + i
  }
  else if (i%%5==0){
    sum = sum + i
  }
}
sum
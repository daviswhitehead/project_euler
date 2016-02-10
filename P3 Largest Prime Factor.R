#The prime factors of 13195 are 5, 7, 13 and 29.
#
#What is the largest prime factor of the number 600851475143 ?

z <- 600851475143 # number of interest
num <- z
i <- 1
factors <- 0

while(i < num){
  if(z%%i!=0){
    num <- round(z/i)
  }
  else if(z%%i==0){
    factors <- append(factors, i, after = length(factors))
    num <- z/i
  }
  i = i+1
}

done <- F
done2 <- F
prime <- 0
for(i in length(factors):3){
  if(done2==T){
    break
  }
  else if(done2==F){
    for(j in 3:(i-1)){
      if(done==T){
        break
      }
      else if(factors[i]%%factors[j]==0){
        done = T
        break
      }
      else{
        prime <- factors[i]
        done2 <- T
      }
    }
    done = F
  }
}
prime
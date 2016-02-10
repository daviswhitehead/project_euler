# The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
# 
# Find the sum of all the primes below two million.

### Solution works but is super slow

limit <- 2000000
#limit <- 1000
(primes <- c(2, 3, 5, 7))
count <- 5

for(i in 7:limit){
  IS.prime = T
  if((i%%2==0)|(i%%3==0)|(i%%5==0)){
    IS.prime = F
  }
  if(IS.prime==T){
    done <- F
    k <- 4
    while(done==F & k<=(count-1)){
      if(i%%primes[k]==0){
        IS.prime = F
        done = T
      }
      k = k+1
    }
  }
  if(IS.prime==T){
    primes[count] = i
    count = count + 1
  }
}
#primes
length(primes)
sum <- 0
for(i in 1:length(primes)){
  sum = sum + primes[i]
}
sum


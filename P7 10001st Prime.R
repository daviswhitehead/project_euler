# By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, 
# we can see that the 6th prime is 13.
# 
# What is the 10001st prime number?
# http://projecteuler.net/problem=7

### DNF

(primes <- c(2,3,5,7,11,13,17,19,23,29))
length(primes)
num <- c(1:10)
df <- data.frame(num,primes)

prime <- c(rep("A",10001))
z <- 1
i <- 2
while(z <= 100){
  p = T
  for(k in 2:i/2){
    if(i%%k==0 & i!=k){
      p = F
    }
  }
  if(p != F){
    prime[z] <- i
    z = z + 1
  }
  i = i + 1
}
z
head(prime)
prime[10001]






for(i in 1:10000){
  p = T
  for(k in 2:i/2){
    if(i%%k==0 & i!=k){
      p = F
    }
  }
  if(p != F){
    prime[z] <- i
    z = z + 1
  }
}  
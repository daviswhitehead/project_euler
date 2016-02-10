# 2520 is the smallest number that can be divided by each of the numbers
# from 1 to 10 without any remainder.

# What is the smallest positive number that is evenly divisible by all of
# the numbers from 1 to 20?

# http://projecteuler.net/problem=5

x <- 20
i <- 19
product <- 0
done <- F

while(done!=T){
  product = x*i
  if(FUN.divisible(x,product)==T){
    done = T
    print(product)
  }
  else{
    i = i + 19
  }
}

FUN.divisible <- function(x,num){
  if(num%%x==0){
    if(x==1){
      return(T)
    }
    else{
      FUN.divisible(x-1,num)
    }
  }
  else{
    return(F)
  }
}

#A palindromic number reads the same both ways. 
#The largest palindrome made from the product of two 2-digit
#numbers is 9009 = 91 × 99.

#Find the largest palindrome made from the product of two 
#3-digit numbers.

# http://projecteuler.net/problem=4

palindrome <- function(number){ # checks whether a number is a palindrome
  LIST.number <- strsplit(toString(number), split = "")
  LEN.number <- length(LIST.number[[1]])
  check <- TRUE
  for(i in 1:LEN.number){
    if(LIST.number[[1]][i]!=LIST.number[[1]][(LEN.number+1)-i]){
      check <- FALSE
      break
    }
  }
  return(check)
}
x <- 0
y <- 0
num <- 0
palin <- 0

for(x in 836:999){
  for(y in 836:999){
    num = x*y
    if(palindrome(num)==T){
      if(num>palin){
       palin <- x*y 
      }
    }
  }
}


test <- 836*836
palindrome(test)

x <- 1000
y <- 1000
xProduct <- 0

while(xProduct==0){
  y = 999
  x = x - 1
  if(palindrome(x*y)==T){
    xProduct = (x*y)
  }
}

x <- 1000
y <- 1000
yProduct <- 0

while(yProduct==0){
  x = 999
  y = y - 1
  if(palindrome(x*y)==T){
    yProduct = (x*y)
  }
}

x <- 1000
y <- 1000
xyProduct <- 0

while(xyProduct==0){
  x = x - 1
  if(palindrome(x*y)==T){
    xyProduct = (x*y)
  }
  y = y - 1
  if(palindrome(x*y)==T){
    xyProduct = (x*y)
  }
}



vector <- c(rep("A",1000))
z = 1
for(i in (999*999):(900*888)){
  if(palindrome(i)==T){
    vector[z] = i
    z = z+1
  }
}
head(vector)


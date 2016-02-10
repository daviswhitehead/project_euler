# The sum of the squares of the first ten natural numbers is,
# 
# 1^2 + 2^2 + ... + 10^2 = 385
# The square of the sum of the first ten natural numbers is,
# 
# (1 + 2 + ... + 10)^2 = 552 = 3025
# Hence the difference between the sum of the squares of the 
# first ten natural numbers and the square of the sum is 3025 − 385 = 2640.
# 
# Find the difference between the sum of the squares of the
# first one hundred natural numbers and the square of the sum.

cases <- c(1:10)
SUM.squares <- c(1:10)
SQR.sums <- c(1:10)
difference <- c(1:10)
df <- data.frame(cases,SUM.squares,SQR.sums,difference)

for(i in 2:10){
  df$SUM.squares[i] = ((i^2) + df$SUM.squares[i-1])
}

sum <- 0
for(i in 1:10){
  sum = (sum + i)
  df$SQR.sums[i] = ((sum)^2)
}

for(i in 1:10){
  df$difference[i] = (df$SQR.sums[i] - df$SUM.squares[i])
}

for(i in 1:10){
  df$sqrt[i] = sqrt(df$SQR.sums[i])
}

for(i in 2:10){
  df$DIFF.squares[i] = (df$SUM.squares[i] - df$SUM.squares[i-1])
}

for(i in 1:10){
  df$DIV.difference[i] = (df$difference[i] / df$cases[i])
}

SUM.square <- 1
for(i in 2:100){
  SUM.square = ((i^2) + SUM.square)
}

sum <- 0
for(i in 1:100){
  sum = (sum + i)
  SQR.sum = ((sum)^2)
}
SQR.sum-SUM.square

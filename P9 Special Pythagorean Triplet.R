# A Pythagorean triplet is a set of three natural numbers, 
# a < b < c, for which,
# 
# a^2 + b^2 = c^2
# For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
# 
# There exists exactly one Pythagorean triplet for which
# a + b + c = 1000.
# Find the product abc.


# a + b + c = 1000 is also equal to c = 1000 - a - b, where a < b.
# we also know that a^2 + b^2 = c^2.
# thus, sqrt(a^2 + b^2) = 1000 - a - b = 1000 - (a + b)
# squaring : a^2 + b^2 = 1000000 - 2000(a + b) + a^2 + 2ab + b^2
# remove a^2 and b^2: 2000(a + b) = 1000000 + 2ab
# 0 = 1000000 + 2ab - 2000a - 2000b where a < b

sum <- 2000
(LIMIT.a <- round(((sum/3)-1)))
(LIMIT.b <- round(((sum/2)-1)))

solved <- F
while(solved==F){
  for(a in LIMIT.a:1){ # a can't be more than 332
    for(b in (a+1):LIMIT.b){ # b can't be more than 499
      c = sum - a - b
      if((c^2)==((a^2)+(b^2))){
        solved = T
        product = a*b*c
        string <- paste(a,b,c, collapse = " ")
      }
    }
  }
}
string
product

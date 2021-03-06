#Each new term in the Fibonacci sequence is 
#generated by adding the previous two terms. 
#By starting with 1 and 2, the first 10 terms will be:
#  
#  1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
#
#By considering the terms in the Fibonacci sequence 
#whose values do not exceed four million, find the sum 
#of the even-valued terms.

z <- 4000000 # limit is 4 million
sum <- 0
i <- 1
n <- 0

while(i <= z){
 f = i + n
 n = i
 i = f
 #print(f)
 if(f%%2==0){
   sum = sum + f
 }
}
sum
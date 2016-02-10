# In the 20×20 grid below, 
# four numbers along a diagonal line have been marked in red.
# 
# What is the greatest product of four adjacent numbers in the same
# direction (up, down, left, right, or diagonally) in the 20×20 grid?

(z <- "08022297381500400075040507785212507791084949994017811857608717409843694804566200814931735579142993714067538830034913366552709523046011426924685601325671370236912231167151676389419236542240402866331380244732609903450244753353783684203517125032988128642367102638406759547066183864706726206802621220956394396308409166499421245558056673992697177878968314883489637221362309750076442045351400613397343133957817532822753167159403800462161409535692163905429635314755588824001754243629855786560048357189070544443744602158515417581980816805944769287392138652177704895540045208839735991607975732162626793327986688366887576220720346336746551232639353690442167338253911249472180846293240627636206936417230238834629969826759857404361620733529783190017431497148868116235705540170547183515469169233486143520189196748")

FUN.transform <- function(grid){
  (idx <- 1:nchar(grid))
  (odds <- idx[(idx %% 2) == 1])
  (evens <- idx[(idx %% 2) == 0])
  (grid.pairs <- substring(grid, odds, evens))
  (grid.pairs <- as.integer(grid.pairs))
  return(grid.pairs)
}

FUN.left.right <- function(grid, width, height, chain){ # grid var as string where each number has two digits (eg. '1' -> '01')
  LEN.grid <- length(grid)
  
  if(width < chain){ # change width here for diagonal functions
    break
  }
  else if(LEN.grid != width*height){
    break
  }
  else{ # left-right specific
    old.product <- 0
    old.string <- "blank"
    old.position <- "blank"
    i <- 1
    while(i < LEN.grid){
      new.product <- 1
      string <- "Numbers:"
      position <- "Positions:"
      k <- i
      for(k in i:(i+chain-1)){
        a <- grid[k]
        new.product = new.product*a
        string = paste(string, a, sep=" ")
        position = paste(position, k, sep=" ")
      }
      if(new.product > old.product){
        old.product <- new.product
        old.string <- string
        old.position <- position
      }
      if((i+chain-1)%%width==0){
        i = i+chain
      }
      else{
        i=i+1
      }
    }  
  }
  print(old.product)
  print(old.string)
  print(old.position)
} 

test <- FUN.transform("01020304050607080910111213141516171819200440190028291714050600192827399999999999")
length(test)
FUN.left.right(test,8,5,7)

zx <- FUN.transform(z)
FUN.left.right(zx,20,20,4)

FUN.left.right.diag.1 <- function(grid, width, height, chain){ # grid var as string where each number has two digits (eg. '1' -> '01')
  LEN.grid <- length(grid)
  
  if(width < chain){ # change width here for diagonal functions
    break
  }
  else if(LEN.grid != width*height){
    break
  }
  else{ # left-right specific
    old.product <- 0
    old.string <- "blank"
    old.position <- "blank"
    i <- 1
    while(i < (LEN.grid-63)){
      new.product <- 1
      string <- "Numbers:"
      position <- "Positions:"
      k <- i
      for(k in seq(i, (i+63), 21)){
        a <- grid[k]
        new.product = new.product*a
        string = paste(string, a, sep=" ")
        position = paste(position, k, sep=" ")
      }
      if(new.product > old.product){
        old.product <- new.product
        old.string <- string
        old.position <- position
      }
      if((i+chain-1)%%width==0){
        i = i+chain
      }
      else{
        i=i+1
      }
    }  
  }
  print(old.product)
  print(old.string)
  print(old.position)
} 

zx <- FUN.transform(z)
FUN.left.right.diag.1(zx,20,20,4)

FUN.left.right.diag.2 <- function(grid, width, height, chain){ # grid var as string where each number has two digits (eg. '1' -> '01')
  LEN.grid <- length(grid)
  
  if(width < chain){ # change width here for diagonal functions
    break
  }
  else if(LEN.grid != width*height){
    break
  }
  else{ # left-right specific
    old.product <- 0
    old.string <- "blank"
    old.position <- "blank"
    i <- 1
    while(i < (LEN.grid-63)){
      if(i%%width==1){
        i = i+chain-1
      }
      new.product <- 1
      string <- "Numbers:"
      position <- "Positions:"
      k <- i
      for(k in seq(i, (i+63), 19)){
        a <- grid[k]
        new.product = new.product*a
        string = paste(string, a, sep=" ")
        position = paste(position, k, sep=" ")
      }
      if(new.product > old.product){
        old.product <- new.product
        old.string <- string
        old.position <- position
      }
      if((i+chain-1)%%width==0){
        i = i+chain
      }
      else{
        i=i+1
      }
    }  
  }
  print(old.product)
  print(old.string)
  print(old.position)
} 

zx <- FUN.transform(z)
FUN.left.right.diag.2(zx,20,20,4)

2%%20

# 
# 
# 
# 
# (value <- c(rep(F,length(z.Pairs))))
# (position <- c(rep(F,length(z.Pairs))))
# count <- 1
# 
# 
# for(i in 99:0){
#   for(k in 1:length(z.Pairs)){
#     if(i==z.Pairs[k]){
#       value[count] <- i
#       position[count] <- k
#       count <- count + 1
#     }
#   }
# }
# value # ordered largest to smallest
# position # at value[1], position[1] is the numbers original position in 20 x 20 cube
# length(value)
# 
# (sieve <- c(rep(T,length(z.Pairs))))
# 
# for(j in length(z.Pairs):1){
#   if(z.Pairs[j]==0){
#     y <- c((j + 60),(j + 40),(j + 20),(j - 20),(j - 40),(j - 60))
#     x <- c((j + 3),(j + 2),(j + 1),(j - 1),(j - 2),(j - 3)) # correct
#     A.xy <- c((j + 63),(j + 42),(j + 21),(j - 21),(j - 42),(j - 63))
#     B.xy <- c((j + 57),(j + 38),(j + 19),(j - 19),(j - 38),(j - 57))
#     sieve[j] <- F
#   }
#   for(h in 1:6){ # length of y, x, 1xy, and 2xy
#     if((y[h]>0)&(y[h]<=400)){
#       sieve[y[h]] <- F
#     }
#     if((x[h]>0)&(x[h]<=400)){
#       sieve[x[h]] <- F
#     }
#     if((A.xy[h]>0)&(A.xy[h]<=400)){
#       sieve[A.xy[h]] <- F
#     }
#     if((B.xy[h]>0)&(B.xy[h]<=400)){
#       sieve[B.xy[h]] <- F
#     }
#   }
# }
# 
# sieve
# z.Pairs[7+57]
# z.Pairs[2]
# value
# position
# zeros <- c(7,9,40,186,193,233,243)
# length(zeros)
# 
# length(value)
# length(position)
# position[400]
# 
# LR.product <- 1
# i <- 1
# while(i<=400){
#   LR.new <- (z.Pairs[i]*z.Pairs[i+1]*z.Pairs[i+2]*z.Pairs[i+3])
#   UD.new <- (z.Pairs[i]*z.Pairs[i+20]*z.Pairs[i+40]*z.Pairs[i+60])
#   if(LR.new>LR.product){
#     LR.product = LR.new
#   }
#   if((i+3)%%20==0){
#     i = i+4
#   }
#   else{
#     i = i+1
#   }
# }
# LR.product
# 
# 
# FUN.ConnectFour <- function(position, gridH, gridL, vector){
#   
# }
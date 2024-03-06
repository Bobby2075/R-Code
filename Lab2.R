# Loops in R has two versions

primes <- c(2, 3, 5, 7, 11, 13)

#Loop version 1

for (p in primes) {
  print(p)
}

# loop version 2

for (i in 1:length(primes)) {
  print(primes[i])
}

# Same goes with looping voer a list

primes_list <- list(2, 3, 5, 7, 11, 13)

# loop version 1
for (p in primes_list) {
  print(p)
}

# loop version 2
for (i in 1:length(primes_list)) {
  print(primes_list[[i]])
}

# Looping over a matrix with a double loop

# The tic-tac-toe matrix ttt has already been defined for you

# define the double for loop
for (i in 1:nrow(ttt)) {
  for (j in 1:ncol(ttt)) {
    print(paste("On row", i, "and column", j, "the board contains", ttt[i,j]))
  }
}

# A function can be written as
hello <- function(){
  # if you require arguments write the variable in the function statement
  #if you want to return something write return() and pass the value you want to return
}

## You can use the function lapply() to apply a function over all the indexes of an array/vector/list

# fx

# The vector pioneers has already been created for you
pioneers <- c("GAUSS:1777", "BAYES:1702", "PASCAL:1623", "PEARSON:1857")

# Split names from birth year
split_math <- strsplit(pioneers, split = ":")

# Convert to lowercase strings: split_low
lapply(split_math, tolower())

# an anonoymous function can be written as such # Anonymous function with same implementation
#function(x) { 3 * x }
#this is good if we fx have lapply and only want to use the function once instead of creating a new object

# lapply can also use functions that require more then one argument, just pass it as another argument for lapply
# sapply can be used just as lapply. but instead of creating seperate vectors it creates on single array
# vapply works as sapply, but it has the FUN.VAL which has to be equal the return length of the FUN function
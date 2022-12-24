library(gmp) # for big integers
#help_function
#don't pay attention
rand <- function(n = 1, seed = 7) {
  a <- as.bigz(1103515245)
  c <- as.bigz(12345)
  m <- as.bigz(2^31)
  x <- rep(as.bigz(0), n)
  x[1] <- (a * as.bigz(seed) + c) %% m
  i <- 1
  while (i < n) {
    x[i+1] <- (a * x[i] + c) %% m
    i <- i + 1
  }

  as.integer(x)
}

#0
lcg <- function(n = 1, seed = 7, flag = 0) {
  a <- as.bigz(1103515245)
  c <- as.bigz(12345)
  m <- as.bigz(2^31)
  x <- rep(as.bigz(0), n)
  x[1] <- (a * as.bigz(seed) + c) %% m
  i <- 1
  start_time = Sys.time()
  while (i < n) {
    x[i+1] <- (a * x[i] + c) %% m
    i <- i + 1
  }
  end_time = Sys.time()

  cat('time', as.integer((end_time - start_time) * 10^3), 'ms', '\n')
  
  if(flag){
    for(i in 1:length(x))
      cat(as.integer(x[i]), " ")
    cat("\n")
  }
}

#1.1
max_find <- function(N, seed, flag = 0){
  vector <- rand(N, seed) 
  max <- vector[1]
  
  start_time = Sys.time()
  for (i in 1:length(vector))
    if(vector[i] > max)
      max <- vector[i]
  end_time = Sys.time()
  cat('time', as.integer((end_time - start_time) * 10^3), 'ms', '\n')

  if(flag)
    print(max)
}

#1.2
merge <- function(a, b) {
    temp <- numeric(length(a) + length(b))
    astart <- 1
      bstart <- 1
      j <- 1
    for(j in 1:length(temp)) {
        if((astart <= length(a) &&
            a[astart] < b[bstart]) ||
            bstart > length(b)) {
            temp[j] <- a[astart]
            astart <- astart + 1
        }
      else {
            temp[j] <- b[bstart]
            bstart <- bstart + 1         
        }
    }
    temp
}

mergeSort <- function(arr) {
    if(length(arr) > 1) {
        mid <- ceiling(length(arr)/2)
        a <- mergeSort(arr[1:mid])
        b <- mergeSort(arr[(mid+1):length(arr)])
        merge(a, b)
    }
    else
    {
        arr
    }
}

mergeSortTotal <- function(N,seed, flag = 0) {
  arr <- rand(N,seed)
  start_time = Sys.time()
  arr <- mergeSort(arr)
  end_time = Sys.time()
  cat('time', as.integer((end_time - start_time) * 10^3), 'ms', '\n')
  
  if(flag){
    for(i in 1:length(arr))
      cat(arr[i], " ")
    cat("\n")
  }
}

#2.1 skip


#3.1
matrix_multiplication <- function(N, seed, flag = 0){

  A <- matrix(
             c(rand(N^2, seed)),
             nrow = N,
             ncol = N,
             byrow = TRUE
  )

  B <- matrix(
             c(rand(N^2, seed)),
             nrow = N,
             ncol = N,
             byrow = TRUE
  )
  
  start_time = Sys.time()
  C <- A %*% B
  end_time = Sys.time()
  cat('time', as.integer((end_time - start_time) * 10^3), 'ms', '\n')

  if(flag)
    print(C)
}


#4.1
isPrime <- function(n){
  if(n < 1)
    return(FALSE)
  for(i in 2:sqrt(n))
    if(n %% i == 0)
      return(FALSE)
  return(TRUE)
}

find_prime_numbers <- function(a, b, flag = 0){
  
  if(b < 60184)
    size <- 6076
  if(b >= 60184 && a < 5393)
    size <- b/(log(b) - 1.1)
  if(b >= 60184 && a >= 5393)
    size <- b/(log(b) - 1.1) - a/(log(a) - 1)
  
  vector <- character(size)
  indx <- 0

  start_time = Sys.time()
  for(i in a:b)
  {
    if(isPrime(i))
    {
      vector <- append(vector, i, indx)
      indx <- (indx + 1)
    }
  }
  end_time = Sys.time()
  cat('time', as.integer((end_time - start_time) * 10^3), 'ms', '\n')

  if(flag){
    for(i in 1:indx)
      cat(vector[i], " ")
    cat("\n")
  }

}
#5.1
readFile <- function(filename){
  new_data <- NULL 

  start_time = Sys.time()

  data <- read.delim(filename, header = FALSE, sep = "\t", dec = ".") 
  s <- 0

  for(i in 1:length(data[[1]]))
    for(j in 1:nchar(data[[1]][i]))
      if(j %% 2 == 0)
        new_data <- append(new_data, substr(data[[1]][i], j, j))
  
  cat(new_data, file='sample.txt', sep="")
  for(i in 1:length(data[[1]]))
    cat(data[[1]][i], file='sample.txt', append = TRUE)

  end_time = Sys.time()
  cat('time', as.integer((end_time - start_time) * 10^3), 'ms', '\n')

} 

main <- function(){
  choice <- scan('input_from_file', what = "")
  if(choice[1] == '0'){
    choice <- as.integer(choice)
    lcg(choice[3], choice[4], choice[2])
  }
  if(choice[1] == '1.1'){
    choice <- as.integer(choice)
    max_find(choice[3], choice[4], choice[2])
  }
  if(choice[1] == '1.2'){
    choice <- as.integer(choice)
    mergeSortTotal(choice[3], choice[4], choice[2])
  }
  if(choice[1] == '3.1'){
    choice <- as.integer(choice)
    matrix_multiplication(choice[3], choice[4], choice[2])
  }
  if(choice[1] == '4.1'){
    choice <- as.integer(choice)
    find_prime_numbers(choice[3], choice[4], choice[2])
  }
  if(choice[1] == '5.1'){
    readFile(choice[2])
  }
}

main()

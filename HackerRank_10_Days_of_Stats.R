data <- suppressWarnings(readLines(file("stdin"),-1))
n1  <- as.numeric(unlist(strsplit(data[2]," ")))
cat(format(round(sum(n1 * n2)/sum(n2), 1), nsmall = 1), "/n")

line1 <- c("3 7 8 5 12 14 21 13 18")
line2 <- c("1 2 3 4 5 6 7 8 9 10")

n1  <- as.numeric(unlist(strsplit(line1," ")))
n2  <- as.numeric(unlist(strsplit(line2," ")))

sum(n1 * n2)/sum(n2)
cat(format(round(sum(n1 * n2)/sum(n2), 1), nsmall = 1))

getmode <- function(v) {
  v <- sort(v)
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(n1)


library(dplyr)
types <- unique(n1)
data <- as.data.frame(table(n1))
data %>% mutate(pairs = Freq %/% 2) %>% summarise(tot_pairs = sum(pairs)) %>% unlist() %>% cat()


## Day 1: Quartiles ----

data <- suppressWarnings(readLines(file("stdin"),-1))
n1  <- as.numeric(unlist(strsplit(data[2]," ")))
get.quarts <- function(x) {
  x <- sort(x)
  med <- median(x)
  tot_len <- length(x)
  lower <- x[1:(tot_len %/% 2)]
  if (tot_len %%2 == 0) {
    upper <- x[(tot_len %/% 2 + 1):tot_len]
  } else {
    upper <- x[(tot_len %/% 2 + 2):tot_len]
  }
  q1 <- median(lower)
  q3 <- median(upper)
  cat(q1, "\n")
  cat(med, "\n")
  cat(q3)
}
get.quarts(n1)


line1 <- c("6 12 8 10 20 16")
line2 <- c("5 4 3 2 1 5")

n1  <- as.numeric(unlist(strsplit(line1," ")))
n2  <- as.numeric(unlist(strsplit(line2," ")))

## Day 1: IQR ----

library(purrr)
data <- suppressWarnings(readLines(file("stdin"),-1))
n1  <- as.numeric(unlist(strsplit(data[2]," ")))
n2  <- as.numeric(unlist(strsplit(data[3]," ")))

get.iqr <- function(x, y) {
  s <- unlist(map2(x, y, rep.int))
  s <- sort(s)
  tot_len <- length(s)
  lower <- s[1:(tot_len %/% 2)]
  if (tot_len %%2 == 0) {
    upper <- s[(tot_len %/% 2 + 1):tot_len]
  } else {
    upper <- s[(tot_len %/% 2 + 2):tot_len]
  }
  q1 <- median(lower)
  q3 <- median(upper)
  iqr <- q3 - q1
  cat(format(round(iqr, 1), nsmall = 1))
}

get.iqr(n1, n2)

## Day 1: standard deviation

line1 <- c("10 40 30 50 20")
n1  <- as.numeric(unlist(strsplit(line1," ")))

library(purrr)
data <- suppressWarnings(readLines(file("stdin"),-1))
n1  <- as.numeric(unlist(strsplit(data[2]," ")))

std.dev <- function(x) {
  mu <- mean(x)
  stdev <- sqrt(sum(map_dbl(x, ~(mu-.)^2))/length(x))
  cat(format(round(stdev, 1), nsmall = 1))
}

std.dev(n1)

## Day 4: Binomial Distribution I

line1 <- c("1.09 1")
n1  <- as.numeric(unlist(strsplit(line1," ")))

data <- suppressWarnings(readLines(file("stdin"),-1))
n1  <- as.numeric(unlist(strsplit(data[1]," ")))

pbinom.3.6 <- function(x) {
  boys <- x[1]
  girls <- x[2]
  births <- sum(boys,girls)
  p <- boys / births
  dens <- pbinom(q = 2.9, size = 6, prob = p, lower.tail = FALSE)
  cat(format(round(dens, 3), nsmall = 1))
}

pbinom.3.6(n1)

## Day 4: Binomial Distribution II

line1 <- c("12 10")
n1  <- as.numeric(unlist(strsplit(line1," ")))

data <- suppressWarnings(readLines(file("stdin"),-1))

pistons <- function(data) {
  n1  <- as.numeric(unlist(strsplit(data[1]," ")))
  p <- n1[1]/100
  n <- n1[2]

  t1 <- pbinom(q = 2, size = n, prob = p, lower.tail = TRUE)
  t2 <- pbinom(q = 1.9, size = n, prob = p, lower.tail = FALSE)
  
  cat(format(round(t1, 3), nsmall = 3), "\n")
  cat(format(round(t2, 3), nsmall = 3))
}

pistons(data)

## Day 4: Geomteric Distribution I

line1 <- c("1 3")
line2 <- ("5")
n1  <- as.numeric(unlist(strsplit(line1," ")))
n2  <- as.numeric(line2)

data <- suppressWarnings(readLines(file("stdin"),-1))

geom1 <- function(data) {
  n1  <- as.numeric(unlist(strsplit(data[1]," ")))
  n2  <- as.numeric(data[2])
  # line1 <- c("1 3")
  # line2 <- ("5")
  # n1  <- as.numeric(unlist(strsplit(line1," ")))
  # n2  <- as.numeric(line2)
  
  prob <- n1[1] / n1[2] # probability of failure
  
  # pgeom() will calculate probability of q (OR FEWER!) failures before a success (cumulative sum of dgeom() for 0, 1, ..., q)
  answ <- pgeom((n2-1), prob) - pgeom((n2-2), prob) # probability of 4 "failed" failures before a "successful" failure: (4 or fewer) - (3 or fewer)
  
  cat(format(round(answ, 3), nsmall = 3))
}

geom1(data)
geom1(0)

## Day 4: Geometric Distribution II

data <- suppressWarnings(readLines(file("stdin"),-1))

geom1 <- function(data) {
  n1  <- as.numeric(unlist(strsplit(data[1]," ")))
  n2  <- as.numeric(data[2])
  
  prob <- n1[1] / n1[2] # probability of failure
  
  # pgeom() will calculate probability of q (OR FEWER!) failures before a success (cumulative sum of dgeom() for 0, 1, ..., q)
  answ <- pgeom((n2-1), prob) 
  
  cat(format(round(answ, 3), nsmall = 3))
}

geom1(data)

## Day 5: Poisson Distribution I

data <- suppressWarnings(readLines(file("stdin"),-1))

pois1 <- function(data) {
  rate <- as.numeric(data[1])
  X <- as.numeric(data[2])
 
  answ <- dpois(x = X, lambda = rate)
  cat(format(round(answ, 3), nsmall = 3))
}

pois1(data)

## Day 5: Poisson Distribution II

data <- suppressWarnings(readLines(file("stdin"),-1))

pois2 <- function(data) {
  rates <- as.numeric(unlist(strsplit(data[1]," ")))
  rateA <- rates[1]
  rateB <- rates[2]
  
  # Poisson E[X] = lambda and Var[X] = lambda
  # hence, since Var[X] = E[X^2] - (E[X])^2, thus E[X^2] = Var[X] + (E[X])^2 = lambda + lambda^2
  
  costA <- 160 + 40 * (rateA + rateA^2) # note here (E[X])^2, so E[X^2] = lambda + lambda^2
  costB <- 128 + 40 * (rateB + rateB^2)
  
  cat(format(round(costA, 3), nsmall = 3), "\n")
  cat(format(round(costB, 3), nsmall = 3))
}

pois2(data)

## Day 5: Normal Distribution I

data <- suppressWarnings(readLines(file("stdin"),-1))

norm1 <- function(data) {
  n1 <- as.numeric(unlist(strsplit(data[1]," ")))
  hrs <- as.numeric(data[2])
  n3 <- as.numeric(unlist(strsplit(data[3]," ")))

  mu <- n1[1]
  stdev <- n1[2]
  h1 <- n3[1]
  h2 <- n3[2]
    
  ans1 <- pnorm(hrs, mean = mu, sd = stdev)
  ans2 <- pnorm(h2, mean = mu, sd = stdev) - pnorm(h1, mean = mu, sd = stdev)
  
  cat(format(round(ans1, 3), nsmall = 3), "\n")
  cat(format(round(ans2, 3), nsmall = 3))
}

norm1(data)

## Day 5: Normal Distribution II

data <- suppressWarnings(readLines(file("stdin"),-1))

norm2 <- function(data) {
  n1 <- as.numeric(unlist(strsplit(data[1]," ")))
  hi <- as.numeric(data[2])
  pass <- as.numeric(data[3])

  mu <- n1[1]
  stdev <- n1[2]
  
  # hi <- 80
  # pass <- 60
  # mu <- 70
  # stdev <- 10
  
  ans1 <- pnorm(hi, mean = mu, sd = stdev, lower.tail = FALSE) * 100
  ans2 <- pnorm(pass, mean = mu, sd = stdev, lower.tail = FALSE) * 100
  ans3 <- 100 - ans2
  
  cat(format(round(ans1, 2), nsmall = 2), "\n")
  cat(format(round(ans2, 2), nsmall = 2), "\n")
  cat(format(round(ans3, 2), nsmall = 2))
}

norm2(0)
norm2(data)

## Day 6: The Central Limit Theorem I

data <- suppressWarnings(readLines(file("stdin"),-1))

clt1 <- function(data) {
  max_load <- as.numeric(data[1])
  n_boxes <- as.numeric(data[2])
  mu <- as.numeric(data[3])
  stdev <- as.numeric(data[4])
  
  ans <- pnorm(max_load, mean = mu * n_boxes, sd = stdev * sqrt(n_boxes))

  cat(format(round(ans, 4), nsmall = 4))
}

clt1(data)

## Day 6: The Central Limit Theorem II

data <- suppressWarnings(readLines(file("stdin"),-1))

clt2 <- function(data) {
  tix_left <- as.numeric(data[1])
  students <- as.numeric(data[2])
  mu <- as.numeric(data[3])
  stdev <- as.numeric(data[4])
  
  # what is probability that students * tix_bought/student < tix_left
  ans <- pnorm(tix_left, mean = mu * students, sd = stdev * sqrt(students))
  
  cat(format(round(ans, 4), nsmall = 4))
}

clt2(data)

## Day 6: The Central Limit Theorem III

data <- suppressWarnings(readLines(file("stdin"),-1))

clt3 <- function(data) {
  n_sample <- as.numeric(data[1]) # 100
  mu <- as.numeric(data[2])       # 500
  stdev <- as.numeric(data[3])    # 80
  interval <- as.numeric(data[4]) # 0.95
  z <- as.numeric(data[5])        # 1.96
  
  p_tail <- (1 - interval) / 2
  
  # z = (x - mu) / stdev
  # x = z * stdev + mu
  # find (lower, upper) such that P(lower < X < upper) = 100% * interval = 95%
  lower <- qnorm(p = p_tail, mean = mu, sd = stdev/sqrt(n_sample))
  upper <- qnorm(p = 1 - p_tail, mean = mu, sd = stdev/sqrt(n_sample))
  
  cat(format(round(lower, 2), nsmall = 2), "\n")
  cat(format(round(upper, 2), nsmall = 2))
}

clt3(data)

## Day 7: Pearson Correlation Coefficient I

data <- suppressWarnings(readLines(file("stdin"),-1))

pcc1 <- function(data) {
  
  n <- as.numeric(data[1]) # 10
  x <- as.numeric(unlist(strsplit(data[2]," "))) # "10 9.8 8 7.8 7.7 7 6 5 4 2" (1 <= x <= 500)
  y <- as.numeric(unlist(strsplit(data[3]," "))) # "200 44 32 24 22 17 15 12 8 4" (1 <= y <= 500)
   
  x_bar <- mean(x)
  y_bar <- mean(y)
  
  x_sd <- sqrt( sum( (x - x_bar)^2 ) / n)
  y_sd <- sqrt( sum( (y - y_bar)^2 ) / n)
  
  covXY <- sum( (x - x_bar) * (y - y_bar) ) / n
  
  rXY <- covXY / (x_sd * y_sd)
  
  cat(format(round(rXY, 3), nsmall = 3))
}

pcc1(data)

## Day 7: Spearman's Rank Correlation Coefficient

library(tibble)
library(dplyr)

data <- suppressWarnings(readLines(file("stdin"),-1))

srcc <- function(data) {
  
  n1 <- as.numeric(data[1]) # 10
  n2 <- as.numeric(unlist(strsplit(data[2]," "))) # "10 9.8 8 7.8 7.7 1.7 6 5 1.4 2 " (1 <= x <= 500)
  n3 <- as.numeric(unlist(strsplit(data[3]," "))) # "200 44 32 24 22 17 15 12 8 4" (1 <= y <= 500)

  x_key <- tibble(uniques = sort(unique(x)), x_rank = seq_along(uniques))
  y_key <- tibble(uniques = sort(unique(y)), y_rank = seq_along(uniques))
  
  input <- tibble(x=x, y=y)
  calcs <- input %>%
    left_join(x_key, by = c("x" = "uniques")) %>%
    left_join(y_key, by = c("y" = "uniques")) %>%
    summarize(x_bar = mean(x_rank),
              y_bar = mean(y_rank),
              x_sd = sqrt( sum( (x_rank - x_bar)^2 ) / n),
              y_sd = sqrt( sum( (y_rank - y_bar)^2 ) / n),
              covXY = sum( (x_rank - x_bar) * (y_rank - y_bar) ) / n,
              rXY = covXY / (x_sd * y_sd))
  
  cat(format(round(calcs$rXY, 3), nsmall = 3))
}

srcc(data)

## Day 8: Least Square Regression Line

library(dplyr)
library(tibble)

data <- suppressWarnings(readLines(file("stdin"),-1))

leastSqrReg <- function(data) {
  
  n1 <- as.numeric(unlist(strsplit(data[1]," ")))
  n2 <- as.numeric(unlist(strsplit(data[2]," "))) 
  n3 <- as.numeric(unlist(strsplit(data[3]," "))) 
  n4 <- as.numeric(unlist(strsplit(data[4]," "))) 
  n5 <- as.numeric(unlist(strsplit(data[5]," ")))
  
  names(n1) <- c("x", "y")
  names(n2) <- c("x", "y")
  names(n3) <- c("x", "y")
  names(n4) <- c("x", "y")
  names(n5) <- c("x", "y")

  scores <- bind_rows(n1, n2, n3, n4, n5)
  
  m1 <- lm(y ~ x, data = scores)
  pred <- m1[[1]][[1]] + m1[[1]][[2]] * 80
  
  cat(format(round(pred, 3), nsmall = 3))
}

leastSqrReg(data)

## Day 8: Pearson Correlation Coefficient II
# regression line is y = -3/4 * x --> what is PCC? (answ: -3/4)

## Day 9: Multiple Linear Regression

library(tidyr)
library(dplyr)
library(tibble)
library(purrr)

data <- suppressWarnings(readLines(file("stdin"),-1))

multLinReg <- function(data) {
  data_list <- data %>% 
    map(strsplit, split = " ") %>% 
    map(unlist) %>%
    map(as.numeric)
  
  m <- data_list[[1]][[1]]
  n <- data_list[[1]][[2]]
  q <- data_list[[n + 2]]
  
  train <- data_list[2:(n+1)]
  test <- data_list[(n+3):(n+2+q)]
  
  train <- data.frame(matrix(unlist(train), nrow=n, byrow=T)) %>% as_tibble()
  test <- data.frame(matrix(unlist(test), nrow=q, byrow=T)) %>% as_tibble()
  
  independent <- paste0("X", 1:m)
  dependent <- paste0("X", m+1)
  
  fmla <- as.formula(paste(dependent, "~ ", paste(independent, collapse= "+")))
  
  mlm <- lm(fmla, data = train)
  preds <- predict(mlm, test)
  out <- format(round(preds, 2), nsmall = 2)
  map(out, ~cat(., "\n"))[[1:q]]
}

multLinReg(data)
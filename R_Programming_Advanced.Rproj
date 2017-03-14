#Advanced R Programming

#control structures

x <- runif(1, 0, 10)
if(x > 3) {
  y <- 10
} else {
  y <- 0
}
y

#for loops

x <- c("a", "b", "c", "d")
for(i in seq_along(x)) {
  print(x[i])
}


x <- matrix(1:6, 2, 3)
x
for(i in seq_len(nrow(x))) {
  for(j in seq_len(ncol(x))) {
    print(x[i, j])
  }
}

#next

for(i in 1:100) {
  if(i <= 20) {
    ## Skip the first 20 iterations
    next
  }
  ## Do something here
}

#break

for(i in 1:100) {
  print(i)
  if(i > 20) {
    ## Stop loop after 20 iterations
    break
  }
}

#Functions

library(dplyr)
library(readr)
## pkgname: package name (character)
## date: YYYY-MM-DD format (character)
num_download <- function(pkgname, date = "2016-07-20") {
  ## Construct web URL
  year <- substr(date, 1, 4)
  src <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz",
                 year, date)
  ## Construct path for storing local file
  dest <- file.path("C:/Users/Balan Gnanam/Desktop/", basename(src))
  ## Don't download if the file is already there!
  if(!file.exists(dest))
    download.file(src, dest, quiet = TRUE)
  cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
  cran %>% filter(package == pkgname) %>% nrow
}

num_download("filehash")

#Refactoring code

check_for_logfile <- function(date) {
  year <- substr(date, 1, 4)
  src <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz",
                 year, date)
  dest <- file.path("C:/Users/Balan Gnanam/Desktop/", basename(src))
  if(!file.exists(dest)) {
    val <- download.file(src, dest, quiet = TRUE)
    if(!val)
      stop("unable to download file ", src)
  }
  dest
}

num_download <- function(pkgname, date = "2016-07-20") {
  dest <- check_for_logfile(date)
  cran <- read_csv(destin, col_types = "ccicccccci", progress = FALSE)
  cran %>% filter(package == pkgname) %>% nrow
}

num_download("filehash")

#checking dependencies
check_pkg_deps <- function() {
  if(!require(readr)) {
    message("installing the 'readr' package")
    install.packages("readr")
  }
  if(!require(dplyr))
    stop("the 'dplyr' package needs to be installed first")
}
check_pkg_deps()

#Vectorization

num_download <- function(pkgname, date = "2016-07-20") {
  check_pkg_deps()
  dest <- check_for_logfile(date)
  cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
  cran %>% filter(package %in% pkgname) %>%
    group_by(package) %>%
    summarize(n = n())
}
num_download(c("filehash", "weathermetrics"))

#Argument Checking- Revised function 

num_download <- function(pkgname, date = "2016-07-20") {
  check_pkg_deps()
  ## Check arguments
  if(!is.character(pkgname))
    stop("'pkgname' should be character")
  if(!is.character(date))
    stop("'date' should be character")
  if(length(date) != 1)
    stop("'date' should be length 1")
  dest <- check_for_logfile(date)
  cran <- read_csv(dest, col_types = "ccicccccci")
  cran %>% filter(package %in% pkgname) %>%
    group_by(package) %>%
    summarize(n = n())
}

num_download("filehash",c("2016-07-20", "2016-0-21"))
num_download("filehash","2016-07-20")


#functional programming

adder_maker <- function(n){
  function(x){
    n + x
  }
}
add2 <- adder_maker(2)
add3 <- adder_maker(3)
add2(5)

#map

library("purrr")

#map function over one data structure

map_chr(c(5, 4, 3, 2, 1), function(x){
  c("one", "two", "three", "four", "five")[x]
})

map_lgl(c(1, 2, 3, 4, 5), function(x){
  x > 3
})

map_if(1:5, function(x){
  x %% 2 == 0
},
function(y){
  y^2
}) %>% unlist()

map_at(seq(100, 500, 100), c(1, 3, 5), function(x){
  x - 10
}) %>% unlist()

#map function over two or more data structures

map2_chr(letters, 1:26, paste)

pmap_chr(list(
  list(1, 2, 3),
  list("one", "two", "three"),
  list("uno", "dos", "tres")
), paste)

#Reduce

reduce(letters[1:4], function(x, y){
  message("x is ", x)
  message("y is ", y)
  message("")
  paste0(x, y)
})

reduce_right(letters[1:4], function(x, y){
  message("x is ", x)
  message("y is ", y)
  message("")
  paste0(x, y)
})

#search

contains(letters, "a")
detect(20:40, function(x){
  x > 22 && x %% 2 == 0
})
detect_index(20:40, function(x){
  x > 22 && x %% 2 == 0
})

#Filter

keep(1:20, function(x){
  x %% 2 == 0
})

discard(1:20, function(x){
  x %% 2 == 0
})

every(1:20, function(x){
  x %% 2 == 0
})

some(1:20, function(x){
  x %% 2 == 0
})


#compose

n_unique <- compose(length, unique) 
# The composition above is the same as:
# n_unique <- function(x){
# length(unique(x))
# }

rep(1:5, 1:5)
n_unique(rep(1:5, 1:5))


#partial

mult_three_n <- function(x, y, z){
  x * y * z
}
mult_by_15 <- partial(mult_three_n, x = 3, y = 5)
mult_by_15(z = 4)

#Side Effects
library(purrr)
walk(c("Friends, Romans, countrymen,",
       "lend me your ears;",
       "I come to bury Caesar,",
       "not to praise him."), message)

#Recursion


vector_sum_rec <- function(v){
  if(length(v) == 1){
    v
  } else {
    v[1] + vector_sum_rec(v[-1])
  }
}
vector_sum_rec(c(5, 40, 91))


# Fibinocci Series 

fib <- function(n){
  stopifnot(n > 0)
  if(n == 1){
    0
  } else if(n == 2){
    1
  } else {
    fib(n - 1) + fib(n - 2)
  }
}


#memoization

fib_tbl <- c(0, 1, rep(NA, 23))
fib_mem <- function(n){
  stopifnot(n > 0)
  if(!is.na(fib_tbl[n])){
    fib_tbl[n]
  } else {
    fib_tbl[n - 1] <<- fib_mem(n - 1)
    fib_tbl[n - 2] <<- fib_mem(n - 2)
    fib_tbl[n - 1] + fib_tbl[n - 2]
  }
}
map_dbl(1:12, fib_mem)

#Expressions

two_plus_two <- quote(2 + 2)
two_plus_two

eval(two_plus_two)


#string into expression 

tpt_string <- "2 + 2"
tpt_expression <- parse(text = tpt_string)
eval(tpt_expression)

deparse(two_plus_two)

#the call() function

sum_40_50_expr <- call("sum", 40, 50)
sum_40_50_expr
eval(sum_40_50_expr)


x <- quote(read.csv("important.csv", row.names = FALSE))
x[[1]]
x[[2]]


#match.call function
first_arg <- function(...){
  expr <- match.call()
  first_arg_expr <- expr[[2]]
  first_arg <- eval(first_arg_expr)
  if(is.numeric(first_arg)){
    paste("The first argument is", first_arg)
  } else {
    "The first argument is not numeric."
  }
}

first_arg(2, 4, "seven", FALSE)
first_arg("two", 4, "seven", FALSE)

#Environment 

my_new_env <- new.env()
my_new_env$x <- 4
my_new_env$x
assign("y", 9, envir = my_new_env)
get("y", envir = my_new_env)

ls(my_new_env)
exists("y", envir = my_new_env)
search()

x<-10
assign1 <- function(){
  x <<- "Wow!"
}
x

##Errors and Warnings

"hello" + "world"

as.numeric(c("5", "6", "seven"))

# Generating Errors and Warnings

stop("Something erroneous has occurred!")

name_of_function <- function(){
  stop("Something bad happened.")
}
name_of_function()

error_if_n_is_greater_than_zero <- function(n){
  stopifnot(n <= 0)
  n
}
error_if_n_is_greater_than_zero(5)

make_NA <- function(x){
  warning("Generating an NA.")
  NA
}
make_NA("Sodium")

message("In a bottle.")

#error handling

beera <- function(expr){
  tryCatch(expr,
           error = function(e){
             message("An error occurred:\n", e)
           },
           warning = function(w){
             message("A warning occured:\n", w)
           },
           finally = {
             message("Finally done!")
           })
}
beera({
  2 + 2
})

beera({
  as.numeric(c(1, "two", 3))
})
#error checking is slow

is_even_error <- function(n){
  tryCatch(n %% 2 == 0,
           error = function(e){
             FALSE
           })
}
is_even_check <- function(n){
  is.numeric(n) && n %% 2 == 0
}

library(microbenchmark)
microbenchmark(sapply(letters, is_even_check))
microbenchmark(sapply(letters, is_even_error))


##debugging

#traceback

check_n_value <- function(n) {
  if(n > 0) {
    stop("n should be <= 0")
  }
}
error_if_n_is_greater_than_zero <- function(n){
  check_n_value(n)
  n
}
error_if_n_is_greater_than_zero(5)

traceback()

#browser

check_n_value <- function(n) {
  if(n > 0) {
    browser() ## Error occurs around here
    stop("n should be <= 0")
  }
}

check_n_value(3)

#trace

trace("check_n_value")

as.list(body(check_n_value))
trace("glm", browser, at = 4, where = asNamespace("stats"))




error_if_n_is_greater_than_zero(4)

#profiling and benchmarking

# Function that uses a loop

#function 1
find_records_1 <- function(datafr, threshold){
  highest_temp <- c()
  record_temp <- c()
  for(i in 1:nrow(datafr)){
    highest_temp <- max(highest_temp, datafr$temp[i])
    record_temp[i] <- datafr$temp[i] >= threshold &
      datafr$temp[i] >= highest_temp
  }
  datafr <- cbind(datafr, record_temp)
  return(datafr)
}

#function 2

library(tidyverse)
find_records_2 <- function(datafr, threshold){
  datafr <- datafr %>%
    mutate_(over_threshold = ~ temp >= threshold,
            cummax_temp = ~ temp == cummax(temp),
            record_temp = ~ over_threshold & cummax_temp) %>%
    select_(.dots = c("-over_threshold", "-cummax_temp"))
  return(as.data.frame(datafr))
}

#testing on a small dataset
example_data <- data.frame(date = c("2015-07-01", "2015-07-02",
                                    "2015-07-03", "2015-07-04",
                                    "2015-07-05", "2015-07-06",
                                    "2015-07-07", "2015-07-08"),
                           temp = c(26.5, 27.2, 28.0, 26.9,
                                    27.5, 25.9, 28.0, 28.2))
example_data
test_1 <- find_records_1(example_data, 27)
test_1
test_2 <- find_records_2(example_data, 27)
test_2
all.equal(test_1, test_2)

library(microbenchmark)

record_temp_perf <- microbenchmark(find_records_1(example_data, 27),
                                   find_records_2(example_data, 27))
record_temp_perf


#bigger data set 

library(dlnm)
data("chicagoNMMAPS")
record_temp_perf_2 <- microbenchmark(find_records_1(chicagoNMMAPS, 27),
                                     find_records_2(chicagoNMMAPS, 27))
record_temp_perf_2


##Object Oriented Principles

#constructor

shape_s3 <- function(side_lengths){
  structure(list(side_lengths = side_lengths), class = "shape_S3")
}

#objects- instantiations of the s_3 class

square_4 <- shape_s3(c(4, 4, 4, 4))
class(square_4)

triangle_3 <- shape_s3(c(3, 3, 3))
class(triangle_3)

#Create method define actions

is_square <- function(x) UseMethod("is_square")

#Associate method with class
is_square.shape_S3 <- function(x){
  length(x$side_lengths) == 4 &&
    x$side_lengths[1] == x$side_lengths[2] &&
    x$side_lengths[2] == x$side_lengths[3] &&
    x$side_lengths[3] == x$side_lengths[4]
}

is_square.default <- function(x){
  NA
}


is_square(square_4)

is_square(c(1, 1, 1, 1))

print(square_4)



print.shape_S3 <- function(x){
  if(length(x$side_lengths) == 3){
    paste("A triangle with side lengths of", x$side_lengths[1],
          x$side_lengths[2], "and", x$side_lengths[3])
  } else if(length(x$side_lengths) == 4) {
    if(is_square(x)){
      paste("A square with four sides of length", x$side_lengths[1])
    } else {
      paste("A quadrilateral with side lengths of", x$side_lengths[1],
            x$side_lengths[2], x$side_lengths[3], "and", x$side_lengths[4])
    }
  } else {
    paste("A shape with", length(x$side_lengths), "slides.")
  }}


print(square_4)

print(triangle_3)

#inheritance

class(square_4)
class(square_4) <- c("shape_S3", "square")

class(square_4)
inherits(square_4, "square")

##S3 example

#constructor class for polygon 

make_poly <- function(x, y) {
  if(length(x) != length(y))
    stop("'x' and 'y' should be the same length")
  ## Create the "polygon" object
  object <- list(xcoord = x, ycoord = y)
  ## Set the class name
  class(object) <- "polygon"
  object
}

## Print method for polygon objects
## x an object of class "polygon"
print.polygon <- function(x, ...) {
  cat("a polygon with", length(x$xcoord),
      "vertices\n")
  invisible(x)
}

## Summary method for polygon objects
## object an object of class "polygon"

summary.polygon <- function(object, ...) {
  object <- list(rng.x = range(object$xcoord),
                 rng.y = range(object$ycoord))
  class(object) <- "summary_polygon"
  object
}

## Print method for summary.polygon objects
## x an object of class "summary_polygon"
print.summary_polygon <- function(x, ...) {
  cat("x:", x$rng.x[1], "-->", x$rng.x[2], "\n")
  cat("y:", x$rng.y[1], "-->", x$rng.y[2], "\n")
  invisible(x)
}

## Construct a new "polygon" object
x <- make_poly(1:4, c(1, 5, 2, 1))

print(x)

out <- summary(x)
class(out)

print(out)


summary(x)

##S4 system 

setClass("bus_S4",
         slots = list(n_seats = "numeric",
                      top_speed = "numeric",
                      current_speed = "numeric",
                      brand = "character"))

setClass("party_bus_S4",
         slots = list(n_subwoofers = "numeric",
                      smoke_machine_on = "logical"),
         contains = "bus_S4")



my_bus <- new("bus_S4", n_seats = 20, top_speed = 80,
              current_speed = 0, brand = "Volvo")

my_bus

my_party_bus <- new("party_bus_S4", n_seats = 10, top_speed = 100,
                    current_speed = 0, brand = "Mercedes-Benz",
                    n_subwoofers = 2, smoke_machine_on = FALSE)
my_party_bus

my_bus@n_seats
my_party_bus@top_speed

setGeneric("is_bus_moving", function(x){
  standardGeneric("is_bus_moving")
})

setMethod("is_bus_moving",
          c(x = "bus_S4"),
          function(x){
            x@current_speed > 0
          })


my_bus@current_speed <- 1
is_bus_moving(my_bus)

setGeneric("print")
setMethod("print",
          c(x = "bus_S4"),
          function(x){
            paste("This", x@brand, "bus is traveling at a speed of", x@current_speed)
          })

print(my_bus)

print(my_party_bus)

#Reference classes

Student <- setRefClass("Student",
                       fields = list(name = "character",
                                     grad_year = "numeric",
                                     credits = "numeric",
                                     id = "character",
                                     courses = "list"),
                       methods = list(
                         hello = function(){
                           paste("Hi! My name is", name)
                         },
                         add_credits = function(n){
                           credits <<- credits + n
                         },
                         get_email = function(){
                           paste0(id, "@jhu.edu")
                         }
                       ))

brooke <- Student$new(name = "Brooke", grad_year = 2019, credits = 40,
                      id = "ba123", courses = list("Ecology", "Calculus III"))
roger <- Student$new(name = "Roger", grad_year = 2020, credits = 10,
                     id = "rp456", courses = list("Puppetry", "Elementary Algebra"))

brooke$credits

Grad_Student <- setRefClass("Grad_Student",
                            contains = "Student",
                            fields = list(thesis_topic = "character"),
                            methods = list(
                              defend = function(){
                                paste0(thesis_topic, ". QED.")
                              }
                            ))
jeff <- Grad_Student$new(name = "Jeff", grad_year = 2021, credits = 8,
                         id = "jl55", courses = list("Fitbit Repair",
                                                     "Advanced Base Graphics"),
                         thesis_topic = "Batch Effects")
jeff$defend()


#......................................................................#


#R package development 
































































add2 <- function(x, y){
  x + y
}

above10 <- function(x) {
        use <- x > 10
        x[use]
}

above <-  function(x, n = 10) {
      use <-  x > n
      x[use]
}

columnmean <- function(y, removeNA = TRUE){
          nc <- ncol(y)
          means <- numeric(nc)
          for(i in 1:nc) {
                means[i] <- mean(y[, i], na.rm = removeNA)
          }
          means
}

# Functions are created using the function() directive and are stored as R objects just like anything else. In particular, they are R objects of class 'function'.

# f <- function(<arguments>) { ## Do something interesting}

# Functions in R are "first class objects", which means that they can be treated much like any other R object. Importantly,
# - Functions can be passes as arguments to other functions
# - Functions can be nested, so that you can define a function inside of another function. The return value of a function is the last expression in the function body to be evaluated.

# Functions have "named arguments" which potentially have "default values".
# - The 'formal' arguments are the arguments included in the function definition.
# - The 'formals' function returns a list of all the formal arguments of a function.
# - Not every function call in R makes use of all the formal arguments.
# - Function arguments can be missing or might have default values.

# ARGUMENT MATCHING
# R functions arguments can be matched positionally or by name. So the following calls to 'sd' are all equivalent.

mydata <- rnorm(100)
sd(mydata)
sd(x = mydata)
sd(x = mydata, na.rm = FALSE)
sd(na.rm = FALSE, x = mydata)
sd(na.rm = FALSE, mydata)
# Not recommend to rearrange the order of the argument which can lead to confusion.

# You can mix positional matching with matching by name. When an argument is matched by name, it is "taken out" of the argument list and the remaining unnamed arguments are matched in the order that they are listed in the function definiton.
args(lm)
function(formula, data, subset, weights, na.action,
         method = 'qr', model = TRUE, x = FALSE, 
         y = FALSE, qr = TRUE, singular.ok = TRUE,
         contrasts = NULL, offset, ...)
  
# The following two calls are equivalent
lm(data = mydata, y - x, model = FALSE, 1:100)
lm(y - x, mydata, 1:100, model = FALSE)

# Most of the time, named arguments are useful on the command line when you have a long argument list and you want to use the defaults for everything except for an argument near the end of the list.
# Named arguments also help if you can remember the name of the argument and not its position on the argument list (plotting is a good example)
# Function arguments can also be partially matched, which is useful for interactive work. The order of operations when given an argument is
# 1. Check for exact match for a named argument
# 2. Check for a partial match
# 3. Check for a positional match
# Find a unique match

# Defining a Function
f <- function(a, b = 1, c = 2, d = NULL){
  
}
# In addition to not specifying a default value, you can also set an argument value to NULL.

# LAZY EVALUATION
# Arguments to functions are evaluated lazily, so they are evaluated only as needed.
f <- function(a,b){
  a^2
}
f(2)
# This function never actually uses the arguments "b", so calling "f(2)" will not produce an error because the "2" gets positionally matched to "a".

f <- function(a,b) {
  print(a)
  print(b)
}
f(45)
## Error: argument "b" is missing, with no default
## Notice that "45" got printed first before the error was triggered. This is because "b" did not have to be evaluated until after 'print(a)'.
## Once the function tried to evaluate 'print(b)' , it had to throw an error.

## The '...' Argument
## The '...' argument indicate a variable number of arguments that are usually passed on to other functions.
## '...' is often used when extending another function and you don't want to copy the entire argument list of the original function

myplot <- function(x, y, type = "l", ...){
  plot(x, y, type = type, ...)
}
## Generic functions use '...' so that extra arguments can be apssed to methods.
mean
function(x,...)
UseMethod("mean")

## The '...' Argument is to a placeholder for all the other arguments in the plot function that could be used to pass down to the original plot function.
## so all the original arguments can be preserved and don't need to retype or reconstruct all of those arguments in the extended function.

## The '...' argument is also necessary when the number of arguments passed to the function cannot be known in advance.
args(paste) 
function(..., sep = " ", collaspe = NULL)
## The 'paste' function concatenates a set of strings together to create one string or a vector of strings and it can take a variable number of arguments.
## The first argument for 'paste' is '...' using a separator 'sep'. The other arguments is collaspe. They come after the '...' argument.   

args(cat)
function(..., file = "", sep = " ", fill = FALSE,
         labels = NULL, append = FALSE)
## The 'cat' function works similar to 'paste'. It puts together a number of strings then prints out the concatenated string either to a file or to a console.

## ARGUMENTS COMING AFTER THE '...' ARGUMENT
## One catch with '...' is that any arguments that appear after '...' on the argument list must be named explicitly and cannot be partially matched.
args(paste)
function(..., sep = " ", collapse = NULL)

paste("a", "b", sep = ":")

paste("a", "b", se = ":")
## In the above example, R  is passing ":" as another string because 'se' is partial of 'sep' which R cannot tell if it is a different argument.


## A Diversion on Binding Values to Symbol
## How does R know which value to assign to which symbol?
lm <- function(x) {x * x}
lm
# how does R know what value to assign to the symbol "lm"? Why doesn't it give it the value of "lm" that is in the stats package?

# When R tries to bind a value to a symbol, it searches through a series of "environments" to find the appropriate value. 
# When you are working on the command line and need to retrieve the value of an R object, the order is roughly
# 1. Search the global environment for a symbol name matching the one requested.
# 2. Search the namespaces of each of the packages on the search list.

# The search list can be found by using the 'search' function.
search()

# The global environment or the user's workspace is always the first element of the search list and the base package is always the last.
# The order of the packages on the search list matters!
# User's can configuer which packages get loaded on startup so you cannot assume that there will be a set list of packages available.
# When a user loads a package with 'library' the namespace of that package gets put in position 2 of the search list (by default) and everything else gets shifted down the list.
# Note that R has separate namespaces for functions andnon-functions so it's possible to have an object named "c" and a function named "c".

# SCOPING RULES
# The scoping rules for R are the main feature that make it different from the orignal S language.
# The scoping rules determine how a value is associated with a free variable in a function
# R uses "lexical scoping" or "static scoping". A common alternative is "dynamic scoping".
# Related to the scoping rules is how R uses the search list to bind a value to a symbol.
# Lexical scoping turns out to be particularly useful for simplifying statistical computations.


  







#Question 1

new.function <- function(a,b,c)
  if(c == "add"){
    return(a+b)
  }else if(c == "subtract")
{return(a-b)
  
  }else if(c == "multiply")
{return(a*b)
  
  }else if(c == "divide")
{return(a/b)
  
  }else if(c == "log")
{ return(log(a,base=b))
  
  }else if(c == "power")
{
  return(a^b)
  } else
  {
    a <- -1
    return(a) 
  }

output <- new.function(4,4,"divided")
print(output)


#function that takes a number as input and outputs a sequence of odd numbers strating from 1 to that number.

another.function <- function(a)
  return(seq(1,a,2))

output <- another.function(20)
print(output)


# function that return the class of supplied input

class.function <- function(x)
  return(class(x))

output <- class.function(11L)
print(output)

#program that checks whether a number is prime and returns true for prime and false for not prime

  function.prime <- function(num) 
    if (num == 2) {
      TRUE
    } else if (any(num %% 2:(num-1) == 0)) {
      FALSE
    } else { 
      TRUE
    }
 out <-  function.prime(7)
 print(out)
 
 # program that iterates through all LETTERS and prints only the vowels.
 
 len <- 0 
 i <- 0
 vowel.function <- function(x)
   len= length(x)
 for(i in len){
   if(i =='a' || i=='A' || i=='e' || i=='E' || i=='i' || i=='I' || i=='o' || i=='O' || i=='u' || i=='U')
     return(i)
 }
  
#' Higher order functions and function wrappers to empower and simplify common operations.
#'
#' Hordr adds features, generalizes, and fills in gaps in standards R operations to add power and flexibility.  
#' @name hordr
#' @docType package
NULL


rollApply <- function(data,fun,window=len(data),minimum=1,align='left')
{
  if(minimum>len(data))
    return()
  FUN=match.fun(fun)
  if (align=='left')
    result<-sapply(1:(len(data)-minimum+1),function (x) FUN(rows(data,x:(min(len(data),(x+window-1))))))
  if (align=='right')
    result<-sapply(minimum:len(data),function (x) FUN(rows(data,max(1,x-window+1):x)))
  return(result)
}

rowApply<-function(data,fun)
{
  vapply(1:len(data),function (x) fun(rows(data,x)))
}

#'Wraps a function to only display it's results when matching a specific condition.
#'
#'Implementation of T-SQL \code{count} and Excel \code{COUNTIF} functions.  Shows the total number of elements in any number of data objects altogether or that match a condition.
#'
#'@param condition 
#'@param fun
#'@param ...
#'@export
#'@examples
#'count(c(NA,1,2))
#'count(c(NA,1,2),is.na)
#'count(c(NA,1,2),list('A',4),cbind(1,2,3))
#'count(c(NA,1,2),list('A',4),cbind(1,2,3),condition=is.character)
alert<-function(condition)
{
  function(fun)
  {
    function(...)
    {
      results<-fun(...)
      if(condition(results))
      {
        results
      }
    }
  }
}

#'An alternative to \code{\link{system.time}} that prints the times to the screen while returning the result of the original operation.
#'
#'This process time utility can be wrapped around any normal function with no effect other than printing the process time to the screen.  This allows visual feedback on timing without otherwise interrupting the operation of the function.
#'
#'@param fun the function to evaluate
#'@param ... all other arguments to be passed to fun
#'@export
#'@examples
#'results<-time (vectorize(sum))(rep.int(x=5,times=1000000))
#'results
time<-function(fun)
{
  function(...)
  {
    ptm<-proc.time()
    results<-fun(...)
    print(proc.time()-ptm)
    return(results)
  }
}

#'A wrapper that will add a progress bar to many higher order functions.
#'
#'This process time utility can be wrapped around any normal function with no effect other than printing the process time to the screen.  This allows visual feedback on timing without otherwise interrupting the operation of the function.
#'
#'@param fun the function to evaluate
#'@param ... all other arguments to be passed to fun
#'@export
#'@examples
#'results<-pb(lapply)(sqrt,rep.int(x=5,times=100000))
#'results<-pb(Reduce)('+',sqrt(c(1:100000)))
pb<-function(fun)
{
  function(f,data,...)
  {
    env <- environment()
    pb_Total <- len(data)
    counter <- 0
    pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)
    FUN<-match.fun(f)
    wrapper <- function(...){
      curVal <- get("counter", envir = env)
      assign("counter", curVal +1 ,envir=env)
      setTxtProgressBar(get("pb", envir=env), curVal +1)
      FUN(...)
    }
    if(any(grepl('f',names(formals(fun)))))
      arg<-'f'
    if(any(grep('FUN',names(formals(fun)))))
      arg<-'FUN'
    ptm<-proc.time()
    call<-parse(text=paste("fun(",arg,'=wrapper,','data)'))
    res <- eval(call)
    print(proc.time()-ptm)
    close(pb)
    res 
  }
}

echo<-function(string)
{
  function(f)
  {
    
  }
}



Curry <- function(FUN,...) {
  .orig = list(...);
  function(...) do.call(FUN,c(.orig,list(...)))
}

Compose <- function(...) {
  fs <- list(...)
  function(...) Reduce(function(x, f) f(x),
                       fs,
                       ...)
}

#Funcall and iterate can be called by example(Reduce)

Funcall <- function(f, ...) 
{
  f(...)
}
## Compute log(exp(acos(cos(0))
#Reduce(Funcall, list(log, exp, acos, cos), 0, right = TRUE)
#This is probably obsoleted by proper use of do.call:
#Reduce(function (fun,x) do.call(fun,list(x)), list(log, exp, acos, cos), 5, right = TRUE)

## n-fold iterate of a function, functional style:
Iterate <- function(f, n = 1) function(x) Reduce(Funcall, rep.int(list(f), n), x, right = TRUE)
#> Iterate (function (x) x^2,n=3)(2)
#[1] 256
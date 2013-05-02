#' Higher order functions and function wrappers to empower and simplify common operations.
#'
#' Hordr adds features, generalizes, and fills in gaps in standards R operations to add power and flexibility.  
#' @name hordr
#' @docType package
NULL

#' Robust alternative to Vectorize function that accepts function with two or more arguments.  
#'
#' Returns a function that will work an arbitrary number of vectors, lists or data frames, though output may be unpredicatable in unusual applications The results are also intended to be more intuitive than \code{\link{Vectorize}}. 
#'
#' @param fun a two or more argument function
#' @param type 1 forces a row-wise evaluation, even on atomic vectors
#' @export
#' @examples
#' vectorize(`+`)(c(1,2,3))
#' vectorize(sum)(c(1,2,3),c(1,2,3))
#' # Compare these results to Vectorize, which does not vectorize sum at all.
#' Vectorize(sum)(c(1,2,3),c(1,2,3))
#' # Any combination of vectors, lists, matrices, or data frames an be used (although cbind has some strange behavior in certain cases)
#' vectorize(`+`)(c(1,2,3),list(1,2,3),cbind(c(1,2,3)))

vectorize<-function(fun,type=0)
{
  function(...)
  {
    cols<-cbind(...)
    if((ncol(cols)>1) | (type==1 & (nrow(cols) > 1)))
      apply(cols,1,function (x) Reduce(fun,unlist(x)))
    else
      Reduce(fun,cols[,1])
  }  
}

#' A more versatile form of the T-SQL \code{coalesce()} function.  
#'
#' Little more than a wrapper for \code{\link{vectorize}}, allows for duplication of SQL coalesce functionality, certain types of if-else statements, and \code{\link{apply}}/\code{\link{Reduce}} combinations.
#' 
#' @param ... an arbitrary number of data objects, including vectors, lists, data objects
#' @param fun a two argument function that returns an atomic value
#' @export
#' @examples
#' coalesce(c(NA,1,2))
#' coalesce(c(NA,1,2),c(3,4,NA))

coalesce<-function(...,fun=(function (x,y) if(!is.na(x)) x else y))
{

    FUN=match.fun(fun)
    vectorize() (FUN)(...)
}

#'A more versatile form of the T-SQL \code{count()} function.
#'
#'Implementation of T-SQL \code{count} and Excel \code{COUNTIF} functions.  Shows the total number of elements in any number of data objects altogether or that match a condition.
#'
#'@param ... an arbitrary number of vectors
#'@param condition a 1 argument condition
#'@export
#'@examples
#'count(c(NA,1,2))
#'count(c(NA,1,2),is.na)
#'count(c(NA,1,2),list('A',4),cbind(1,2,3))
#'count(c(NA,1,2),list('A',4),cbind(1,2,3),condition=is.character)

count<-function(...,condition=(function (x) TRUE))
{
  data<-c(...)
  result<-sum(sapply(data, function (x) if(condition(x)) 1 else 0))
  return(result)
}

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


rollApply <- function(data,fun,window=len(data),minimum=1,align='left')
{
  if(minimum>len(data))
    return()
  FUN=match.fun(fun)
  if (align=='left')
    result<-sapply_pb(1:(len(data)-minimum+1),function (x) FUN(rows(data,x:(min(len(data),(x+window))))))
  if (align=='right')
    result<-sapply_pb(minimum:len(data),function (x) FUN(rows(data,max(1,x-window+1):x)))
  return(result)
}

#'Allows finding the 'length' without knowledge of dimensionality.

len <- function(data)
{
  result<-ifelse(is.null(dim(data)),length(data),nrow(data))
  return(result)
}

#'Allows row indexing without knownledge of dimensionality.

rows <- function(data,rownums)
{
  #result<-data[rownums]
  if(is.null(dim(data)))
  {
    result<-data[rownums]
  }
  else
  {
    result<-data[rownums,]
  }
  #result<-ifelse(is.null(dim(data)),data[c(rownums)],data[c(rownums),])
  return(result)
}

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

buffer<-function(...,size=0,fill=NA,align='left')
{
  input<-c(...)
  if(align=='left')
    result<-c(input,rep(fill,(max(0,size-len(input)))))
  else
    result<-c(rep(fill,(max(0,size-len(input)))),input)
  return(result)
}
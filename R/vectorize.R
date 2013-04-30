#' vectorize
#'
#' @name vectorize
#' @docType package

# 
# vectorize<-function()
# {
#   function(fun)
#   {
#     function(...)
#     {
#       cols<-cbind(...)
#       if(ncol(cols)<=1)
#         fun(cols[,1])
#       else
#         apply(cols,1,function (x) fun(unlist(x)))
#     }
#   }
# }

vectorize<-function(type=0)
{
  function(fun)
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
}


coalesce<-function(...,fun=(function (x,y) if(!is.na(x)) x else y))
{

    FUN=match.fun(fun)
    vectorize() (FUN)(...)
}


count<-function(...)
{
  args<-list(...)
  result<-sum(!is.na(unlist(args)))
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

len <- function(data)
{
  result<-ifelse(is.null(dim(data)),length(data),nrow(data))
  return(result)
}

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




# pb<-function(FUN,...)
# {
#   data<-unlist(...)
#   FUN=match.fun(FUN)
#   function(fun)
#   {
#     env <- environment()
#     pb_Total <- len(data)
#     counter <- 0
#     pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)   
#     
#     # wrapper around FUN
#     wrapper <- function(...){
#       curVal <- get("counter", envir = env)
#       assign("counter", curVal +1 ,envir=env)
#       setTxtProgressBar(get("pb", envir=env), curVal +1)
#       FUN(...)
#     }
#     ptm<-proc.time()
#     res <- fun(f=wrapper,data)
#     print(proc.time()-ptm)
#     close(pb)
#     res
#   }
# }

# pb<-function()
# {
#   function(fun,...)
#   {
#     function(data)
#     {
#       env <- environment()
#       pb_Total <- len(data)
#       counter <- 0
#       pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)   
#       wrapper <- function(...){
#         curVal <- get("counter", envir = env)
#         assign("counter", curVal +1 ,envir=env)
#         setTxtProgressBar(get("pb", envir=env), curVal +1)
#         FUN(data,...)
#       }
#     }
#     ptm<-proc.time()
#     res <- fun(f=wrapper,data)
#     print(proc.time()-ptm)
#     close(pb)
#     res  
#   }
# }

# #**********************************************************************************
# # utilities.r
# # This file contains the code to load a control file into an R list, and other
# # useful things.
# #
# # Author            : Chris Grandin
# # Development Date  : June 2013 - Present
# #**********************************************************************************
# 
controlfile.to.rlist <- function(fn){
  # Read in the data from the REP file given as 'fn'.
  # File structure:
  # It is assumed that each text label (variable name) will be on its own line,
  # followed by one or more lines of data. The variable name must be preceeded by a $ character
  # with no spaces in between, e.g. $n_obs
  # If the label is followed by a single value or line of data,
  #  a vector will be created to hold the data.
  # If the label is followed by multiple lines of data,
  #  a matrix will be created to hold the data. The matrix might be
  #  ragged so a check is done ahead of time to ensure correct
  #  matrix dimensions.
  #
  # If a label has another label following it but no data,
  #  that label is thrown away and not included in the returned list.
  #
  # A label must start with an alphabetic character followed by
  # any number of alphanumeric characters (includes underscore and .)

  dat <- readLines(fn, warn = FALSE)

  # Remove preceeding and trailing whitespace on all elements,
  #  but not 'between' whitespace.
  dat <- gsub("^[[:blank:]]+", "", dat)
  dat <- gsub("[[:blank:]]+$", "", dat)

  # Remove all lines starting with the comment character
  dat <- dat[-grep("^#",dat)]

  # Remove all blank lines
  dat <- dat[nzchar(dat)]

  # Find the line indices of the labels
  # Labels start with a '$' followed by one or more alphabetic characters
  # followed by zero or more alphanumeric characters
  idx  <- grep("^\\$[[:alpha:]]+[[:alnum:]]*", dat)
  # Remove $ from the variable names
  dat[idx]=substr(dat[idx],2,nchar(dat[idx]))

  objs <- dat[idx]     # A vector of the object names
  nobj <- length(objs) # Number of objects
  ret  <- list()
  indname <- 0

  for(obj in 1:nobj){
    indname <- match(objs[obj], dat)
    if(obj != nobj){ # If this is the last object
      inddata <- match(objs[obj + 1], dat)
    }else{
      inddata <- length(dat) + 1 # Next row
    }
    # 'inddiff' is the difference between the end of data
    # and the start of data for this object. If it is zero,
    # throw away the label as there is no data associated with it.
    inddiff <- inddata - indname
    tmp <- NA

    if(inddiff > 1){
      if(inddiff == 2){
        # Create and populate a vector
        vecdat <- dat[(indname + 1) : (inddata - 1)]
        vecdat <- strsplit(vecdat,"[[:blank:]]+")[[1]]
        #vecnum <- as.numeric(vecdat)
        ret[[objs[obj]]] <- vecdat
      }else if(inddiff > 2){
        # Create and populate a (possible ragged) matrix
        matdat <- dat[(indname + 1) : (inddata - 1)]
        matdat <- strsplit(c(matdat), "[[:blank:]]+")
        # Now we have a vector of strings, each representing a row
        # of the matrix, and not all may be the same length
        rowlengths <- unlist(lapply(matdat, "length"))
        nrow <- max(rowlengths)
        ncol <- length(rowlengths)
        # Create a new list with elements padded out by NAs
        matdat <- lapply(matdat, function(.ele){c(.ele, rep(NA, nrow))[1:nrow]})
        matnum <- do.call(rbind, matdat)
        mode(matnum) <- "numeric"
        ret[[objs[obj]]] <- matnum
      }
    }else{
      # Throw away this label since it has no associated data.
    }
  }
  return(ret)
}

cat0 <- function(...){
  ## Makes it so you can write this:
  ## cat0("X = ", X, " and Y = ", Y)
  ## instead of this:
  ## cat(paste("X = ", X, " and Y = ", Y, "\n", sep=""))
  cat(..., "\n", sep="")
}
# 
curfnfinder <- function(skipframes=0, skipnames="(FUN)|(.+apply)|(replicate)",
    retIfNone="Not in function", retStack=FALSE, extraPrefPerLevel="\t")
{
  # Get the current function name from within the function itself.
  # Used to prepend the function name to all messages so that the
  # user knows where the message came from.
    prefix<-sapply(3 + skipframes+1:sys.nframe(), function(i){
            currv<-sys.call(sys.parent(n=i))[[1]]
            return(currv)
        })
    prefix[grep(skipnames, prefix)] <- NULL
    prefix<-gsub("function \\(.*", "do.call", prefix)
    if(length(prefix)==0)
    {
        return(retIfNone)
    }
    else if(retStack)
    {
        return(paste(rev(prefix), collapse = "|"))
    }
    else
    {
        retval<-as.character(unlist(prefix[1]))
        if(length(prefix) > 1)
        {
            retval<-paste(paste(rep(extraPrefPerLevel, length(prefix) - 1), collapse=""), retval, sep="")
        }
        return(retval)
    }
}
# 
getCurrFunc <- function(){
  # Returns the calling function's name followed by ": "
  funcName <- curfnfinder(skipframes=1) # skipframes=1 is there to avoid returning getCurrFunc itself
  # Strip extraneous whitespace
  funcName <- gsub("\t+","",funcName)
  funcName <- gsub("\ +","",funcName)
  funcName <- paste(funcName,": ",sep="")
  return(funcName)
}

getQuants <- function(data=NULL, ci=NULL){
  # Return the column quantiles for data matrix.
  # The median along with the confidence interval 'ci'
  # will be calculated and the quantiles returned.
  currFuncName <- getCurrFunc()
  if(is.null(data)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply an input posterior matrix (data).")
    return(NULL)
  }
  if(is.null(ci)){
    cat0(.PROJECT_NAME,"->",currFuncName,"You must supply an input confidence interval in % (ci).")
    return(NULL)
  }
  ciprop <- ci / 100
  probs <- c((1-ciprop)/2,0.5,1-((1-ciprop)/2))
  if(is.null(dim(data))){
    # It is a single posterior, e.g. sbo
    quants <- quantile(data, probs)
  }else{
    # It is a timeseries posterior, e.g. sbt
    quants <- apply(data, 2, quantile, probs)
  }
  return(quants)
}

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
  # Return TRUE if x is an integer, FALSE otherwise
  abs(x - round(x)) < tol
}

ile <- function(l, ind, val, replace=FALSE){
  # insert the element 'val' at list 'l' in position given by 'ind'
  # while preserving the rest of the list.
  # i.e. a list of [[1]] 1 [[2]] 2 [[3]] 4
  # with function call(l, 3, 3) will return:
  # [[1]] 1 [[2]] 2 [[3]] 3 [[4]] 4
  # Algorithm: Get the left part of the list, then glue on the 'val'
  #            element and then glue on the right part of the list.
  #            If replace is TRUE, the list element at position
  #            'ind' will be replaced with the 'val'.
  # if 'val' is a list, it will be inserted as if each element is
  # on its own, i.e. the return list will be a single, simple list
  # with the sublist 'val' flattened and inserted element-by-element
  #
  # Returns NA if there is an error
  # Only works on lists of values, not lists of lists.
  currFuncName <- getCurrFunc()

  if(ind < 1 || ind > (length(l)+ 1)){
    cat0(.PROJECT_NAME,"->",currFuncName,"Index less than zero or greater than the length of the list.")
    return(NA)
  }
  # tmpl is the left part of the list
  if(ind == 1){
    tmpl <- NULL
  }else{
    tmpl <- l[1:(ind-1)]
  }
  # Glue on the 'val' element to the end of tmpl
  # remember the old index, so that we can refer to the list 'l' after
  origind <- ind
  if(is.list(val)){
    unval <- unlist(val)
    for(i in 1:length(val)){
      tmpl[[ind]] <- unval[i]
      ind <- ind + 1
    }
  }else{
    tmpl[[ind]] <- val
    ind <- ind + 1
  }
  if(replace){
    origind <- origind + 1
  }
  # Glue on the right part of the list to tmpl if
  # the list has more elements to be appended
  if(origind <= length(l)){
    for(i in origind:length(l)){
      tmpl[[ind]] <- l[[i]]
      ind <- ind + 1
    }
  }
  return(as.list(tmpl))
}

testile <- function(){
  # Test the 'insert list element' (ile) function
  currFuncName <- getCurrFunc()

  l <- list(2,3,4)
  ind <- 1
  val <- 1
  out <- ile(l,ind,val)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert value at beginning of list")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(9,2,3)
  ind <- 1
  val <- 1
  out <- ile(l,ind,val,replace=TRUE)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert value at beginning of list with replacement")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(1,2,3)
  ind <- 4
  val <- 4
  out <- ile(l,ind,val)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert value at end of list")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(1,2,9)
  ind <- 3
  val <- 3
  out <- ile(l,ind,val,replace=TRUE)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert value at end of list with replacement")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(1,2,4)
  ind <- 3
  val <-3
  out <- ile(l,ind,val)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert value in middle of list")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(1,9,3)
  ind <- 2
  val <- 2
  out <- ile(l,ind,val,replace=TRUE)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert value in middle of list with replacement")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(4,5,6)
  ind <- 1
  val <- list(1,2,3)
  out <- ile(l,ind,val)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert list elements at beginning of list")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(9,4,5)
  ind <- 1
  val <- list(1,2,3)
  out <- ile(l,ind,val,replace=TRUE)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert list elements at beginning of list with replacement")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(1,2,3)
  ind <- 4
  val <- list(4,5,6)
  out <- ile(l,ind,val)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert list elements at end of list")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(1,2,9)
  ind <- 3
  val <- list(3,4,5)
  out <- ile(l,ind,val,replace=TRUE)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert list elements at end of list with replacement")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(1,2,6)
  ind <- 3
  val <- list(3,4,5)
  out <- ile(l,ind,val)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert list elements in middle of list")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")

  l <- list(1,9,5)
  ind <- 2
  val <- list(2,3,4)
  out <- ile(l,ind,val,replace=TRUE)
  cat0("**********************************************")
  cat0(.PROJECT_NAME,"->",currFuncName,"Insert list elements in middle of list with replacement")
  cat0("Input list:")
  print(l)
  cat0("Index:")
  print(ind)
  cat0("Value:")
  print(val)
  cat0("Returned list:")
  print(out)
  cat0("**********************************************")
}

.getShade <- function(color, opacity){
  # If color is a single R color string or single number,
  #  returns an rgb string of the specified color and opacity
  # If color is a vector of cR color strings or numbers,
  #  returns a vector of rgb strings of the specified color and opacity.
  # If the opacity argument is non-integer or not between 0 and 99, NULL will be returned.
  # - opacity - 2-decimal-digit string (00-99), i.e. "20" means 20%
  # Notes: format of returned string is #RRGGBBAA
  #        where RR=red, a 2-hexadecimal-digit string
  #        GG=green, a 2-hexadecimal-digit string
  #        BB=blue, a 2-hexadecimal-digit string
  #        AA=alpha or opacity
  #
  # The opacity agrument is scalar and will be applied to all colors.
  if(!(opacity %% 1 == 0) || opacity<0 || opacity>99){
    cat0(.PROJECT_NAME,"->",currFuncName,"opacity argument must be an integer between 0 and 99.")
    return(NULL)
  }
  colorDEC <- col2rgb(color)
  if(is.matrix(colorDEC)){
    colorHEX <- matrix(nrow=3,ncol=ncol(colorDEC))
    shade <- NULL
    for(col in 1:ncol(colorDEC)){
      for(row in 1:nrow(colorDEC)){
        colorHEX[row,col] <- sprintf("%X", colorDEC[row,col])
        if(nchar(colorHEX[row,col])==1){
          colorHEX[row,col] <- paste0("0",colorHEX[row,col])
        }
      }
      shade[col] <- paste0("#",colorHEX[1,col],colorHEX[2,col],colorHEX[3,col],opacity)
    }
  }else{
    colorHEX <- sprintf("%X", colorDEC)
    for(i in 1:length(colorHEX)){
      if(nchar(colorHEX[i])==1){
        colorHEX[i] <- paste0("0",colorHEX[i])
      }
    }
    shade <- paste0("#",colorHEX[1],colorHEX[2],colorHEX[3],opacity)
  }
  return(shade)
}

.gletter <- function(letter){
  # gletter()
  # adds letters to plot panels
  # - letter - the letter to place on the panel
  usr <- par("usr")
  inset.x <- 0.05*(usr[2]-usr[1])
  inset.y <- 0.05*(usr[4]-usr[3])
  text(usr[1]+inset.x,usr[4]-inset.y,paste("(",letters[letter],")",sep=""),cex=1.,font=1)
}


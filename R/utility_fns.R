
#' Test out how to write appropriate unit tests
#'
#' @param x integer value
#' @return Text indicating whether the input was a "1"
#' @examples
#' intro_to_unittest(x)
intro_to_unittest <- function(x){
  if(x == 1){
    return("one")
  }else{
    return("not one")
  }
}

#' Probabilistically inject label noise according to proportions 
#'
#' @param y The binary variable output.
#' @param flip_i The proportion of samples of class A to flip to class B - will probabilistically flip ~this proportion of samples, but may not be exact.
#' @param flip_j The proportion of samples of class B to flip to class A - will probabilistically flip ~this proportion of samples, but may not be exact.
#' @return A list containing: "yz", the flipped sample labels; "fd", an indicator variable with +1 if the sample was flipped and -1 if the sample label is unflipped.
#' @examples
#' inject_label_noiseR(y, flip_i, flip_j)
inject_label_noiseR <- function(y, flip_i, flip_j){
  fd <- rep(1, length(y)) * -1
  yz <- castLabel(y, -1)
  y <- castLabel(y, 2)
  flip_rate <- c(flip_i, flip_j)
  for(i in c(1,2)){
    prob = rand(length(yz),1)
    idx = intersect(which(y==i), which(prob <= flip_rate[i]))
    yz[idx] = yz[idx] * -1
    fd[idx] = fd[idx] * -1
  }
  yz = castLabel(yz, 2)
  return(list("yz" = yz, "fd" = fd))
}


#' Cast the labels to the appropriate notation for processing 
#'
#' @param y The binary variable output.
#' @param t 
#' @return y, a list of labels cast to the appropriate notation 
#' @examples
#' castLabel(c(1,1,2,2), 2)
castLabel <- function(y, t){
  if (length(y) == 1){
    print('All value of y required to recognise current format')
    return
  }
  if (-1 %in% y){
    # {-1,1} input
    if(t == -1){
      y = y # do nothing, included for clarity
    }else if(t == 0){
      y = (y+1)/2
    }else if(t==2){
      y = (y+3)/2
    }
  }else if(0 %in% y){
    # {0,1} input
    if(t == -1){
      y = y *2 -1
    }else if(t == 0){
      y = y # do nothing, included for clarity
    }else if(t == 2){
      y = y + 1
    }
  }else if (2 %in% y){
    # {1,2} input
    if (t == -1){
      y = y * 2 - 3
    }else if(t == 0){
      y = y -1
    }else if(t == 2){
      y = y # do nothing, included for clarity
    }
  }else if(1 %in% y){
    if(t == -1){
      y = y * -1
    }
  }
  return(y)
}
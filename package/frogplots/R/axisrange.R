#' @title int_dvsr
#'
#' @description A simple to use function to find all divisors for an integer number
#' @param x An integer number
#' @return A vector containing all the divisors for the input integer
#' @examples
#' \dontrun{
#' int_dvsr(10)
#' }
#' @export
nat_dvsr<-function(x){
tstSeq<-seq_len(abs(x))
tstSeq[x%%tstSeq == 0] # %% does "modulus"
}

#' @title all_dvsr
#'
#' @description A function to find all divisors for any types of number
#' @param x The number of interest
#' @param i Decimal scaling factor. If the input number is an integer, the starting number will be \code{x*10^i}; if a decimal, the starting number will be \code{x*10^n*10^i} (\code{x*10^(n+i)}), where the auto-determined \code{n} is the decimal amplification factor that transforms the decimal numbers to their corresponding minimum intergers by \code{10*n}.
#' @return A vector containing all the divisors for the input number
#' @examples
#' \dontrun{
#' all_dvsr(0.5)
#' all_dvsr(0.6,1) # with decimal scaling factor set at 1, meaning the divisor will be based on 60.
#' all_dvsr(2)
#' all_dvsr(2,1) # with decimal scaling factor set at 1, meaning the divisor will be based on 20.
#' }
#' @export
all_dvsr<-function(x,i=0){
  dopt<-getOption("scipen")
  on.exit(options(scipen = dopt))

  options(scipen=999)
  if (x%%1==0){
    smpl<-x*10^i
    return(nat_dvsr(smpl)/10^i)
  } else {
    n<-nchar(unlist(strsplit(as.character(x),"[.]"))[2])
    sc<-as.integer(round(x*10^(n+i)))
    sc<-abs(sc)
    return(nat_dvsr(sc)/(10^(n+i)))
  }
}

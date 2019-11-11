.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Written by Jing Zhang, Ph.D. Please direct questions to jzhangcad@gmail.com.
                        To cite in publication: Zhang J, Storey KB. (2016) RBioplot: an easy-to-use R pipeline for automated statistical analysis and data visualization in molecular biology and biochemistry. PeerJ 4:e2436.
                        For more details, please visit: http://kenstoreylab.com/?page_id=2448")
  return(TRUE)
}

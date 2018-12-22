.onAttach <- function(lib, pkg)
{
  ## unlockBinding("forestChange", asNamespace("forestChange")) 
  version <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
  
  if(interactive())
    { # > figlet forestChange
        packageStartupMessage(
          "forestChange
version: ", version)
}
else
    { packageStartupMessage(
          "Package 'forestChange' version ", version) } 

  packageStartupMessage("Type 'citation(\"forestChange\")' for citing this R package in publications.")
  invisible()
}
  

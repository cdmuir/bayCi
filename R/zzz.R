.onAttach <- function(libname, pkgname) {
  packageStartupMessage({
    paste0(
      crayon::red("\n\n*** WARNING ***\n\n"),
      crayon::bold("bayCi"), " is very preliminary and under active development.\n\n",       "Most features will therefore change substantially in the near future.\n", 
      "I encourage people to use it and give critical feedback. However, it is\n",
      "not ready for publication-quality analysis. If you wish to use these tools\n",
      "for your data, please contact me (Chris Muir, cdmuir@hawaii.edu) for\n",
      "assistance."
    )
    })
}

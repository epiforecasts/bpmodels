.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Note: bpmodels is now retired and replaced by epichains. ",
    "All features from bpmodels are available in epichains. ",
    "Get epichains from <https://github.com/epiverse-trace/epichains>.",
    "Thank you for your support!"
  )
}

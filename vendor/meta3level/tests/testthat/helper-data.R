make_example <- function() {
  raw <- utils::read.csv(system.file("extdata", "example_correlations.csv",
                                     package = "meta3level"),
                         stringsAsFactors = FALSE)
  m3prep(raw, measure = "r", study = "studyID", effect = "effectID",
         value = "r", n = "n")
}

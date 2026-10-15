# Developer setup only; source this file from an authorized R environment.
manifest_files <- Filter(Negate(is.null),lapply(sys.frames(),function(f)f$ofile))
manifest_root <- if (exists("agent_repository",inherits=TRUE)) normalizePath(agent_repository,mustWork=TRUE) else normalizePath(file.path(dirname(tail(manifest_files,1)[[1]]), "../../.."),mustWork=TRUE)
manifest_db <- installed.packages()
manifest_roots <- c("jsonlite", "metafor", "clubSandwich", "readxl", "pkgload",
  "lavaan","mclust","poLCA","networktree","partykit","bootnet","qgraph","IsingFit",
  "bnlearn","pcalg","ranger","openxlsx","officer")
manifest_packages <- setdiff(unique(c(manifest_roots, unlist(tools::package_dependencies(manifest_roots,
  db = manifest_db, which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE)))), "R")
manifest_paths <- vapply(manifest_packages, function(p) system.file(package = p), character(1))
if (any(!nzchar(manifest_paths))) stop("Install the missing build dependencies first.")
write.csv(data.frame(package = manifest_packages, path = manifest_paths),
  file.path(manifest_root, "agent/runtime-dependencies.csv"), row.names = FALSE)

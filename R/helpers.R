

#' mvExistingApsimxFiles
#'
#' @description Move the existing `Temp_ApsimFiles` into another folder with date.
#'
#' @return
#' @export
#'
#' @examples
mvExistingApsimxFiles <- function(outdir){
  # if(dir.exists(outdir)){
  cmd <- paste0("mv " , outdir, " ", outdir, "_\"$(date +\"%Y-%m-%d-%T\")\"")

  system(cmd)
  # } else{
  # dir.create(outdir)
  # }



}


# outdir <- file.path(Sys.getenv("path"), "Temp_ApsimFiles")
#' FindIncompleteJobs
#' @description Find the failed and incomplete jobs after all simulations.
#'
#' @param outdir Directory to where the simulation files hold
#' @param outpath path to a file to output the list of trouble simulation files.
#'
#' @return
#' @export
#'
#' @examples
FindIncompleteJobs <- function(outdir, outpath){

  # list all into categories
  incomplete_shm <- list.files(outdir, "*db-shm$")
  incomplete_wal <- list.files(outdir, "*db-wal$")
  dbs <- list.files(outdir, "*.db$")
  apsimx <- list.files(outdir, "*.apsimx")

  # Report how many failed completely - missing dbs
  failed <- length(apsimx) - length(dbs)
  cat("There are", failed, " jobs failed completely. \r\n")
  # Report how many incomplete - db with temps
  cat("There are", length(incomplete_shm), " jobs incomplete. \r\n")
  cat("There are", length(incomplete_wal), " jobs incomplete. \r\n")

  # Output failed ones
  dbs_name <- gsub(".db", "", dbs)
  apsimx_name <- gsub(".apsimx", "", apsimx)
  failed_basename <- apsimx_name[!apsimx_name %in% dbs_name]
  # Output incompleted ones
  incomplete_shm_name <- gsub(".db-shm", "", incomplete_shm)
  incomplete_wal_name <- gsub(".db-wal", "", incomplete_wal)
  apsimx_name <- gsub(".apsimx", "", apsimx)

  # Combine failed and incomplete
  combined <- c(failed_basename, incomplete_wal_name)
  fullpath <- gsub("(.+)($)", paste0(outdir,  "/\\1.apsimx"), combined)
  # temppath <- gsub("Temp_ApsimFiles", "LocalParameters/FailedAndIncompletedJobs.txt", outdir)
  writeLines(fullpath, con = outpath)
  cat("The apsimx files have been written into", outpath, "\r\n")

}

#' ReSubmit
#'
#' @description Re-submit the jobs from a txt file
#'
#' @param file The path to the file contains the failed or incomplete apsimx names.
#'
#' @return
#' @export
#'
#' @seealso [AtlasEDA::FindIncompleteJobs()]
#' @examples
ReSubmit <- function(file, projectName, apsimversion){

  cmd <- paste0("sed -E 's/(.+)/", apsimversion, " \"\\1\"/' ", file,
                " | split -l 200 --filter 'asub -q lowpriority -j ", projectName,"'")
  cat("The CLI is: \r\n", cmd, "\r\n")
  system(cmd)
}

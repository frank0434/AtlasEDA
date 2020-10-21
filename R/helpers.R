

#' listMets
#' @description call command line `find` to list all existing met files.
#'
#' @param input_dir A string to show the directory where the met files are
#' @param historical Logical. Default is TRUE, which ERA will be listed.
#'
#' @return vector has full met file names
#' @export
#'
#' @examples
#'
listMets <- function(input_dir, historical = TRUE){

  if(dir.exists(input_dir)){
    if(isTRUE(historical)){
      cmd_ERA <-  paste0('find ', input_dir, 'ERA/ -name *.met')
      existings_ERA <- system(cmd_ERA, intern = TRUE)
      return(existings_ERA)
    }
    else {
      cmd_CMIP5 <-  paste0('find ', input_dir, 'CMIP5/ -name *.met')
      existings_CMIP5 <- system(cmd_CMIP5, intern = TRUE)
      return(existings_CMIP5)
    }
  } else{
    print("Strang! Directory is not there!!!")
  }

}

#' ExistenceTest_Pixels
#'
#'
#' @author Jian(Frank) Liu
#'
#' @param projectName A string shows project name
#' @param agentNoLookup A sting shows the path to the agent number file with GPS location
#' @param PRM_Grid File path to the required agent number
#' @param PRM_RCPs File path to the climate models
#' @param output File path to output a missing list if any met files not there
#' @param ERA A character vector contains met file names. Returned from `listMets`
#' @param CMIP5 A character vector contains met file names. Returned from `listMets`
#'
#' @seealso \code{\link{listMets}}
#'
#' @return a data.frame shows the missing pixels
#'
#' @import data.table
#'
#' @export
#'

ExistenceTest_Pixels <- function(projectName,
                                 ERA, CMIP5, input_dir,
                                 agentNoLookup,
                                 PRM_Grid,
                                 PRM_RCPs,
                                 output){

  t1 <- Sys.time()

  # things we should have
  grid <- data.table::fread(PRM_Grid,  header = F)[, list(agent_no = as.integer(V1))][!is.na(agent_no)]
  # cat("The warning message \"NAs introduced by coercion\" in the \`.err\` file is casued by setting \`header = FALSE\` \r\n")
  #cat("when read \"PRM_GridCellsMet.txt\" file. \r\n")
  #cat("This is for fixing the incompleted tabs in the file.\r\n")
  RCPs <- data.table::fread(PRM_RCPs, select = "rcp")
  # RCPs <- c("RCP2.6" , "RCP4.5" , "RCP6.0" , "RCP8.5" , "RCPpast")
  models <- c("BCC-CSM1.1" ,"CESM1-CAM5", "GFDL-CM3" ,  "GISS-EL-R",  "HadGEM2-ES", "NorESM1-M" )
  target <- data.table::as.data.table(expand.grid(CMIP = "CMIP5", RCPs = RCPs[rcp!="ERA"]$rcp, GCMs = models, agent_no = grid$agent_no,stringsAsFactors = FALSE ))
  target <- target[, files:= do.call(paste, c(.SD, sep = "/"))][,list(files, agent_no)]
  if(length(grid) <1){
    stop("PRM_GridCellsMet.txt NOT FOUND.\n", call. = FALSE)
  }
  cat("---\r\n")
  cat("Read existing .met file names. \r\n---\r\n")
  existings_CMIP5 <- data.table::data.table(files = CMIP5, col.names = "files")

  existings_ERA <- data.table::data.table(files = ERA, col.names = "files")

  cat(nrow(existings_ERA), ".met files present in the ERA climate directory. \r\n")
  cat(nrow(existings_CMIP5), ".met files present in the CIMP5 climate directory. \r\n---\r\n")

  ERA <- existings_ERA[, agent_no := regmatches(files, regexpr("(?<=/)\\d{4,5}(?=\\.met)", files, perl = T))]
  # grid <- data.table::rbindlist(list(grid, data.frame(a =  c(9974), b = "Yes"))) #testing

  index_ERA <- grid$agent_no%in% ERA$agent_no

  CMIP5 <- existings_CMIP5[, files := gsub(paste0(input_dir, "|\\.met"),"", files)]


  index_CMIP5 <- target$files %in% CMIP5$files

  pixels_all <- data.table::fread(agentNoLookup)
  colnames(pixels_all) <- tolower(colnames(pixels_all))
  pixels_all <- pixels_all[,list(agent_no, lon = as.double(longt), lat = as.double(lat))]

  if(all(index_ERA)){

    cat("Great. All pixels have .met files present in the historical (ERA:1971~2000) climate data.\r\n")
    cat("---\r\n")
  } else {
    cat("---\r\n")
    cat("There are ", length(unique(ERA$agent_no)),
        "PIXELS have .met files in historical data\r\nBut unfortunately, \r\n")
    cat("Your projects' agent number(s):\r\n")
    cat(paste0(grid$agent_no[!index_ERA], collapse = "\r\n"), "\r\n")
    cat("has no matched .met files in the climate data.\r\n")
    cat("Please generate the .met files before moving forward!!!\r\n---\r\n\r\n")
    sdOUT_ERA <- pixels_all[grid[!index_ERA,], on =  "agent_no"][,list(agent_no, lon,  lat)]
    data.table::fwrite(sdOUT_ERA, file = output, sep = ",",quote = FALSE, row.names = FALSE)

  }

  if(all(index_CMIP5)){

    cat("Great. All pixels have .met files present in the CMIP5 (2001~2099/2100) climate data.\r\n")
    cat("---\r\n")
  } else {
    missing_pixel = unique(target$agent_no[!index_CMIP5])
    cat("---\r\n")
    cat("There are ", nrow(unique(CMIP5)),
        "PIXELS have .met files in CMIP5.\r\nBut unfortunately, \r\n")
    cat("Your project has ", length(missing_pixel),  "agent number(s) has no met file found in this RCP/GCM:\r\n")
    cat(paste0(missing_pixel, collapse = "\r\n"), "\r\n")
    missing_pixel <- target[!target$files %in% CMIP5$files][, list(agent_no)]
    cat("Please generate the .met files before moving forward!!!\r\n---\r\n\r\n")
    sdOUT_CMIP5 <- pixels_all[missing_pixel, on = c("agent_no")][,list(agent_no, lon,  lat)]

    data.table::fwrite(unique(sdOUT_CMIP5), file = file.path(output, "/temp_missings_CMIP5.txt"), sep = ",",quote = FALSE, row.names = FALSE)

  }

  cat("Temporary file(s) are generated for ERA and CMIP5 separately in",file.path(output),
      "\r\n")
  t2 <- Sys.time()
  cat(paste0("Time taken to figure out the missing pixels ", difftime(t2, t1, units="secs"), " seconds. \n"))

}
# ExistenceTest_Pixels(projectName, input_dir)


# Filter out the pixels have no met files at all  -------------------------

# Get rid of the agent number that has no .met files


#' filterGridCells
#'
#' @description Pixels along the coast or lake may be lack of raw data in the `netcdf`.
#' It might be worth to filter out those ones if there are still missing mets after regeneration.
#'
#' @param path path to the file reports missing met files
#' @param PRM_Grid path to the input grid cell
#'
#' @return
#' @export
#'
#' @examples
filterGridCells <- function(path, PRM_Grid, output){
  if(file.exists(path)){

    dt <- data.table::fread(missing_met)

    dt_mother <- data.table::fread(PRM_Grid)
    dt_mother <- dt_mother[!dt, on = "agent_no"]
    data.table::fwrite(dt_mother,
                       file = output,
                       append = FALSE, sep = "\t")

  }


}






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

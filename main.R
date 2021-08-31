library(BiocManager)
library(tercen)
library(dplyr)
library(flowCore)
 

fcs_to_data = function(filename, which.lines) {
  data_fcs = read.FCS(filename, which.lines, transformation = FALSE)
  names_parameters = data_fcs@parameters@data$desc
  data = as.data.frame(exprs(data_fcs))
  col_names = colnames(data)
  names_parameters = ifelse(is.na(names_parameters),col_names,names_parameters)
  colnames(data) = names_parameters
  data %>%
    mutate_if(is.logical, as.character) %>%
    mutate_if(is.integer, as.double) %>%
    mutate(.ci = rep_len(0, nrow(.))) %>%
    mutate(filename = rep_len(basename(filename), nrow(.)))
}
 
ctx = tercenCtx(workflowId = "b2a47cd105182a7d4c065c6946003838",
                 stepId = "d3370f96-2bf1-4881-8096-22b51100cd56")
 
if (!any(ctx$cnames == "documentId")) stop("Column factor documentId is required")

which.lines <- NULL
if(!is.null(ctx$op.value('which.lines')) && !ctx$op.value('which.lines') == "NULL") which.lines <- as.integer(ctx$op.value('which.lines'))

# extract files
df <- ctx$cselect()

### Function now takes 1 file in the workflow. Cant handle 2 files as it only takes 1 see #36 docId = df$documentId[1]
docId = df$documentId[1]
doc = ctx$client$fileService$get(docId)
filename = tempfile()
writeBin(ctx$client$fileService$download(docId), filename)
on.exit(unlink(filename))

# unzip if archive

if(length(grep(".zip", doc$name)) > 0) {
  tmpdir <- tempfile()
  unzip(filename, exdir = tmpdir)
  # Check if the unzipped directory contains fcs files.
  if(length(grep(".fcs", list.files(tmpdir))) == 0){
    # Search 1 directory deeper on first file
    deeperdir = list.files(tmpdir, full.names = TRUE)[1]
    ifelse(length(grep(".fcs", list.files(deeperdir))) == 0,
    stop("No FCS files found in zipped file."),
    f.names <- list.files(deeperdir, full.names = TRUE))}
  else {f.names <- list.files(tmpdir, full.names = TRUE)}
} else { f.names <- filename
  }

# check FCS
if(any(!isFCSfile(f.names))) stop("Not all imported files are FCS files.")

### make it so that they exclude the non-FCS files? Had some datasets with readmes in them.

assign("actual", 0, envir = .GlobalEnv)
task = ctx$task


# convert them to FCS files
f.names %>%
  lapply(function(filename){
    data = fcs_to_data(filename, which.lines)
    if (!is.null(task)) {
      # task is null when run from RStudio
      actual = get("actual",  envir = .GlobalEnv) + 1
      assign("actual", actual, envir = .GlobalEnv)
      evt = TaskProgressEvent$new()
      evt$taskId = task$id
      evt$total = length(f.names)
      evt$actual = actual
      evt$message = paste0('processing FCS file ' , filename)
      ctx$client$eventService$sendChannel(task$channelId, evt)
    } else {
      cat('processing FCS file ' , filename)
    }
    data
  }) %>%
  bind_rows() %>%
  ctx$addNamespace() %>%
  ctx$save()

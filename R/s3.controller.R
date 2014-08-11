check.bucket <- function(bucket) {
  if(!is.character(bucket)) {
    stop("bucket is not character.")
  }
  if(length(bucket)!=1L) {
    stop("bucket has length > 1.")
  }
}


#' Make Bucket Function
#'
#' This function creates a bucket on S3.
#' @param bucket Name of bucket to create. REQUIRED
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' s3.mb("mybucket")
#' ## End(**Not run**)
s3.mb <- function(bucket) {
  s3.cmd <- paste0("aws s3 mb s3://", bucket)
  system(s3.cmd,intern=TRUE)
}

#' Remove Bucket Function
#'
#' Creates a bucket on S3.
#' @param bucket Name of bucket to remove. REQUIRED
#' @param force Boolean (Default FALSE). Deletes all objects in the bucket including the bucket itself.
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' s3.rb("mybucket")
#' ## End(**Not run**)
s3.rb <- function(bucket, force=FALSE) {
  check.bucket(bucket)
  s3.cmd <- paste0("aws s3 rb s3://", bucket,
                  ifelse(force," --force",""))
  system(s3.cmd,intern=TRUE)
}

#' List Bucket Function
#'
#' List buckets or files within a specified bucket on S3.
#' @param path S3 Path/name of bucket. Optional
#' @param recursive Boolean (Default FALSE). Command is performed on allfiles or objects under the specified bucket.
#' @param filters character string indicating filters.
#' @details
#' filters: a character string See AWS documentaion for the filters usage.
#' \code{\link{http://docs.aws.amazon.com/cli/latest/reference/s3/index.html#use-of-exclude-and-include-filters}}
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' # list all file and directories under mybucket
#' s3.ls("mybucket")
#' 
#' # list all bucket on S3
#' s3.ls()
#' 
#' # list all all files under the folder_test which reside inside mybucket
#' s3.ls("mybucket/folfer_test")
#' ## End(**Not run**)
s3.ls <- function(bucket=NULL, recursive = FALSE, filters = NULL) {
    s3.cmd <- paste0("aws s3 ls s3://", bucket)
  
  if (recursive) s3.cmd <- paste(s3.cmd, "--recursive")
  if (!is.null(filters)) s3.cmd <- paste(s3.cmd, filters, sep = " ")
  system(s3.cmd,intern=TRUE)
}

#' Copy function
#'
#' Copies a local file or S3 object to another location locally or in S3.
#' s3.cp <LocalPath> <S3Path> or <S3Path> <LocalPath> or <S3Path> <S3Path>
#' @param from.path Path of file to copy from.
#' @param to.path Path of file to copy to.
#' @param recursive Boolean (Default FALSE). Command is performed on allfiles or objects under the specified bucket.
#' @param ACL Access Control list (Optional).
#' @param filters A string that define the include and/or exclude filters. See details for more info.
#' @details
#' filters: a character string See AWS documentaion for the filters usage. 
#' \code{\link{http://docs.aws.amazon.com/cli/latest/reference/s3/index.html#use-of-exclude-and-include-filters}}
#' ACL: Sets the ACl for the object when the command is performed. Allowed values are: \emph{private, public-read, public-read-write, authenticated-read, bucket-owner-read, bucket-owner-full-control and log-delivery-write}. For more information see \code{\link{http://docs.aws.amazon.com/cli/latest/reference/s3/cp.html}}
#' \strong{NOTE:} 
#' from.path Path and to.path Path must be prefixed with \preformatted{s3://} if moving from/to S3. See examples below.
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' # copy local text.txt to my bucket on S3 to a file name test2.txt
#' s3.cp("test.txt", "s3://mybucket/test2.txt"
#' 
#' # copying file from S3 to S3
#' s3.cp("s3://mybucket/test.txt", "s3://mybucket/tes2t.txt")
#' ## End(**Not run**)
s3.cp <- function(from.path, to.path, recursive = FALSE, ACL = NULL, filters = NULL, options = NULL) {
  s3.cmd <- paste("aws s3 cp", from.path, to.path)
  if (recursive) s3.cmd <- paste(s3.cmd, "--recursive")
  if (!is.null(ACL)) s3.cmd <- paste(s3.cmd, "--acl", ACL)
  if (!is.null(filters)) s3.cmd <- paste(s3.cmd, filters)
  
  system(s3.cmd,intern=TRUE)
}


#' Move function
#'
#' Move a local file or S3 object to another location locally or in S3.
#' s3.mv <LocalPath> <S3Path> or <S3Path> <LocalPath> or <S3Path> <S3Path>.
#' @param from.path Path of file to copy from.
#' @param to.path Path of file to copy to.
#' @param recursive Boolean (Default FALSE). Command is performed on allfiles or objects under the specified bucket.
#' @param ACL Access Control list (Optional).
#' @param filters A string that define the include and/or exclude filters. See details for more info.
#' @details
#' filters: a character string See AWS documentaion for the filters usage. 
#' \code{\link{http://docs.aws.amazon.com/cli/latest/reference/s3/index.html#use-of-exclude-and-include-filters}}.
#' ACL: Sets the ACl for the object when the command is performed. Allowed values are: \emph{private, public-read, public-read-write, authenticated-read, bucket-owner-read, bucket-owner-full-control and log-delivery-write}.
#' For more information see \code{\link{http://docs.aws.amazon.com/cli/latest/reference/s3/mv.html}}
#' \strong{NOTE:} 
#' from.path Path and to.path Path must be prefixed with \preformatted{s3://} if moving from or to S3. See examples below.
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' # move local text.txt to my bucket on S3 to a file name test2.txt
#' s3.mv("test.txt", "s3://mybucket/test2.txt")
#' 
#' # move file from one S3 bucket to another S3 bucket while retaining its original name
#' s3.mv("s3://mybucket/test.txt", "s3://mybucket2/")
#' 
#' # move ALL files from S3 bucket(mybucket) to current local folder
#' s3.mv("s3://mybucket", ".", recursive = TRUE)
#' ## End(**Not run**)
s3.mv <- function(from.path, to.path, recursive = FALSE, ACL = NULL, filters = NULL) {
  s3.cmd <- paste("aws s3 mv", from.path, to.path)
  if (recursive) s3.cmd <- paste(s3.cmd, "--recursive")
  if (!is.null(ACL)) s3.cmd <- paste(s3.cmd, "--acl", ACL)
  if (!is.null(filters)) s3.cmd <- paste(s3.cmd, filters)
  
  system(s3.cmd,intern=TRUE)
}


#' remove Bucket Function
#'
#' Delete files within a specified bucket on S3; or entire bucket (if empty or recursive set to TRUE).
#' @param S3 object (bucket of file).
#' @param recursive Boolean (Default FALSE). Command is performed on allfiles or objects under the specified bucket.
#' @param filters character string indicating filters.
#' @details
#' filters: a character string See AWS documentaion for the filters usage.
#' \code{\link{http://docs.aws.amazon.com/cli/latest/reference/s3/index.html#use-of-exclude-and-include-filters}}
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' # list all file and directories under mybucket
#' s3.rm("mybucket", is.bucket = TRUE, recursive = TRUE)
#' 
#' # rm text2.txt which is located under S3 bucket (mybucket)
#' s3.rm("mybucket/test2.txt")
#' ## End(**Not run**)
s3.rm <- function(s3.object, is.bucket = FALSE, recursive = FALSE, filters = NULL) {
  s3.cmd <- paste0("aws s3 rm s3://", s3.object)
  
  if (is.bucket) check.bucket(s3.object)
  if (recursive) s3.cmd <- paste(s3.cmd, "--recursive")
  if (!is.null(filters)) s3.cmd <- paste(s3.cmd, filters)
  
  system(s3.cmd,intern=TRUE)
}

#' Sync function
#'
#' Syncs directories and S3 prefixes.
#' s3.sync <LocalPath> <S3Path> or <S3Path> <LocalPath> or <S3Path> <S3Path>.
#' @param from.path Path to sync from.
#' @param to.path Path to sync to.
#' @param delete Boolean (Default FALSE).
#' @param filters A string that define the include and/or exclude filters. See details for more info.
#' @details
#' filters: a character string See AWS documentaion for the filters usage. 
#' \code{\link{http://docs.aws.amazon.com/cli/latest/reference/s3/index.html#use-of-exclude-and-include-filters}}.
#' delete: Files that exist in the destination but not in the source are deleted during sync.
#' For more information see \code{\link{http://docs.aws.amazon.com/cli/latest/reference/s3/sync.html}}
#' \strong{NOTE:} 
#' from.path Path and to.path Path must be prefixed with \preformatted{s3://} if moving from or to S3. See examples below.
#' 
#' A file (or s3 object) will be copied (upload/download) if the size of the file (in from.path) is different than the size of the file in the to.path; the last modified time of the local file is newer than the last modified time of file in the to.path; or the file in the to.path does not exist.
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' # sync content of local folder to a specified S3 bucket
#' s3.sync(".", "s3://mybucket")
#' 
#' sync content S3 bucket to local folder
#' s3.sync("s3://mybucket/test.txt", ".")
#' 
#' # In the following example, the user syncs the bucket mybucket to the local current directory. The local current directory contains the files test.jpg and test2.txt. The bucket mybucket contains the object test.jpg of a different size than the local test.jpg
#' Because the exclude filters is used, all files matching the pattern existing both in s3 and locally will be excluded from the sync.
#' s3.sync(".", "s3://mybucket", filters ="--exclude '*.jpg' ")
#' # will results in uploading: test2.txt to s3://mybucket/test2.txt
#' 
#' # In this example, the user syncs the local current directory to the bucket mybucket. The local current directory contains the files test.txt and another/test2.txt. The bucket mybucket contains the object test1.txt and another/test5.txt:
#' s3.sync("s3://mybucket/", ".", filters = "--exclude '*another/*' "
#' # Will results in downloading s3://mybucket/test1.txt to test1.txt
#' ## End(**Not run**)
s3.sync <- function(from.path, to.path, recursive = FALSE, ACL = NULL, filters = NULL, delete = FALSE) {
  s3.cmd <- paste("aws s3 sync", from.path, to.path)
  if (!is.null(filters)) s3.cmd <- paste(s3.cmd, filters)
  if (delete) s3.cmd <- paste(s3.cmd, "--delete")
  
  system(s3.cmd,intern=TRUE)
}
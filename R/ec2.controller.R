#' Add aws PATH
#'
#' @details Currently, this is a OS X and linux based function only; does not work for windows.  This function adds the aws bin to the enviroment variable PATH.  For this function to work correctly, ensure that you set up the config file correctly, which is contained in your \code{.aws} folder (by default the \code{.aws} folder should be set up at \code{~/.aws}).  Specifically, in your config file, you must specify the \code{aws_access_key_id}, \code{aws_secret_access_key} and the default \code{region}. The Default region, \code{aws_access_key_id}, and the \code{aws_secret_access_key}, can be specified by setting the enviroment variables \code{AWS_DEFAULT_REGION}, \code{AWS_ACCESS_KEY_ID}, and \code{AWS_SECRET_ACCESS_KEY}, respectively (e.g. Sys.setenv(AWS_DEFAULT_REGION = "us-east-1"))
#' @param path.to.aws.bin The path to aws bin path.
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' add.aws.path("~/.local/lib/aws/bin")
#' ## End(**Not run**)
add.aws.path <- function(path.to.aws.bin) {
  Sys.setenv(PATH=paste(Sys.getenv("PATH"),path.to.aws.bin, sep = ":"))
}


#' Describes AWS regions
#'
#' @details Describes AWS regions available so that you can specify the region parameters when creating EC2-key pairs, launching instanaces, requesting spot instances, ... etc. Use the value in the ID columns when specifying regions.
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' describe.regions()
#' ## End(**Not run**)
describe.regions <- function(){
  res <- system("aws ec2 describe-regions", intern = TRUE)
  res <- as.data.frame(do.call(rbind, strsplit(res, "\t")))[,-1]
  names(res) <- c("Location", "ID")
  return(res)
}

#' Describes Availability Zone
#'
#' @details Describes Availability Zone in a given region (if specified).
#' @param region.name Name of the aws region. (OPTIONAL). If missing, the default (aws) region is used
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' describe.avail.zone()
#' describe.avail.zone("us-west-2")
#' ## End(**Not run**)
describe.avail.zone <- function(region.name){
  cmd <- "aws ec2 describe-availability-zones"
  if(!missing(region.name)) cmd <- paste(cmd, "--region", region.name)
  res <- system(cmd, intern = TRUE)
  res <- do.call(rbind, strsplit(res, split = "\t"))
  res <- res[,-1]
  colnames(res) <- c("Region", "Status", "AvailabilityZone")
  return(res)
}


#' Request Spot Instances
#'
#' @details Request EC2 Spot instances on AWS. Spot instances are cheaper than On Demand Instances.
#' @param spot.price The maximum bidding price the user is willing to pay.
#' @param ami The AMI ID the user wishes to use to launch the instances with.
#' @param instance.count The number of instances the user wishes to launches
#' @param instance.type The type of the machine the user wishes to launches eg. "t1.micro"
#' @param security.grp (OPTIONAL). If mssing the default security group is applied to instances that are to be launched.
#' @param key.name  Name of the key to launch EC2 instances with; in order to connect to them later through ssh.
#' @param region The region ID (OPTIONAL). If not specified, the default region is used. See AWS config file, or the enviromemt variable \code{AWS_DEFAULT_REGION} for default value.
#' @param root_DeleteOnTermination TRUE(defaults) or FALSE. If set to false, the root volume will NOT be deleted when instance is terminated.
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' req.spot.instances(spot.price = 0.0032, ami = "ami-e84d8480", instance.count = 2, instance.type = "t1.micro", key.name = "MyEC2Key" )
#' ## End(**Not run**)
req.spot.instances <- function(spot.price, ami, instance.count,instance.type, security.grp, key.name, region, root_DeleteOnTermination = TRUE, verbose = FALSE){
  lnch.spec <-  RJSONIO::toJSON(list(ImageId =  ami, InstanceType = instance.type, KeyName = key.name))
  query <- "--query 'SpotInstanceRequests[*].[SpotInstanceRequestId,Status.UpdateTime,Status.Code]'"
  cmd <- paste0("aws ec2 request-spot-instances --spot-price '", spot.price,"' --instance-count ", instance.count ,
                " --type 'one-time' --launch-specification '", lnch.spec, "'" )
  
  cmd <- paste(cmd, query)
  if (!missing(security.grp)) cmd <- paste(cmd, "--security-groups", security.grp)
  if(!missing(region)) cmd <- paste0(cmd, " --region '", region, "'")
  if (!root_DeleteOnTermination) cmd <- paste(cmd, 
                                              "--block-device-mappings '[{\"DeviceName\": \"/dev/sda1\",\"Ebs\":{\"DeleteOnTermination\":false}}]'")
  if (verbose == TRUE) print(paste("Command used:", cmd))
  res <- system(cmd, intern = TRUE)
  res <- do.call(rbind, strsplit(res, "\t"))
  colnames(res) <- c("SpotInstanceReqID", "UpdateTime", "OrderState")
  return(res)
}


#' Make the root volume of an instance Persistent
#'
#' @details By default, the root device volume for an AMI backed by Amazon EBS is deleted when the instance terminates. This function changes this default behavior. 
#' @param instance.id Instance ID for which to make the root volume persistent.
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' rootVol.DeleteOnTermination("i-5203422c")
#' ## End(**Not run**)
rootVol.DeleteOnTermination <- function(instance.id){
  if (length(instance.id) > 1) {
    stop("This function works on 1 id at a time")
  }
  cmd <- paste("aws ec2 modify-instance-attribute --instance-id", instance.id,
               "--block-device-mappings '[{\"DeviceName\": \"/dev/sda1\",\"Ebs\":{\"DeleteOnTermination\":false}}]'")
  
  res <- system(cmd, intern = TRUE)
  return(res)
}


#' Describe Spot Instances Request
#'
#' @details Describes EC2 Spot instances Requests that were placed earlier by the user.
#' @param SpotInstanceReqIDs The spot Instances Request IDs (Optional)
#' @return
#' A matrix whose columns represents: Spot Instance Request ID, the Request Time, the specified Bidding Price, the status update time, the status code, the request state, and if the later is active, the Instance ID
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' describe.spot.instanceReq()
#' ## End(**Not run**)
describe.spot.instanceReq <- function(SpotInstanceReqIDs){
  query <- "--query 'SpotInstanceRequests[*].[SpotInstanceRequestId,CreateTime,SpotPrice,Status.UpdateTime,Status.Code,State,InstanceId]'"
  cmd <- "aws ec2 describe-spot-instance-requests"
  if(!missing(SpotInstanceReqIDs)) cmd <- paste(cmd,"--spot-instance-request-ids", paste(SpotInstanceReqIDs, collapse = " "))
  cmd <- paste(cmd, query)
  res<- system(cmd, intern = TRUE)
  res <- do.call(rbind, strsplit(res, "\t"))
  colnames(res) <- c("SpotInstanceReqID", "CreationTime","SpotPrice", "StatusUpdateTime", "StatusCode", "OrderState", "InstanceID")
  return(res)
}

#' Cancel Spot Instances Request
#'
#' @details Cancel EC2 Spot instances Requests.
#' @param SpotInstanceReqIDs The spot Instances Request IDs to be cancelled.
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' cancel.spot.instanceReq(SpotInstanceReqIDs = c("sir-9a80844f", "sir-b06d364f"))
#' ## End(**Not run**)
cancel.spot.instanceReq <- function(SpotInstanceReqIDs){
  cmd <- paste("aws ec2 cancel-spot-instance-requests --spot-instance-request-ids", 
               paste(SpotInstanceReqIDs, collapse = " "))
  res <- system(cmd, intern = TRUE)
  res <- do.call(rbind, strsplit(res, "\t"))
  res <- res[,-1]
  colnames(res) <- c("SpotInstanceReqID", "State")
  return(res)
}

#' Create EC2 Key Pairs
#'
#' @details Create an EC2 key pairs and saves it to the \code{.ssh} folder located in your home directory. EC2 key is need to be able to Launch and connect to an instance on AWS cloud. If no Key pairs is used when launching the instances, the user will not be able to ssh login into his/her instance.
#' @param key.name Name of the key pair that will be created.
#' @param region (OPTIONAL) if missing, the default region (defined part of the enviroement variable, or aws config file) is used
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' create.ec2.key("MyEC2Key")
#' ## End(**Not run**)
create.ec2.key <- function(key.name, region) {
  file.name <- paste0(key.name,".pem")
  if(file.exists(paste0("~/.ssh/",file.name))) 
    stop("A file with this name already exist in ~/.ssh directory. Please choose a different name")
  cmd <- paste0("aws ec2 create-key-pair --key-name ", key.name, " --query 'KeyMaterial' --output text > ~/.ssh/", file.name)
  if(!missing(region)) cmd <- paste0(cmd, " --region '", region, "'")
  res <- system(cmd, intern = TRUE)
  if (length(res) == 0) return ("Key created")
  else return(res)
}

#' Upload Public Key
#'
#' @details Upload a public key from the user machine to the AWS cloud to be used to launch/connect to an EC2 instance.
#' @param key_name Name of the key pair that will be created on AWS
#' @param path_public_key The path to the public key file (e.g "~/.ssh/id_rsa.pub")
#' @param region (OPTIONAL) The name of the region to upload the key too 
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' upload.key("~/.ssh/id_rsa.pub", "awsKey")
#' ## End(**Not run**)
upload.key <- function(path_public_key, key_name, region) {
  cmd <- paste0("aws ec2 import-key-pair --key-name ", key_name, " --public-key-material file://", path_public_key)
  if (!missing(region)) cmd <- paste(cmd, "--region", region)
  system(cmd, intern = TRUE)
}

#' Delete EC2 Key Pairs
#'
#' @details Delete an EC2 key pairs from AWS and removes it from \code{.ssh} folder if it exist.
#' @param key.name Name of the key pair to delete.
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' delete.ec2.key("MyEC2Key")
#' ## End(**Not run**)
delete.ec2.key <- function(key.name, region) {
  file.name <- paste0("~/.ssh/", key.name,".pem")
  if(file.exists(file.name)) file.remove(file.name)   
  else warning("A file with this name does NOT exist in ~/.ssh directory.")
  cmd <- paste0("aws ec2 delete-key-pair --key-name ", key.name)
  if(!missing(region)) cmd <- paste0(cmd, " --region '", region, "'")
  res <- system(cmd, intern = TRUE)
  if (length(res) == 0) return ("Key deleted")
  else return(res)
}

#' Get Instance Spot Price
#'
#' @details Get the most recent spot price for a type of instance.
#' @param instance.type Type of instance desired (eg m1.xlarge).
#' @param region (OPTIONAL) If not specified, default region is used.
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' get.spot.price("cr1.8xlarge")
#' ## End(**Not run**)
get.spot.price <- function(instance.type = "cr1.8xlarge", region){
  cmd <-paste0("aws ec2 describe-spot-price-history --instance-types ", instance.type, " --filters 'Name=product-description, Values=Linux/UNIX'")
  if (!missing(region)) cmd <- paste(cmd, "--region", region)
  res <- system(cmd, intern = TRUE)
  res <- as.data.frame(do.call(rbind, strsplit(res, "\t")))
  res$V6 <- as.POSIXct(res$V6, format = "%FT%T")
  
  res <- lapply(X = split(res, res$V2), FUN = function(df) {
    df[which.max(df[,6]), c(2,3,5,6) ]
  })
  res <- do.call(rbind, res)
  row.names(res) <- NULL
  names(res) <- c("Region","Instance Type","Bid Price", "Last Update Date Time")
  return(res)
}


#' Describe instances
#'
#' Describe EC2 instances on the AWS cloud.
#' @param region (OPTIONAL) If missing, default region (defined part the enviromement variable or in the AWS config file)is used
#' @keywords AWS.tools
#' @export
#' @return
#' A matrix where the columns are: LanchTime, InstanceId, State.Name, RootDeviceType, RootDeviceName, KeyName and PublicDns Name if available.
#' @examples
#' describe.instances()
describe.instances <- function(region){
  cmd <- paste0("aws ec2 describe-instances ",
                "--query 'Reservations[*].Instances[*].[LaunchTime, InstanceId, State.Name, Placement.AvailabilityZone, RootDeviceType, RootDeviceName, KeyName, PublicDnsName]'")
  if(!missing(region)) cmd <- paste(cmd, "--region", region)

  res <- system(cmd, intern = TRUE)
  if (length(res) > 0) {
    res <- t(matrix(unlist(strsplit(res, "\t")), ncol = length(res)))
    colnames(res) <- c("LaunchTime", "InstanceId", "State.Name", "AvailabilityZone", "RootDeviceType", 
                       "RootDeviceName", "KeyName", "PublicDnsName")
  } else res <- "No instance is available."
  
  return(res)
}

instances.from.reservation <- function(reservation.id, region, verbose=FALSE){
  cmd <- paste0("aws ec2 describe-instances ",
                "--query 'Reservations[*].Instances[*].[LaunchTime, InstanceId, State.Name, PublicDnsName]' ",
                "--filters 'Name=reservation-id, Values=",reservation.id,"'")
  
  if (!is.null(region)) cmd <- paste(cmd, "--region", region)
  
  if(verbose) {
    cat("Using this cmd:\n")
    print(cmd)
  }
  res <- system(cmd, intern = TRUE)
  res <- t(matrix(unlist(strsplit(res, "\t")), ncol = length(res)))
  res <- cbind(reservation.id, res)
  colnames(res) <- c("reservationId","LaunchTime", "InstanceId", "State.Name", "PublicDnsName")
  return(res)
}

instances.status <- function(instances, region, verbose = FALSE){
  cmd <- paste0("aws ec2 describe-instance-status --query 'InstanceStatuses[*].InstanceStatus.Details' ",
                "--instance-id ", paste(instances, collapse = " "))
  if (!is.null(region)) cmd <- paste(cmd, "--region", region)
  if(verbose) {
    cat("Using this cmd:\n")
    print(cmd)
  }
  res <- system(cmd, intern = TRUE)
  if (length(res) > 0) {
    res <- t(matrix(unlist(strsplit(res, "\t")), ncol = length(res)))
  } else {
    res <- matrix(, nrow = length(instances), ncol = 2)
    res[, 1] <- 'reachability'
    res[, 2] <- 'NA'
  }
  colnames(res) <- c("Reachability" , "Status")
  return(res)
}

pending.instance <- function(reservation.id, region){
  res1 <- instances.from.reservation(reservation.id, region)
  res2 <- instances.status(res1[, "InstanceId"], region)
  ans <- any(res1[,"State.Name"] == "pending" | res1[,"PublicDnsName"] == "None" | res2[,"Status"] != "passed")
} 

sleep.while.pending <- function(reservation.id,sleep.time=2, region, verbose=TRUE) {
  while(pending.instance(reservation.id, region)) {
    if(verbose) { cat(".") }
    Sys.sleep(sleep.time)
  }
  if(verbose) { cat("\n") }
}

#' Start an EC2 cluster
#'
#' Create an EC2 cluster on the AWS cloud.
#' @param ami The image-id for the instance to use.  This will be specific to your account (log into AWS to see what image-id's are available to you).
#' @param instance.count An integer number of instances to start.
#' @param instance.type Type of instance (eg. t1.micro).
#' @param key.name The Name of the key to launch the EC2 instances with in order to connect to them later through ssh.
#' @param region (OPTIONAL) If missing, the default region is used.
#' @param security.grp  (OPTIONAL) Security group to be applied to the cluster. If you don't specify a security group when launching an instance, Amazon EC2 uses the default security group.
#' @param verbose TRUE/FALSE Print to console the command used.
#' @param root_DeleteOnTermination TRUE/FALSE. If set to TRUE (Default); the Root Device Volume is deleted when the instance Terminates/Fails.
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' # starts a cluster of 2 nodes of type t1.micro
#' myCluster <- startCluster(ami = "ami-80778be8", instance.count = 2, instance.type = "t1.micro")
#' ## End(**Not run**)
startCluster <- function(ami, instance.count,instance.type, security.grp, key.name, region, verbose=FALSE, root_DeleteOnTermination = TRUE) {
  cmd <- paste("aws ec2 run-instances --image-id", ami,
               "--count",instance.count,
               "--instance-type",instance.type,
               "--key-name", key.name)
  
  if (!missing(security.grp)) cmd <- paste(cmd, "--security-groups", security.grp)
  if(!missing(region)) cmd <- paste0(cmd, " --region '", region, "'")
  if (!root_DeleteOnTermination) cmd <- paste(cmd, 
                                              "--block-device-mappings '[{\"DeviceName\": \"/dev/sda1\",\"Ebs\":{\"DeleteOnTermination\":false}}]'")
  
  if(verbose) {
    cat("Using this cmd:\n")
    print(cmd)
  }
  res <- system(cmd,intern=TRUE)
  reservation <- strsplit(res[[1]],split="\t")[[1]][-1]
  
  if (missing(region)) region <- NULL
  sleep.while.pending(reservation.id = reservation[1],region = region)
  ans <- instances.from.reservation(reservation[1], region)
  class(ans) <- "ec2.cluster"
  ans
}

get.instances.from.cluster <- function(cluster) {
  cluster[, "InstanceId"]
}

#' Stop EC2 instance(s) using instance IDs
#'
#' Stops one or more chosen instances currently running on EC2 using instance IDs.
#' @param instance.ids A character vector specifying instance ids that will be stopped.
#' @param region (OPTIONAL) If not specified, default region is used.
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run: 
#' # Stop one instance with id "i-41df9c6d"
#' ec2stop.instances(instance.ids = "i-41df9c6d")
#' 
#' # Stop two instances
#' ec2stop.instances(instance.ids = c("i-41df9c6d","i-41eg1e3d"))
#' ## End(**Not run**)
ec2stop.instances <- function(instance.ids, region) {
  cmd <- paste("aws ec2 stop-instances --query 'StoppingInstances[*].CurrentState.Name'",
               "--instance-ids ", paste(instance.ids,collapse=" "))
  if (!missing(region))
    if (!is.null(region)) cmd <- paste(cmd, "--region", region)
  res <- system(cmd,intern=TRUE)
  res <- t(matrix(unlist(strsplit(res, "\t")), ncol = length(instance.ids)))
  res <- cbind(instance.ids, res)
  colnames(res) <- c("InstanceId", "CurrentState.Name")
  return(res)
}

#' Terminate EC2 instances using instance IDs
#' 
#' Terminates one or more chosen instances currently running on EC2.
#' @param instance.ids a vector of the desired instances id that you wish to stop.
#' @param region (OPTIONAL) If not specified, default region is used.
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#'  # Terminate one instance with id "i-41df9c6d"
#' ec2terminate.instances(instance.ids = "i-41df9c6d")
#' ## End(**Not run**)
ec2terminate.instances <- function(instance.ids, region) {
  cmd <- paste0("aws ec2 terminate-instances --query 'TerminatingInstances[*].CurrentState.Name' ",
                "--instance-ids ",paste(instance.ids,collapse=" "))
  if (!missing(region))
      if (!is.null(region)) cmd <- paste(cmd, "--region", region)
  res <- system(cmd,intern=TRUE)
  res <- t(matrix(unlist(strsplit(res, "\t")), ncol = length(instance.ids)))
  res <- cbind(instance.ids, res)
  colnames(res) <- c("InstanceId", "CurrentState.Name")
  return(res)
}

#' Stop an EC2 cluster
#'
#' Stop a specified cluster on EC2.
#' @param cluster An \code{ec2.cluster} object created with startCluster function.
#' @param region (OPTIONAL) If not specified, default region is used.
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' myCluster <- startCluster(ami = "ami-80778be8", instance.count = 2, instance.type = "t1.micro")
#' stopCluster(cluster = myCluster)
#' ## End(**Not run**)
stopCluster <- function(cluster, region) {
  if(missing(region)) region <- NULL
  ec2stop.instances(get.instances.from.cluster(cluster), region)
}

#' Terminate an EC2 cluster
#'
#' Terminate a specified cluster on EC2.
#' @param cluster An \code{ec2.cluster} object created with startCluster function.
#' @param region (OPTIONAL) If not specified, default region is used.
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' myCluster <- startCluster(ami = "ami-80778be8", instance.count = 2, instance.type = "t1.micro")
#' terminateCluster(cluster = myCluster)
#' ## End(**Not run**)
terminateCluster <- function(cluster, region) {
  if(missing(region)) region <- NULL
  ec2terminate.instances(get.instances.from.cluster(cluster), region)
}

#' Stop EC2 instances using reservation IDs
#'
#' Stop instances on EC2 based on reservation IDs.
#' @param reservation.id A character string.
#' @param region (OPTIONAL) If not specified, default region is used.
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' ec2stop.reservation(reservation.id = "r-eaf8ca9b")
#' ## End(**Not run**)
ec2stop.reservation <- function(reservation.id, region) {
  if(missing(region)) region <- NULL
  tmp <- instances.from.reservation(reservation.id, region)
  ec2stop.instances(tmp[,"InstanceId"], region)
}

#' Terminate EC2 instances using reservation IDs
#'
#' Terminate instances on EC2 based on reservation IDs.
#' @param reservation.id A character string.
#' @param region (OPTIONAL) If not specified, default region is used.
#' @keywords AWS.tools
#' @export
#' @examples
#' ## Not run:
#' ec2terminate.reservation(reservation.id = "r-eaf8ca9b")
#' ## End(**Not run**)
ec2terminate.reservation <- function(reservation.id, region) {
  if(missing(region)) region <- NULL
  tmp <- instances.from.reservation(reservation.id, region)
  ec2terminate.instances(tmp[,"InstanceId"], region)
}

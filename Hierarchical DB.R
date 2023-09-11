# Name: Amit Pai
# Assignment1: Building Hierarchial Document Database


# declaring a global variable called rootDir with value "docDB
rootDir <- "docDB"


# declaring a new function called configDB
# parameters: root, path
# This function sets up all the folders and database related structure
configDB <- function(root, path){
  # check for windows and Mac format
  if(tolower(Sys.info()[['sysname']]) == "windows"){
    seperator <- "\\"
  } else {
    seperator <- "/"
  }
  if (path == ""){
    # create folder if path not given
    dir.create(root)
  } else{
    folder_path <- paste(path, separator, root, sep="")
    dir.create(folder_path)
  }
}


# declaring a new function called genObjPath(root, tag)
# parameters: root, tag
# returns correctly generated path to a tag folder
genObjPath <- function(root, tag){
  # split the string where # is found
  strsplit(tag, "#")
  # get the string value after the #
  new_string <- c(sub('.*#','', tag))
  # return the tag name
  if(tolower(Sys.info()[['sysname']]) == "windows"){
    return (paste0("docDB\\", new_string))
  }
  return(paste0("docDB/", new_string))
}



# declaring a new function getTags
# parameters: filename
# returns the tag name
getTags <- function(filename){
  new_name <- strsplit(filename, ".#")
  # Going to check for the windows and Mac format
  if(tolower(Sys.info()[['sysname']]) == "windows"){
    # separate each character and get the last character value
    new_tag <- unlist(new_name)[-1]
    l <- length(new_tag)
    tag_name <- unlist(strsplit(new_tag[l], "\\."))[-2]
    new_tag[l] <- tag_name
    new_var <- paste('#', new_tag, sep='')
    return (as.vector(new_var, mode = "any"))
  } else {
    new_tag <- unlist(new_name)[-1]
    new_var <- paste('#', new_tag, sep='')
    return (as.vector(new_var, mode = "any"))
  }
}



# declaring a new function getFileName
# parameters: filename
# returns the filename
getFileName <- function(filename){
  # check for windows format
  if(tolower(Sys.info()[['sysname']]) == "windows"){
    new_name <- gsub("\\#.*",'', filename)
    file_extn <- unlist(strsplit(filename, "\\."))[-1]
    return(paste(new_name,".", file_extn, sep=''))
  }else {
    new_name <- gsub("\\#.*",'', filename)
    return(gsub(" ","",new_name))
  }
}


# declaring a new function storeObjs
# parameters: folder, root
# copies files on to respective tag folders
storeObjs <- function(folder, root, verbose = FALSE){
  # get only the filename by calling getFileName
  filename <- getFileName(folder)
  # get only tag names by calling getTags
  tags <- getTags(folder)
  new_string <- ""
  tag_range <- 1:length(tags)
  for(i in tag_range){
    # replace the # with ''
    new_var <- gsub("#","", tags[i])
    if (i < length(tags)){
      new_string <- paste(new_string, new_var,', ', sep="") 
    } else{
      new_string <- paste(new_string, new_var, sep="")
    }
    folder_name <- genObjPath(root, tags[i])
    dir.create(folder_name)
    if(tolower(Sys.info()[['sysname']]) == "windows"){
      seperator <- "\\"
    } else {
      seperator <- "/"
    }
    file_path <- file.path(paste(folder_name, seperator, filename,sep=''))
    file.create(file_path)
  }
  new_string <- paste("Copying", filename, "to", new_string, sep=" ")
  if(verbose){
    print(new_string)
  }
}

# declaring a new function clearDB
# parameters:root
# clears all the subfolders in the root folder while 
# keeping itself intact
clearDB <- function(root){
  # get all the folders in the root
  folder_list <- list.dirs(path = root, 
                           full.names = TRUE, recursive = TRUE)
  if(tolower(Sys.info()[['sysname']]) == "windows"){
    seperator <- "\\"
  } else {
    seperator <- "/"
  }
  for (folder in folder_list){
    # ensure the root folder doesn't get removed
    if (folder == root){
      next
    } 
    file_list <- list.files(folder)
    # Loop through all the files inside the root folder and remove them
    for(file in file_list){
      file.remove(paste(folder, seperator, file, sep=""))
    }
    file.remove(paste(folder))
  }
}


# declaring a new function called main
main <- function(){
  configDB("docDB", getwd())
  print(genObjPath(rootDir, "campus at night.pdf #Northeastern #ISEC"))
  print(getTags("CampusAtNight #Northeastern #ISEC.jpg"))
  print(getFileName("Campus At Night.jpg #Northeastern #ISEC"))
  storeObjs("CampusAtNight.jpg #Northeastern #ISEC", "docDB", TRUE)
  clearDB("docDB")
  
}


main()



quit()

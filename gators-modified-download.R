# A REPEX of Gators download loop with an integrated delay function for GBIF overpinging. 

## Load necessary Libararies + note at time tested versions
library(gatoRs) # pkg version 1.0.0
library(data.table) # pkg version 1.14.8

## Suppose you had a taxonomic table with the fields name1, relation, name2

name_table <- data.table(
  name1 = c("Shortia galacifolia", "Sherwoodia galacifolia", "Sherwoodia galacifolia var. brevistyla", 
            "Shortia rotundifolia", "Schizocodon rotundifolius", "Sherwoodia rotundifolia", "Shortia exappendiculata", "Shortia ritoensis", # Ill truncate this here for simplicity but there are more synonyms for Shortia rotundifolia
            "Shortia uniflora", "Schizocodon uniflorus", "Sherwoodia uniflora",
            "Shortia sinensis", "Sherwoodia sinensis", "Shortia sinensis var. pubinervis", "Shortia sinensis var. sinensis"),
  
  relation = c("HAS_ACCEPTED_NAME", "SYNONYM_OF", "SYNONYM_OF",
               "HAS_ACCEPTED_NAME", "SYNONYM_OF", "SYNONYM_OF", "SYNONYM_OF", "SYNONYM_OF",
               "HAS_ACCEPTED_NAME", "SYNONYM_OF", "SYNONYM_OF",
               "HAS_ACCEPTED_NAME", "SYNONYM_OF", "SYNONYM_OF", "SYNONYM_OF"),
  name2 = c("Shortia galacifolia", "Shortia galacifolia", "Shortia galacifolia",
            "Shortia rotundifolia", "Shortia rotundifolia", "Shortia rotundifolia", "Shortia rotundifolia", "Shortia rotundifolia",
            "Shortia uniflora", "Shortia uniflora", "Shortia uniflora",
            "Shortia sinensis", "Shortia sinensis", "Shortia sinensis", "Shortia sinensis")
)

## Alternatively, we can think of this table as a nested list with the 1st item containing the harmonized taxon name (name2 here) and the subsequent items being the synonymous names (name1 if applicable)
accepted_name_v <- unique(name_table$name2) # extract only unique harmonized/accepted names
name_list <- list() # create a holding list variable
for(i in 1:length(accepted_name_v)){
  p <- name_table[name2 == accepted_name_v[[i]]]
  s <- p[!is.na(name1)] # in case there is nothing present
  x <- print(s$name1) # Extract
  a <- print(unique(p$name2)) # Retain unique names
  x <- x[!x %in% a] # Remove name2 entry repeats from the subsequent field.
  name_list[[i]] <- c(a,x) # make the accepted names and subsequent synonym vectors and store them in a list
}

## If we wanted to extract the first value which is our "accepted name" as a double check 
accepted_name_v2 <- sapply(name_list, function(vec) vec[1])
accepted_name_v == accepted_name_v2 # Identical

## Create a list of accepted names with '-' instead of ' ' for file storage purposes
accepted_name_v_filestyle = gsub(" ", "-", accepted_name_v)

## Download: 
### First, due to APIs having issues with overpinging we need a retry function. This function will attempt to download function gators_donwload(), but in the case of failure will retry after a cooldown period. If all attempts fail, an error handler is created denoting the output of this issue 
retry_download <- function(i, retry_count) { # Build a retry_downloader fxn that proceeds to try if an API error occurs
  if (retry_count >= 2) { # Allow up to 11 attempts
    failed_names_holder[[i]] <<- c(accepted_name_v[[i]], "Maximum retries exceeded", format(Sys.time(), "%a %b %d %X %Y")) # Customize the error handler 
    return(NULL) # Null otherwise 
  }
  
  tryCatch({ # Try and Catch errors
    gators.download <- gatoRs::gators_download( # Gators download 
      synonyms.list = name_list[[i]], # Pull from nested list of organized names
      write.file = FALSE,
      gbif.match = "fuzzy",
      idigbio.filter = TRUE,
      limit = 1e+05
    )
    
    return(gators.download) # Return the gators.download
    
  }, error = function(e) { # create error report
    if (e$message != "No records found.") { # If errors besides the following occurs, retry
      Sys.sleep(retry_count*30)  # Apply time-cooling period: half a minute between API tries...
      print(paste("Download attempt", retry_count + 1, "for", accepted_name_v[[i]], "failed. Retrying with delay", print(retry_count*30), "second delay"))
      return(retry_download(i, retry_count + 1))  # Retry the download
    } else {
      failed_names_holder[[i]] <<- c(accepted_name_v[[i]], e, format(Sys.time(), "%a %b %d %X %Y")) # Store other errors in our error handler as well. 
      return(NULL)
    }
  })
}

## Now run the download function with retry 
failed_names_holder <- list() # create a holding list 
for(i in 1:length(accepted_name_v)){
  gators.download <- retry_download(i, 0) # 0 intializes the download count
  path <- "./" # Customize this if you have a desired data dir
  write.table(gators.download,
              paste0(path, accepted_name_v[[i]], ".txt"),
              row.names = FALSE, 
              sep = "\t", 
              quote = FALSE)
  
}

failed_names_table <- as.data.frame(do.call(rbind, failed_names_holder)) # unpack the failed_names_holder into a dataframe

if(nrow(failed_names_table > 0)){ # If there are actually names that failed
  colnames(failed_names_table)[1] <- "name" # Assign column headers
  colnames(failed_names_table)[2] <- "errorType"
  colnames(failed_names_table)[3] <- "systemTime"
  write.table(failed_names_table, # Write this dataframe out as a table
              paste0(path, "failed-names-report", ".txt"),
              row.names = FALSE,
              sep = "\t", 
              quote = FALSE)
  } else{
  print("No failed Names to Report") # else we just print there is nothing to report. 
  }


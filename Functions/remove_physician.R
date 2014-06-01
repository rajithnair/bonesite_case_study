
remove_physician<-function(vesicle_data,nearby_region){ 
  
  ###Subsetting the data with respect to the region
  if(!(is.null(nearby_region))){
    
    vesicle_data<-subset(vesicle_data,vesicle_data$City %in% nearby_region)
    
  }
  
  lowest_PI<-vesicle_data$Popularity.Index[which.min(vesicle_data$Popularity.Index)] ##Finding the lowest popular index 
  temp_data<-subset(vesicle_data, vesicle_data$Popularity.Index == lowest_PI)
  
  if("GP" %in% temp_data$Specialty){ ### Checking whether the lowest PI physician is a GP
    
    index<-which(temp_data$Specialty == "GP") ### Finding the row for that GP
    Removal_Physician_Id<-temp_data$Physician_Id[index[1]] ### Saving the Physician Id of that GP
    return(Removal_Physician_Id)
    
  }else{
    
    Removal_Physician_Id<-temp_data$Physician_Id[1]
    return(Removal_Physician_Id)
    
  }
  
}


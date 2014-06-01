
replace_physician<-function(vesicle_data,city){
  
  ### Subsetting the data with respect to the city
  vesicle_data<-subset(vesicle_data, vesicle_data$City == city)
  
  if("Orthopedic" %in% vesicle_data$Specialty){
    
    vesicle_data<-subset(vesicle_data, vesicle_data$Specialty == "Orthopedic")
    
    ##Finding the Orthopedic with highest Popularity Index
    highest_PI_index<-which.max(vesicle_data$Popularity.Index)
    
    Replace_Physician_Id<-vesicle_data$Physician_Id[highest_PI_index]
    return(Replace_Physician_Id)
    
  }else{
    
    vesicle_data<-subset(vesicle_data, vesicle_data$Specialty == "GP")
    
    ##Finding the Orthopedic with highest Popularity Index
    highest_PI_index<-which.max(vesicle_data$Popularity.Index)
    
    Replace_Physician_Id<-vesicle_data$Physician_Id[highest_PI_index]
    return(Replace_Physician_Id)
    
  }
  
}
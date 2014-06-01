
task2_new<-function(list2,vesicle_physicians,mapping,awardees,opera_physicians,regions){
  
  # Adding City column to list2
  
  for(i in 1:nrow(list2)){
    
    index<-which(vesicle_physicians$Physician_Id == list2$Bonesite_Id[i])
    list2$City[i]<-vesicle_physicians$City[index]
    list2$Popularity_Index[i]<-vesicle_physicians$Popularity.Index[index]
  }
  
  ## Aggregating the number of physicians from each city
  city_list<-table(list2$City)
  
  vesicle_data<-subset(vesicle_physicians, vesicle_physicians$Specialty %in% c("GP","Orthopedic"))
  
  list_index<-1 ### Index for the list2 data frame
  source("https://github.com/rajithnair/bonesite_case_study/blob/master/Functions/replace_physician.R")
  
  ### Preparing List2 from vesicle physicians based on the popularity index
  for(i in 1:nrow(city_list)){
    
    for(k in 1:city_list[i]){
      
      Replace_Physician_Id<-replace_physician(vesicle_data,names(city_list)[i])
      
      ### Getting the Physician's id (INDxxxxx) through the mapping data
      if(Replace_Physician_Id %in% mapping$Physician_Id){
        
        list2$Physician_Id[list_index]<-mapping$Bonesite_Id[which(mapping$Physician_Id == Replace_Physician_Id)]  
        
      }else{
        
        list2$Physician_Id[list_index]<-NA
        
      }
      
      index<-which(vesicle_physicians$Physician_Id == Replace_Physician_Id)
      
      list2$Name[list_index] <- vesicle_physicians$Physician.First.Name[index]
      list2$Surname[list_index] <- vesicle_physicians$Physician.Second.Name[index]
      list2$Specialty[list_index] <- vesicle_physicians$Specialty[index]
      list2$Bonesite_Id[list_index] <- vesicle_physicians$Physician_Id[index]
      list2$City[list_index] <- vesicle_physicians$City[index]
      list2$Popularity_Index[list_index] <- vesicle_physicians$Popularity.Index[index]
      
      
      ### Removing the physician from Vesicle Data
      vesicle_data<-vesicle_data[-(which(vesicle_data$Physician_Id == Replace_Physician_Id)),]   
      
      list_index <- list_index + 1  
    }   
    
  }
  
  source("https://github.com/rajithnair/bonesite_case_study/blob/master/Functions/task1_new.R")
  
  ### Sorting list2 in order to accomodate special promotions and extra calls for physicians from Opera Hospital
  list2<-task1_new(list2,vesicle_physicians,mapping,awardees,opera_physicians,regions)
  
  row.names(list2) <- seq(nrow(list2))
  return(list2)
  
}


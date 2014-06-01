
task1_new<-function(list1,vesicle_physicians,mapping,awardees,opera_physicians,regions){
  
  vesicle_data<-subset(vesicle_physicians, (vesicle_physicians$Physician_Id %in% list1$Bonesite_Id))
  
  ### Adding Popular Index column in the list1
  for(i in 1:nrow(list1)){
    
    index<-which(vesicle_physicians$Physician_Id == list1$Bonesite_Id[i])
    list1$City[i]<-vesicle_physicians$City[index]
    list1$Popularity_Index[i]<-vesicle_physicians$Popularity.Index[index]
  }
  
  
  ### Adding Special Promotion column in the list1
  list1$Special_Promotion<-factor("No",levels=c("Yes","No"))
  
  ### Adding Opera Physicians column in the list
  list1$Opera_Physicians<-factor("No",levels=c("Yes","No"))
  
  ### Creating a subset of vesicle physicians wherein these physicians are in list1
  ### but are not present in the awardees list.
  vesicle_data<-subset(vesicle_physicians, ((vesicle_physicians$Physician_Id %in% list1$Bonesite_Id) & 
                                              !(vesicle_physicians$Physician_Id %in% awardees$Physician_Id)))
  
  source("https://github.com/rajithnair/bonesite_case_study/blob/master/Functions/remove_physician.R")
  
  ### MANIPULATING DATA IN ORDER TO ACCOMODATE PHYSICIANS WITH SPECIAL PROMO
  
  for(i in 1:nrow(awardees)){ ### Adding the awardees to the list1
    
    ### Checking whether any of the awardees are already in the list1
    if(awardees$Physician_Id[i] %in% list1$Bonesite_Id){ 
      
      index<-which(list1$Bonesite_Id == awardees$Physician_Id[i])
      list1$Special_Promotion[index]<-"Yes"
      
      Removal_Physician_Id<-remove_physician(vesicle_data,NULL)
      
      ### Removing the physician from Vesicle Data
      vesicle_data<-vesicle_data[-(which(vesicle_data$Physician_Id == Removal_Physician_Id)),] 
      
      ### Removing the physician from List1
      list1<-list1[-(which(list1$Bonesite_Id == Removal_Physician_Id)),]
      
    }
  }
  
  for(i in 1:nrow(awardees)){ ### Adding the awardees to the list1
    
    ### Adding the awardees who are currently not included in the list1
    if(!(awardees$Physician_Id[i] %in% list1$Bonesite_Id)){
      
      index<-which(vesicle_physicians$Physician_Id == awardees$Physician_Id[i])
      
      
      ## Adding a a new row to the data-frame for accomodating the awardee
      list1[nrow(list1)+1,]<- NA
      
      ### Getting the Physician's id (INDxxxxx) through the mapping data
      if(vesicle_physicians$Physician_Id[index] %in% mapping$Physician_Id){
        
        list1$Physician_Id[nrow(list1)]<-mapping$Bonesite_Id[which(mapping$Physician_Id == vesicle_physicians$Physician_Id[index])]  
        
      }
      
      list1$Name[nrow(list1)] <- vesicle_physicians$Physician.First.Name[index]
      list1$Surname[nrow(list1)] <- vesicle_physicians$Physician.Second.Name[index]
      list1$Specialty[nrow(list1)] <- vesicle_physicians$Specialty[index]
      list1$Bonesite_Id[nrow(list1)] <- vesicle_physicians$Physician_Id[index]
      list1$City[nrow(list1)] <- vesicle_physicians$City[index]
      list1$Popularity_Index[nrow(list1)] <- vesicle_physicians$Popularity.Index[index]
      list1$Special_Promotion[nrow(list1)] <- "Yes"
      list1$Opera_Physicians[nrow(list1)] <- "No"
      
      ### Removing two physicians while adding one awardee
      for(k in 1:2){
        
        Removal_Physician_Id<-remove_physician(vesicle_data,NULL)
        
        ### Removing the physician from Vesicle Data
        vesicle_data<-vesicle_data[-(which(vesicle_data$Physician_Id == Removal_Physician_Id)),] 
        
        ### Removing the physician from List1
        list1<-list1[-(which(list1$Bonesite_Id == Removal_Physician_Id)),]
        
      }
    }
  }
  
  ### ADDING THE OPERA HOSPITAL PHYSICIANS TO THE LIST1
  
  for(i in 1:nrow(opera_physicians)){ ### Adding the awardees to the list1
    
    ### Adding the awardees who are currently not included in the list1
    if(!(opera_physicians$Physician_Id[i] %in% list1$Bonesite_Id)){
      
      index<-which(vesicle_physicians$Physician_Id == opera_physicians$Physician_Id[i])
      
      
      ## Adding a a new row to the data-frame for accomodating the awardee
      list1[nrow(list1)+1,]<- NA
      
      ### Getting the Physician's id (INDxxxxx) through the mapping data
      if(vesicle_physicians$Physician_Id[index] %in% mapping$Physician_Id){
        
        list1$Physician_Id[nrow(list1)]<-mapping$Bonesite_Id[which(mapping$Physician_Id == vesicle_physicians$Physician_Id[index])]  
        
      }
      
      list1$Name[nrow(list1)] <- vesicle_physicians$Physician.First.Name[index]
      list1$Surname[nrow(list1)] <- vesicle_physicians$Physician.Second.Name[index]
      list1$Specialty[nrow(list1)] <- vesicle_physicians$Specialty[index]
      list1$Bonesite_Id[nrow(list1)] <- vesicle_physicians$Physician_Id[index]
      list1$City[nrow(list1)] <- vesicle_physicians$City[index]
      list1$Popularity_Index[nrow(list1)] <- vesicle_physicians$Popularity.Index[index]
      list1$Special_Promotion[nrow(list1)] <- "No"
      list1$Opera_Physicians[nrow(list1)] <- "Yes"
      
      ### Finding the regions located near to the branch of Opera Hospital
      state<-regions$State.Territory[which(regions$City == vesicle_physicians$City[index])]
      
      if(state == "Punjab"){
        
        target_region<-subset(regions, regions$State.Territory %in% c(state,"Haryana"))  
        
      }else{
        
        target_region<-subset(regions, regions$State.Territory == state)
        
      }
      
      ### Removing two physicians while adding one awardee
      for(k in 1:2){
        
        Removal_Physician_Id<-remove_physician(vesicle_data,target_region$City)
        
        ### Removing the physician from Vesicle Data
        vesicle_data<-vesicle_data[-(which(vesicle_data$Physician_Id == Removal_Physician_Id)),] 
        
        ### Removing the physician from List1
        list1<-list1[-(which(list1$Bonesite_Id == Removal_Physician_Id)),]
        
      }
    }
  } 
  
  row.names(list1) <- seq(nrow(list1))
  return(list1)
  
}








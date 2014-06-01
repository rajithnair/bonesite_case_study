Bonesite_Task<-function(){
  
  list0<-read.csv("https://raw.githubusercontent.com/rajithnair/bonesite_case_study/master/Data/TargetList-PrevYear.csv",header=T,colClasses="character")
  
  mapping<-read.csv("https://raw.githubusercontent.com/rajithnair/bonesite_case_study/master/Data/Mapping.csv",header=T,colClasses="character")
  
  suppressWarnings(eval_list<-merge(list0,mapping, by.x = "Physician_Id", by.y = "Bonesite_Id"))
  duplicate_index<-which(colnames(eval_list) == "Physician_Id")
  colnames(eval_list)[duplicate_index[2]]<-"Bonesite_Id" 
  
  vesicle_physicians<-read.csv("https://raw.githubusercontent.com/rajithnair/bonesite_case_study/master/Data/Vesicle-Physician.csv",header=T,colClasses="character")
  
  awardees<-read.csv("https://raw.githubusercontent.com/rajithnair/bonesite_case_study/master/Data/Vesicle-Award.csv",header=T,colClasses="character")
  
  opera_physicians<-read.csv("https://raw.githubusercontent.com/rajithnair/bonesite_case_study/master/Data/FourHospitals.csv",header=T,colClasses="character")
  
  regions<-read.csv("https://raw.githubusercontent.com/rajithnair/bonesite_case_study/master/Data/CityNames.csv",header=T,colClasses="character")
  
  source("https://raw.githubusercontent.com/rajithnair/bonesite_case_study/master/Functions/task1_new.R")
  list1<-task1_new(eval_list,vesicle_physicians,mapping,awardees,opera_physicians,regions)
  View(list1)
  write.csv(list1,file ="List1.csv",row.names=F)
  
  source("https://raw.githubusercontent.com/rajithnair/bonesite_case_study/master/Functions/task2_new.R")
  list2<-task2_new(eval_list,vesicle_physicians,mapping,awardees,opera_physicians,regions)
  View(list2)
  write.csv(list2,file ="List2.csv",row.names=F)
  
  specially_promoted_physicians<-subset(list1, list1$Special_Promotion == "Yes")
  physicians_opera<-subset(list1, list1$Opera_Physicians == "Yes")
  
  Revenue<-(sum(as.numeric(list2$Popularity_Index)) - sum(as.numeric(list1$Popularity_Index))) / sum(as.numeric(list1$Popularity_Index))
  Revenue<-Revenue * 100
  
  output_table<- data.frame(matrix(nrow = 5, ncol = 2))
  colnames(output_table)=c("Description","Values")
  
  output_table$Description<- c("Number of physicians with special promotion","Number of physicians working at Opera",
                               "Newly added physicians in List1","Number of physicians in List2",
                               "% Revenue Growth from using List2 instead of List1")
  output_table$Values<-c(nrow(specially_promoted_physicians),nrow(physicians_opera),nrow(specially_promoted_physicians) + nrow(physicians_opera),
                         nrow(list2),round(Revenue,2))
  write.csv(output_table,file="Reported_Values.csv",row.names=F)
  
  
  cat("\n### Please find three files in your working directory: ###\n\nList1.csv: This file contains List1\n\nList2.csv: This file contains List2\n\nReported_Values.csv: This file answers all the reported questions in the case-study pdf\n\n")

}


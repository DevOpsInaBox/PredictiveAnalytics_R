#install.packages("ggplot2",repos = "http://cran.us.r-project.org")
#install.packages("gridExtra",repos = "http://cran.us.r-project.org")
setwd("/var/lib/jenkins/workspace/Predictive_Analytics_R/")
options( warn = -1 )
library(ggplot2)
library(gridExtra)
library(grid)
args<-commandArgs(TRUE)
temp = list.files(pattern="release.*")
for (i in 1:length(temp))
{
  assign(paste0("release",i), read.csv(temp[i]))
}

#Shrinking the 30 releases
output_dataset<-data.frame(Num_Active=integer(),Num_Days_Active=integer(),Num_Reop=integer(),Tot_LOC=integer(),Tot_Days=double(),Tot_Dev_Hrs=double(),Tot_Test_Hrs=double(),Num_Sev_High=integer(),Num_Sev_Med=integer(),Num_Sev_Low=integer());
for (i in 1:30)
{
  release<-get(paste0("release",i))
  x<-data.frame(i,sum(release$numActive),sum(release$numDaysActive),sum(release$numReopened),sum(release$LOC),sum(release$Total.num.days),sum(release$Num.eff.dev.hours),sum(release$Num.eff.testing.hours),nrow(release[release$Severity=='High',]),nrow(release[release$Severity=='Medium',]),nrow(release[release$Severity=='Low',]),nrow(release[release$Priority=='High',]),nrow(release[release$Priority=='Medium',]),nrow(release[release$Priority=='Low',]))
  #x<-cbind("Rel_num"=i,x)
  names(x)<-c("Rel_num","NumActive","NumDaysActive","NumReopened","Tot_LOC","Tot_Days","Tot_Dev_Hrs","Tot_Test_Hrs","Num_Sev_High","Num_Sev_Med","Num_Sev_Low","Num_Priot_High","Num_Priot_Med","Num_Priot_Low")
  output_dataset<-rbind(output_dataset,x)
}

output_model_TotDays<-lm(Tot_Days ~ NumActive+NumReopened+NumDaysActive+Tot_LOC,data = output_dataset)
output_model_DevHrs<-lm(Tot_Dev_Hrs ~ NumActive+NumReopened+NumDaysActive+Tot_LOC,data = output_dataset)
output_model_testHrs<-lm(Tot_Test_Hrs ~ NumActive+NumReopened+NumDaysActive+Tot_LOC,data = output_dataset)
output_model_svr_high_model<-lm(Num_Sev_High ~ NumActive+NumReopened+NumDaysActive+Tot_LOC,data = output_dataset)
output_model_svr_medium_model<-lm(Num_Sev_Med ~ NumActive+NumReopened+NumDaysActive+Tot_LOC,data = output_dataset)
output_model_svr_low_model<-lm(Num_Sev_Low ~ NumActive+NumReopened+NumDaysActive+Tot_LOC,data = output_dataset)
output_model_priot_high_model<-lm(Num_Priot_High ~ NumActive+NumReopened+NumDaysActive+Tot_LOC,data = output_dataset)
output_model_priot_medium_model<-lm(Num_Priot_Med ~ NumActive+NumReopened+NumDaysActive+Tot_LOC,data = output_dataset)
output_model_priot_low_model<-lm(Num_Priot_Low ~ NumActive+NumReopened+NumDaysActive+Tot_LOC,data = output_dataset)

input_data<-data.frame(NumActive=as.integer(args[1]),NumReopened=as.integer(args[4]),NumDaysActive=as.integer(args[2]),Tot_LOC=as.integer(args[3]));
predicted_totDays<-predict(output_model_TotDays,input_data);
predicted_DevHrs<-predict(output_model_DevHrs,input_data);
predicted_testHrs<-predict(output_model_testHrs,input_data);
predicted_svr_high<-predict(output_model_svr_high_model,input_data);
predicted_svr_medium<-predict(output_model_svr_medium_model,input_data);
predicted_svr_low<-predict(output_model_svr_low_model,input_data);
predicted_priot_high<-predict(output_model_priot_high_model,input_data);
predicted_priot_medium<-predict(output_model_priot_medium_model,input_data);
predicted_priot_low<-predict(output_model_priot_low_model,input_data);

print(paste0("Total Number of QA days for the next release is predicted to be : ",predicted_totDays));
print(paste0("Total Number of Dev Hours required for the next release is predicted to be : ",predicted_DevHrs));
print(paste0("Total Number of Test Hours required for the next release is predicted to be : ",predicted_testHrs));
print(paste0("Total Number of High Severity Defects for the next release is predicted to be : ",predicted_svr_high));
print(paste0("Total Number of Low Severity Defects for the next release is predicted to be : ",predicted_svr_low));
print(paste0("Total Number of High Priority Defects for the next release is predicted to be : ",predicted_priot_high));
print(paste0("Total Number of Low Priority Defects for the next release is predicted to be : ",predicted_priot_low));

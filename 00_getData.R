library(openxlsx); library(dplyr)
afd.file <-'/mnt/B/bea_res/Personal/Jen/Autism/AutScanData_Demos.xlsx'
afd <- read.xlsx(afd.file) 
afd$Handedness[is.na(afd$Handedness)] <- 'NA'
names(afd)[1] <- 'Subj'
afd.out <- afd %>% 
   mutate(diagg = factor(Group1td2aut,levels=1:2,labels=c('TD','ASD')),
          ageg  = factor(AgeGroup1kid2teen3adult,levels=1:3,labels=c('Child','Teen','Adult'))
         ) %>%
   select(-Group1td2aut, -AgeGroup1kid2teen3adult)
write.table(afd.out,sep="\t",row.names=F,file="txt/demog.txt",quote=F)

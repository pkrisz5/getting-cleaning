setwd("D:/DataScientisToolbox/3/3_5")
library(dplyr)
# Load and combine datasets
train<-read.table(file="UCI HAR Dataset/train/X_train.txt", quote="\"")
trains<-read.table(file="UCI HAR Dataset/train/subject_train.txt", quote="\"")
trainy<-read.table(file="UCI HAR Dataset/train/y_train.txt", quote="\"")
train<-cbind(trains,x=trainy, train )

test<-read.table(file="UCI HAR Dataset/test/X_test.txt", quote="\"")
tests<-read.table(file="UCI HAR Dataset/test/subject_test.txt", quote="\"")
testy<-read.table(file="UCI HAR Dataset/test/y_test.txt", quote="\"")
test<-cbind(tests,x=testy, test )
d<-rbind(test, train)

#Select feature with mean and SD values
features<-read.table(file="UCI HAR Dataset/features.txt", quote="\"", row.names = 1)
s<-grep("mean|std",features$V2)

k<-c(1,2,(s+2))
d<-d[,k]
names(d)<-c('volunteer', 'activity', as.character(features[s,'V2']))

act<-read.table(file="UCI HAR Dataset/activity_labels.txt", quote="\"")

d_merged<-merge(d, act, by.x = "activity", by.y = "V1", all=TRUE)
d_fin<-select(d_merged, volunteer, V2, (3:81)) %>%
  rename(activity=V2) %>%
  arrange(volunteer, activity)
write.table(d_fin, file="tidy_data.txt", sep="\t")

g<-mutate(d_fin, group=paste(volunteer, activity, sep="-")) %>%
  select(group, (3:81))
m<-split(g, g$group)   
fin<-sapply(m, function(x) colMeans(x[,names(g)[-1]]))
fin_t<-t(fin)
fin_t<-as.data.frame(fin_t)
fin_t<-mutate(fin_t, volunteer_action=row.names(fin_t)) %>%
  select(volunteer_action, 1:79)
write.table(fin_t, file="tidy_data_averages.txt", sep="\t", row.names = FALSE)

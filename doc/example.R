#!/share/bin/Rscript

library(hefceref)

#load data
options(stringsAsFactors=FALSE)
ref <- read.csv("REF_outputs.csv", header=T)
req <- read.csv("Required_outputs.csv",header=T)

######
# PUBLICATIONS:

# these are the col names that hefce expects for the ref data. 
# PredictedGrade should be one of "One","Two","Three","Four"
colnames(ref)<-c("AuthorID",
                 "AuthorName",
                 "PaperID",
                 "PaperName",
                 "PredictedGrade")

# col names
colnames(req)<-c("AuthorName",
                 "RequiredOutputs",
                 "AuthorID",
                 "FTE")

# and classes it expects
ref$AuthorID<-as.character(ref$AuthorID)
ref$PaperID<-as.character(ref$PaperID)
ref$PredictedGrade<-ordered(ref$PredictedGrade, levels=c("One","Two","Three","Four"))

# make a hefce.pubs object from this data.
# This has $pubs (a df) $shared and $unique - both character vectors of publication IDs
pubs<-hefce.pubs(ref)



#######
# AUTHORS

# turn the req data into an author list
authors<-hefce.authors(name=req[,"AuthorName"],
                      id=as.character(req[,"AuthorID"]),
                      required.outputs=req[,"RequiredOutputs"],
                      fte=req[,"FTE"])



#######
# GENERATE RESULTS

iter<-50
 
max.money<-0
submission<-NULL
for(i in 1:iter){
  cat("iteration",i,"\n")
  this.list<-init.pubs(authors,pubs)
  res<-optimise(this.list,pubs)
  if(res$sub.max>max.money){
     cat(res$sub.max,"\n")
     max.money<-res$sub.max
     submission<-res$submission
  }  
}

full.list<-submission

sub.list<-1:which.max(submission$percentages[,"money"])
submission$authors<-submission$authors[sub.list]
submission$percentages<-submission$percentages[sub.list,]

write.csv(submission$percentages, file="results/percentages.csv")
write.csv(all.pubs(submission$authors)$pubs, file="results/submission.csv", row.names=F)

# for the best list, we want to know the people who aren't being submitted 
# and the papers that aren't being used
used.pubs<-all.pubs(submission$authors)
used.pubs<-used.pubs[!is.na(used.pubs[,"PaperID"]),]
unused.pubs<-pubs[!pubs[,"PaperID"] %in% used.pubs[,"PaperID"],]


write.csv(unused.pubs$pubs, file="results/unused_pubs.csv", row.names=F)
unused.auths<-submission$unused.authors
# for some reason the submissino doesn't seem to be optimising properly. if I run
# optimise(submission$authors,pubs) it works but then I need to recalc theunused auths
unused.auths<-names(authors)[!names(authors) %in% names(submission$authors)]
unused.auths<-authors[unused.auths] 
write.csv(cbind(ID=unused.auths$id, name=unused.auths$name, required.outputs=unused.auths$required.outputs), file="results/unused_authors.csv",row.names=F)




# publication
hefce.pub<-function(id,title,predicted.grade, module.id=NULL){
 pub<-list(id,title,predicted.grade,module.id)
 class(pub)<-"hefce.pub"
 return(pub)
}
is.hefce.pub<-function(){}

# author 
hefce.author<-function(name, id, pubs){}
is.hefce.author<-function(){}






score<-function(a.id, author.list){
  a.id<-as.character(a.id) # just in case
  n<-req[a.id,"RequiredOutputs"]
  # how many one, two, three and four star papers does this person have?
  grades<-summary(author.list[[a.id]][,"PredictedGrade"])
  # Return NA if this author doesn't have enough papers for a submission
  if(length(grades)==5){return(NA)}
  # One and Two score nothing, three scores 1 and four scores 3
  # final score is total paper score / number of papers submitted
  this.score<-sum(c(0,0,1,3) * grades)/n
  return(this.score)
}


score.all<-function(author.list){
  scores<-sapply(names(author.list), function(x){ return(score(x,author.list)) } )
  names(scores)<-names(author.list)
  return(scores)
}

score.submission<-function(author.list){
  
  # sort the authors by total score
  ord<-order(score.all(author.list), decreasing=TRUE)
  author.list<-author.list[ord]
  
  # remove any NA scores - these have missing papers
  author.list<-author.list[!is.na(score.all(author.list))]
  
  percentages<-lapply(1:length(author.list),
                function(x){
                    these<-do.call(rbind, author.list[1:x])
                    counts<-summary(these[,"PredictedGrade"])
                    return(counts/sum(counts))
                })
  percentages<-do.call(rbind, percentages)
  rownames(percentages)<-names(author.list)
  
  # generate running totals of money this translates to
  money<-1:nrow(percentages) * ((percentages[,"Four"]*23) + (percentages[,"Three"]*7))
  percentages<-cbind(percentages,money)
  return(percentages)
}


assign.shared<-function(id, author.list, shared){

   a.ids<-as.character(shared[shared[,"PaperID"]==id,"AuthorID"])
   a.scores<-sapply(names(author.list), function(x){ return(score(x,author.list)) } )

   a.n<-req[a.ids,"RequiredOutputs"]

   these.authors<-author.list[a.ids]

   # some authors have been removed as they're already scoring optimally, 
   # which means some of these.authors could be NA. Remove them
   keep.ids<-which(!is.na(names(these.authors)))
   these.authors<-these.authors[keep.ids]

   score.improvement<-rep(0,length(these.authors))
   p.grade<-shared[shared[,"PaperID"]==id,][1,"PredictedGrade"]

   replacements<-list()
   #create a table of possible replacement positions:
   for (a in names(these.authors)){
     this.a<-authors[[a]]
     these.grades<-this.a[,"PredictedGrade"]
     na.pos<-which(is.na(these.grades))

     one.pos<-which(as.integer(p.grade) - as.integer(these.grades)==1)
     two.pos<-which(as.integer(p.grade) - as.integer(these.grades)==2)
     three.pos<-which(as.integer(p.grade) - as.integer(these.grades)==3)

     one.pos<-cbind(pos=one.pos, weight=rep(1,length(one.pos)))
     two.pos<-cbind(pos=two.pos, weight=rep(2,length(two.pos)))
     three.pos<-cbind(pos=three.pos, weight=rep(3,length(three.pos)))
     na.pos<-cbind(pos=na.pos, weight=rep(4,length(na.pos)))


     pos<-rbind(one.pos,two.pos,three.pos,na.pos)
     pos<-data.frame(auth.id=rep(a,nrow(pos)),pos)

     replacements[[a]]<-pos

   }
   replacements<-do.call(rbind, replacements)

   if (nrow(replacements)==0){
     return(NA)}


   # convert weights to probabilities and select the replacement on the basis of those
   replacements$weight <- replacements$weight / sum(replacements$weight)
   row<-replacements[sample(1:nrow(replacements), 1, prob=replacements$weight),]
   return(row)
}


assign.all.shared<-function(author.list, shared){

   opt.author.list<-author.list

   # shuffle the list of papers so we don't always assign them in the same order
   papers<-as.character(sample(shared[,"PaperID"], nrow(shared)), replace=FALSE)
   # The list of papers has an entry for each paper for each author, so make sure
   # you're only assigning each paper one:
   papers<-unique(papers)
   for(p in papers){
     r<-assign.shared(p, author.list, shared)
     if(any(is.na(r))){next()}
     this.p<-ref[(ref[,"PaperID"]==p) & (ref[,"AuthorID"]==r$auth.id),]
     opt.author.list[[r$auth.id]][r$pos,]<-this.p
   }

   return(opt.author.list)
}


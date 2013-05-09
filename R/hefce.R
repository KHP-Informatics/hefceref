# publication list:
hefce.pubs<-function(pubs.df){
  # should be a data.frame containing columns AuthorName AuthorID PaperID PredictedGrade 
  if(!is.data.frame(pubs.df)){stop("pubs.df should be a data.frame")}
  if(is.null(pubs.df$AuthorName)){stop("AuthorName column not found")}
  if(!is.character(pubs.df$AuthorName)){stop("AuthorName column not a character vector")}
  if(is.null(pubs.df$AuthorID)){stop("AuthorID column not found")}
  if(!is.character(pubs.df$AuthorID)){stop("AuthorID column not a character vector")}
  if(is.null(pubs.df$PaperID)){stop("PaperID column not found")}
  if(!is.character(pubs.df$PaperID)){stop("PaperID not a character vector")}
  if(is.null(pubs.df$PredictedGrade)){stop("PredictedGrade column not found")}
  if(!is.ordered(pubs.df$PredictedGrade)){stop("PredictedGrade not an Ordered Factor")}

  pubs<-list()
  pubs$pubs<-pubs.df
  
  # Identify shared papers at this point.
  ids<-pubs.df[duplicated(pubs.df$PaperID),"PaperID"]
  dup.ids<-pubs.df$PaperID %in% ids
  pubs$shared<-unique(pubs.df[dup.ids,"PaperID"])
  pubs$unshared<-pubs.df[!dup.ids,"PaperID"]

  class(pubs)<-c("hefce.pubs")
  return(pubs)
}
is.hefce.pubs<-function(x){
  return(inherits(x,"hefce.pubs"))
}
# subsetting - shared and unshared are recalculated relative to the subset.
"[.hefce.pubs"<-function(x,i,j){
  new.pubs<-x$pubs[i,j]
  if(inherits(new.pubs,'data.frame')){new.pubs<-hefce.pubs(new.pubs)}
  return(new.pubs)
}
# dim gets you ncol and nrow
dim.hefce.pubs<-function(x){
  return(c(nrow(x$pubs), ncol(x$pubs)))
}
# concatenate
c.hefce.pubs<-function(..., recursive=FALSE){
  elements<-list(...)
  elements<-lapply(elements, function(x){x$pubs})
  new.pubs<-do.call(rbind,elements)
  new.pubs<-hefce.pubs(new.pubs)
  return(new.pubs)
}
shared <- function(x) UseMethod("shared", x)
unshared <- function(x) UseMethod("unshared",x)
shared.hefce.pubs <- function(x){
  x[which(x$pubs[,"PaperID"] %in% x$shared),]
}
unshared.hefce.pubs <- function(x){
  x[which(x$pubs[,"PaperID"] %in% x$unshared),]
}
# returns a character vector of unique Paper IDs
ids<-function(x) UseMethod("ids",x)
ids.hefce.pubs<-function(x) return(unique(x[,"PaperID"]))

# returns a random shuffle of unique paper ids
shuffle.ids <- function(x) UseMethod("shuffle.ids", x)
shuffle.ids.hefce.pubs <- function(x){
  ids<-ids(x)
  shuf<-sample(1:length(ids), replace=FALSE)
  return(ids[shuf])
}








# authors.  
hefce.authors<-function(name, id, required.outputs, pubs=NULL){ 
  if(is.null(name)){stop("No name found")}
  if(!is.character(name)){stop("Name not character")}
  len<-length(name)
  if(len <1){stop("name should be a vector of length > 0")}
  if(is.null(id)){stop("No ID found")}
  if(!is.character(id)){stop("ID not character")}
  if(length(id) !=len){stop("name and id vectors differ in length")}
  if(any(duplicated(id))){stop("There are duplicate IDs")}
  if(is.null(required.outputs)){stop("required.pubs not supplied")}
  if(length(required.outputs)!=len){ stop("name and required.outputs differ in length") }
  
  if(is.null(pubs)){
    pubs<-lapply(required.outputs, function(x){
      blanks<-rep(NA,x)
      new.pubs<-hefce.pubs(data.frame(AuthorName=as.character(blanks), 
                                   AuthorID=as.character(blanks), 
                                   PaperID=as.character(blanks), 
                                   PaperName=as.character(blanks), 
                                   PredictedGrade=ordered(blanks,levels=c("One","Two","Three","Four")), 
                                   REFModuleID=as.character(blanks)))
      return(new.pubs)
    })    
  }else{
    if(inherits(pubs,"hefce.pubs")){pubs<-list(pubs)} # if its a single object, convert to a list
    if(length(pubs)!=len){stop("name and pubs differ in length")}
    if(!inherits(pubs,c("list"))){stop("pubs should be a list")}
  }


  if(!is.null(pubs) && (length(pubs)!=len)) {stop("name and pubs differ in length")}
  if(is.null(pubs)){pubs=list(); length(pubs)<-len}

  # use IDs as 
  names(name)<-id
  names(id)<-id
  names(required.outputs)<-id
  names(pubs)<-id
  
  auth<-list(name=name,id=id,required.outputs=required.outputs, pubs=pubs)
  class(auth)<-c("hefce.authors")
  return(auth)

}
is.hefce.authors<-function(x){
  return(inherits(x,"hefce.authors"))
}
#length
length.hefce.authors<-function(x){
  length(x$name)
}
#names
names.hefce.authors<-function(x){
  x$id
}
# as.list - returns list of single hefce.author objects.
as.list.hefce.authors<-function(x){
  new.list<-lapply(1:length(x),function(i){x[i]})
  names(new.list)<-x$ids
  return(new.list)
}
# subset
"[.hefce.authors"<-function(x,i){
  name<-x$name[i]
  id<-x$id[i]
  required.output<-x$required.output[i]
  pubs<-x$pubs[i]
  return(hefce.authors(name,id,required.output,pubs))
}
"[[.hefce.authors"<-function(x,i){
  x[i]
}
# concatenate
c.hefce.authors<-function(..., recursive=FALSE){
  elements<-list(...)
  name<-do.call(c,lapply(elements, function(x){x$name}))
  id<-do.call(c,lapply(elements, function(x){x$id}))
  required.outputs<-do.call(c,lapply(elements, function(x){x$required.outputs}))
  pubs<-do.call(c,lapply(elements, function(x){x$pubs}))
  hefce.authors(name,id,required.outputs,pubs)
}

assign.unshared <- function(authors, unshared.pubs) UseMethod("assign.unshared", authors)
assign.unshared.hefce.authors<-function(authors, unshared.pubs){
 
 # assign unshared (returns a list of single authors)
 authors.unshared <- lapply(authors, function(x){
   these<-unshared.pubs[unshared.pubs[,"AuthorID"]==x$id,]
   ord<-order(these[,"PredictedGrade"], decreasing=TRUE)
   these<-these[ord,]
   x$pubs<-list(these[1:x$required.outputs,]) # pubs should be a list of pubs objects, even if we've only got 1 author.
   return(x)
 })
 # and combing results into a single authors object
 authors.unshared<-do.call(c,authors.unshared)
 return(authors.unshared)
}


assign.paper <- function(authors, pubs, a.id, pub.id, pos) UseMethod("assign.paper", authors)
assign.paper.hefce.authors<-function(authors,pubs,a.id,pub.id,pos){

  these.pubs<-authors[[a.id]]$pubs[[1]]$pubs

  this.pub.df<-pubs[pubs[,"PaperID"]==pub.id,]$pubs
  this.pub.df<-this.pub.df[this.pub.df[,"AuthorID"]==a.id,]
  if(nrow(this.pub.df)==0){stop("Not a valid assignment")}

  #create a new pubs df, make is a hefce.pubs object and stick in in the pubs list in authors.
  these.pubs[pos,]<-this.pub.df
  authors$pubs[[a.id]]<-hefce.pubs(these.pubs)
  
  return(authors)
}

assign.shared <- function(authors, shared.pubs) UseMethod("assign.shared", authors)
assign.shared.hefce.authors<-function(authors, shared.pubs){

  # we might not have any shared pub, in which case just return the author and warn
  if(nrow(shared.pubs)==0){
    warning("No shared publications found")
    return(authors)
  }

  # initial assignment aims to spread the papers out fairly.
  # shuffle pubs so we don't always assign in the same order
  # then assign stochastically on the basis of how much
  # the assignment would improve the person's score.

  # get a table of assignments and make them
  assignments<-lapply(shuffle.ids(shared.pubs),function(p.id){
    cat(p.id,"\n")
    these<-shared.pubs[shared.pubs[,"PaperID"]==p.id,]
    a.ids<-these[,"AuthorID"]
    if(length(a.ids)==0){return(NA) }
    these.authors<-authors[a.ids]
    # there could be authors who aren't on the author list. Chuck them out.
    these.authors<-these.authors[!is.na(names(these.authors))]
    # and possibly no authors on the author list
    if(length(these.authors)==0){
      warning(paste("No authors in author list for paper id", p.id))
      return(NA)
    }

    p.grade<-unique(these[,"PredictedGrade"])
    if(length(p.grade)>1){
        #warning(paste("conflicting grades for paper id", p.id, "Using best grade"))
        p.grade<-max(p.grade)
    }

    #create a table of possible replacement positions:
    replacements<-list()
    for (a in names(these.authors)){
      this.a<-authors[[a]]
      these.pubs<-this.a$pubs[[1]] # pubs is a list of hefce.pubs
      these.grades<-these.pubs[,"PredictedGrade"]
 
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
    if(nrow(replacements)==0) return(NA)
    # convert weights to probabilities and select the replacement on the basis of those
    replacements$weight <- replacements$weight / sum(replacements$weight)
    row<-replacements[sample(1:nrow(replacements), 1, prob=replacements$weight),]    
    rownames(row)<-p.id
    return(row)
  })

  # chuck out any NAs and make into a table
  assignments<-assignments[!is.na(assignments)]
  assignments<-do.call(rbind,assignments)


  for(i in 1:nrow(assignments)){
    authors<-assign.paper(authors=authors,
                          pubs=shared.pubs, 
                          a.id=assignments[i,"auth.id"],
                          pub.id=rownames(assignments)[i],
                          pos=assignments[i,"pos"]
                         )
  }
  return(authors)     
}

# initial assignment of publications to authors
init.pubs <- function(authors, pubs) UseMethod("init.pubs", authors)
init.pubs.hefce.authors <- function(authors,pubs){
 if(!inherits(authors,'hefce.authors')){stop("authors should be a hefce.authors object")}
 if(!inherits(pubs, 'hefce.pubs')){stop("pubs should be a hefce.pubs object")}

 # only use the pubs with authors in our author list, otherwise we'll confuse the hell
 # out of everything
 sub.pubs<-pubs[pubs$pubs[,"AuthorID"] %in% authors$id,]
 if(nrow(sub.pubs)==0){stop("No matches between publications and authors")}

 shared.pubs<-shared(sub.pubs)
 unshared.pubs<-unshared(sub.pubs)

 # assign unshared (returns a list of single authors)
 authors.assigned <- assign.unshared(authors, unshared.pubs)
 authors.assigned <- assign.shared(authors.assigned, shared.pubs)
  
 return(authors.assigned)
}

#returns the number of publications for each author in pubs
n.pubs <- function(authors,pubs) UseMethod("n.pubs", authors)
n.pubs.hefce.authors<-function(authors,pubs){
  return(sapply(authors, function(x){
     sum(pubs[,"AuthorID"]==x$id)
  }))
}

opt.pubs<-function(authors, max.iter=1000){
  if(any(is.null(authors$pubs))){stop("Some publication elements are NULL. Have you run init.pubs(authors,pubs)?")}
}

# returns a vector of scores for each person.
score<-function(authors) UseMethod("score",authors)
score.hefce.authors<-function(authors){

  scores<-sapply(authors, function(a){ 
     grades<-a$pubs[[a$id]][,"PredictedGrade"]
     if(is.null(grades)){return(NA)} # no papers at all
     if(any(is.na(grades))){return(NA)} # not enough papers to score
     grades<-summary(grades)
     this.score<-sum(c(0,0,1,3) * grades)/a$required.outputs
    return(this.score)
  })
  authors$scores<-scores
  return(scores)
}

# Note that this returns all slots, so you'll probably get a lot of NA rows
all.pubs<-function(authors) UseMethod('all.pubs',authors)
all.pubs.hefce.authors<-function(authors){
  return(do.call(c,authors$pubs))
}

# this returns a sorted list of authors and a list of percentages 
make.submission<-function(authors) UseMethod("make.submission", authors)
make.submission.hefce.authors<-function(authors){

  # throw out any with NAs as they can't be submitted (but keep a note)
  used<-!is.na(score(authors))
#  authors<-authors[!is.na(score(authors))]
  if(sum(!used)>0){
    unused.authors<-authors[!used]
  }
  else{
    unused.authors<-NULL
  }
  authors<-authors[used]

  # sort the authors by score
  ord<-order(score(authors), decreasing=TRUE)
  these.authors<-authors[ord]

  # count the number of 1,2,3 and 4* papers for each author
  counts<-lapply(these.authors,function(x){
    these.counts<-summary(x$pubs[[1]][,"PredictedGrade"])
  })
  names(counts)<-these.authors$id
  counts<-do.call(rbind, counts)
  counts<-data.frame(counts) # otherwise counts[1:1,] has dim
  
  # calculate the percentages of 1,2,3 and 4* papers as you go down
  # the list (so the % for an author assumes submission of that author 
  # all better authors)
  percs<-lapply(1:nrow(counts), function(j){
    total.counts<-apply(counts[1:j,],2,sum)
    percs<-total.counts/sum(total.counts)
  })
  percs<-do.call(rbind,percs)

  # generate running total of cash these percentages would generate
  money<-1:nrow(percs) * ((percs[,"Four"]*23) + (percs[,"Three"]*7))
  percs<-cbind(percs,money)
  rownames(percs)<-names(these.authors)
  return(list(authors=these.authors,percentages=percs, unused.authors=unused.authors))

}


improve.author <- function(authors,id, pubs) UseMethod("improve.author", authors)
improve.author.hefce.authors<-function(authors, id, pubs){
  
  a<-authors[id]
  cat("improving for",a$name, "\n")

  # identify unused papers
  used.pubs<-all.pubs(authors)
  used.pubs<-used.pubs[!is.na(used.pubs[,"PaperID"]),]
  unused.pubs<-pubs[!pubs[,"PaperID"] %in% used.pubs[,"PaperID"],]

  # assign this person the required.outputs best scoring papers from all poss:
  poss.pubs<-c(a$pubs[[1]],unused.pubs[unused.pubs[,"AuthorID"]==id,])
  ord<-order(poss.pubs[,"PredictedGrade"], decreasing=T)
  poss.pubs<-poss.pubs[ord,]
 
  authors$pubs[[id]]<-poss.pubs[1:a$required.outputs,]

  # If the author still doesn't have enough papers to make a submission, throw all their papers back
  # to give other people a chance to pick them up
  if( any(is.na(authors$pubs[[id]]$pubs[,"PredictedGrade"]))){
    na<-authors$pubs[[id]]$pubs
    na$AuthorID=as.character(NA)
    na$AuthorName=as.character(NA)
    na$PaperID=as.character(NA)
    na$PaperName=as.character(NA)
    na$PredictedGrade=ordered(NA, levels=c("One","Two","Three","Four"))
    authors$pubs[[id]]$pubs<-na
  }


  return(authors)
}

optimise<-function(authors, pubs, max.iter, max.no.improv) UseMethod("optimise", authors)
optimise.hefce.authors<-function(authors, pubs, max.iter=100, max.no.improv=1){

  submission<-make.submission(authors)
  max.money<-max(submission$percentages[,"money"])
  iter.count<-1
  no.improv<-0

  repeat{
     #cat("\n\nIteration ",iter.count, "\n")
     if(iter.count>max.iter){break}

     # run a round of improvements
     ids<-sample(authors$id, length(authors$id), replace=FALSE)
     for(i in ids){
       authors<-improve.author(authors,i,pubs)
     }
     # and stop when you aren't improving any more
     submission<-make.submission(authors)
     new.max.money<-max(submission$percentages[,"money"])
     if(new.max.money<=max.money){
       no.improv<-no.improv+1
       if(no.improv==max.no.improv){
         no.improv <- 0
         break
       }
     }
     cat("\n\n",new.max.money,"\n\n")
     max.money<-new.max.money
     iter.count <- iter.count+1
  }

  return(authors)

}





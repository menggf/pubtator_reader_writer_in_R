library(data.table)

### read pubtator file and return a list

read_pubtator<-function(input.file){
  input=as.vector(as.matrix(fread(input.file, sep="\n", header=FALSE, encoding="UTF-8")))
  output.list=list()
  cc=1
  while(1){
    if(cc>=length(input))
      break()
    id=strsplit(input[cc],"\\|")[[1]][1]
    tmp=list()
    tmp[["item"]]=NULL
    tmp[["title"]]=input[cc]
    cc=cc+1
    tmp[["abstract"]]=input[cc]
    cc=cc+1
    
    zz=0;
    while(cc + zz <= length(input) & input[cc + zz]!=""){
      zz=zz+1
    }
    if(zz!=0){
      sub.cc=input[cc:(cc+zz-1)]
      res = as.data.frame(t(sapply(sub.cc, function(x){
        y=strsplit(x,"\t")[[1]]
        if(length(y)==5)
          return(c(y,NA))
        return(y)
      })))
      row.names(res)<-paste0("R",1:nrow(res))
      tmp[["item"]]=res
      output.list[[id]]=tmp
    }else{
      output.list[[id]]=tmp
    }
    cc=cc+zz+1
  }
  return(output.list)
}
### add species information into pubtator

add_species_pubtator<-function(input.list, species_info="Focus:9606"){
  lapply(input.list, function(input){
    if(is.null(input[["item"]]))
      return(input)
    #print(names(input))
    item= input[["item"]]
    mark=as.vector(item$V6)
    mark[is.na(mark)]=species_info
    item$V6=mark
    input[["item"]]=item
    return(input)
  })
}

add_species_pubtator<-function(input.list){
  lapply(input.list, function(input){
    if(is.null(input[["item"]]))
      return(input)
    #print(names(input))
    item= input[["item"]]
    mark=as.vector(item$V6)
    mark[is.na(mark)]="Focus:9606"
    item$V6=mark
    input[["item"]]=item
    return(input)
  })
}

### remove pubtator item without any record

remove_empty_pubtator<-function(input.list, remove.empty.col6=TRUE){
  o=lapply(input.list, function(input){
    if(is.null(input[["item"]]))
      return(NULL)
    if(!remove.empty.col6)
      return(input)
    item=input[["item"]]
    item=subset(item, !is.na(V6))
    if(nrow(item)==0)
      return(NULL)
    input[["item"]]=item
    return(input)
  })
  o=o[!sapply(o, is.null)]
  o
}

remove_nogene_pubtator<-function(input.list){
  o=lapply(input.list, function(input){
    if(is.null(input[["item"]]))
      return(NULL)
   
    item=input[["item"]]
    n.gene=nrow(subset(item, V5=="Gene"))
    if( n.gene==0)
		return(NULL)   
    input[["item"]]=item
    return(input)
  })
  o=o[!sapply(o, is.null)]
  o
}

### write pubtator to file

write_pubtator<-function(input.list, outfile, overwrite=FALSE, remove.empty=FALSE){
  if(file.exists(outfile) & overwrite){
    file.remove(outfile)
  }
  o=lapply(input.list, function(input){
    #print(names(input))
    if(is.null(input[["item"]])){
      if(!remove.empty){
         write.table(input[["title"]],outfile, row.names=F,col.names=F,quote=F, append=TRUE)
         write.table(input[["abstract"]],outfile, row.names=F,col.names=F,quote=F, append=TRUE)
         write.table("",outfile, row.names=F,col.names=F,quote=F, append=TRUE)
      }
    }else{
       write.table(input[["title"]],outfile, row.names=F,col.names=F,quote=F, append=TRUE)
       write.table(input[["abstract"]],outfile, row.names=F,col.names=F,quote=F, append=TRUE)
       write.table(input[["item"]],outfile, sep="\t",row.names=F,col.names=F,quote=F, append=TRUE)
       write.table("",outfile,  row.names=F,col.names=F,quote=F, append=TRUE)
    }
    return(1)
  })
}

merge_pubtator<-function(ip1, ip2){
  ids=unique(c(names(ip1), names(ip2)))
  o=lapply(ids, function(id){
    if(is.null(ip1[[id]]))
      return(ip2[[id]])
    if(is.null(ip2[[id]]))
      return(ip1[[id]])
    if(is.null(ip2[[id]][["item"]]))
      return(ip1[[id]])
    if(is.null(ip1[[id]][["item"]]))
      return(ip2[[id]])  
    new.ip=list()
    new.ip[["title"]]=ip1[[id]][["title"]]
    new.ip[["abstract"]]=ip1[[id]][["abstract"]]
    new.ip[["item"]]=rbind(ip1[[id]][["item"]], ip2[[id]][["item"]])
    
    return(new.ip)
  })
  names(o)<-ids
  return(o)
}



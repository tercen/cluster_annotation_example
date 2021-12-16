library(tercen)
library(dplyr)
library(tidyr)

### FUNCTION
find.match<- function(cluster_pop,tbl_pop){
  population.list<-c()
  
  for (i in c(1:length(rownames(tbl_pop)))){
    pop.name<-rownames(tbl_pop)[i]
    marker.list<-c()
    
    for (y in c(1:length((tbl_pop[i,])))){
      if (tbl_pop[i,y] == 1){
        marker<- paste(colnames(tbl_pop)[y],"+",sep="")
      } else if(tbl_pop[i,y] == -1){
        marker<- paste(colnames(tbl_pop)[y],"-",sep="")
      } else{
        marker<-""
      }
      marker.list<-append(marker.list,marker)
    }
    
    unmatch<-FALSE
    for(element in marker.list){
      match<-grepl(element, cluster_pop["population"],fixed = TRUE)
      if(!match){
        #print("unmatch")
        unmatch<-TRUE
        break
      }
    }
    
    if (!unmatch){
      population.list<-append(population.list,pop.name)
    }
  }
  return(population.list)
}
###


ctx <- tercenCtx()

###########
script_name <- ifelse(
  is.null(ctx$op.value("file_name")), "script.R",
  ctx$op.value("file_name")
)

file_list <- ctx$client$projectDocumentService$findFileByLastModifiedDate(limit = 1000, descending = TRUE)
names_list <- unlist(lapply(file_list, function(x) x$name))
script_id <- which(names_list == script_name)
if(!length(script_id)) stop("File not found, check file name.")

bytes <- ctx$client$fileService$download(file_list[[script_id[1]]]$id)
script <- rawToChar(bytes)
eval(parse(text = script))
############

mem_matrix<-ctx %>% 
  select(.ci, .ri, .y)

tbl_pop<-read.csv2(file="table_pop.csv", header = TRUE, sep = ",", row.names = 1)

channel_list<-ctx$rselect()
data_mem<-pivot_wider(mem_matrix,names_from = .ri, values_from = .y)
colnames(data_mem)[-1]<-channel_list$channel

out.mat<-matrix(, nrow = 0, ncol = 2)
for (cluster.nb in c(1:length(data_mem[[".ci"]]))){
  label<-data_mem[[".ci"]][cluster.nb]
  population<-""
  for (cname in colnames(data_mem)[-1]){
    #threshold <- median(data_mem[[cname]])
    threshold <-0
    if (data_mem[cluster.nb,cname]<threshold){
      population<-paste(population,cname,"-",sep="")
    }
    else{
      population<-paste(population,cname,"+",sep="")
    }
  }
  res<-cbind(label,population)
  out.mat<-rbind(out.mat,res)
}


###


output<-matrix(, nrow = 0, ncol = 2)
for (row.nb in c(1:length(out.mat[,1]))){
  result<-find.match(out.mat[row.nb,],tbl_pop)
  for (res.pop in result){
    new.row<-cbind(as.numeric(row.nb),res.pop)
    output<-rbind(output,new.row)
  }
  if(is.null(result)){
    new.row<-cbind(as.numeric(row.nb),"Unknown")
    output<-rbind(output,new.row)
  }
}
colnames(output)<-c(".ci","population")

output%>%
  as_tibble()%>% 
  mutate(.ci = as.integer(as.integer(.ci)-1)) %>%
  ctx$addNamespace() %>%
  ctx$save()


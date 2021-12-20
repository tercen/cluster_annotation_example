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

### Extract the population table with the documentId
doc.id.tmp<-as_tibble(ctx$select())
doc.id<-doc.id.tmp[[grep("documentId" , colnames(doc.id.tmp))]][1]

table.pop<-ctx$client$tableSchemaService$select(doc.id)

tbl_pop<-as_tibble(table.pop)
Population = tbl_pop$Population
tbl_pop %<>% select(-Population) %>% as.matrix
rownames(tbl_pop) = Population
###

mem_matrix<-ctx %>% 
  select(.ci, .ri, .y)

channel_list<-ctx$rselect()
data_mem<-pivot_wider(mem_matrix,names_from = .ri, values_from = .y)
colnames(data_mem)[-1]<-channel_list[[1]]
colnames(data_mem)[-1]<-c("HLADR","pERK1","CD3","Perf","CD38","IFNg","CD4","CD8")

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


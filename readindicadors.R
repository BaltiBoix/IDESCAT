require(RSQLite)
require(jsonlite)
require(dplyr)
require(tidyr)

url<-'http://api.idescat.cat/indicadors/v1/nodes.json?lang=es'
raw.data <- readLines(url, warn = "F", encoding = "UTF-8")
rd <- fromJSON(raw.data, simplifyVector = FALSE, flatten = TRUE)
remove(raw.data)

rdl<-as.data.frame(rd, stringsAsFactors = FALSE)
rdl<-gather(rdl, 'key', 'value', convert = TRUE)
rdlist<-strsplit(rdl[,1],'[.]')

df<-data.frame(id1=character(), id2=character(), id3=character(), id4=character(), 
               id5=character(), id6=character(), id7=character(), id8=character(),
               id9=character(), value=character(), stringsAsFactors = FALSE)
for(i in 1:length(rdlist)){
      df[i,10]<-rdl[i,2]
      for(j in 1:9){
          df[i,j]<-rdlist[[i]][j]
          if(is.na(df[i,j])){
                if(!is.na(as.integer(df[i,j-1]))){
                      df[i,9]<-as.integer(df[i,j-1])
                      df[i,j-1]<-NA
                }else{
                      df[i,9]<-0
                  }
                break
          }
      }
}


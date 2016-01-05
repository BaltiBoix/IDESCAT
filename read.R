require(RSQLite)
require(jsonlite)
require(dplyr)
require(tidyr)

url<-'http://api.idescat.cat/emex/v1/dades.json?id=250019&lang=es'
raw.data <- readLines(url, warn = "F", encoding = "UTF-8")
rd <- fromJSON(raw.data, simplifyVector = FALSE, flatten = TRUE)
remove(raw.data)

rdl<-as.data.frame(rd)
rdl<-gather(rdl, 'key')
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

for(i in unique(as.list(select(filter(df, id3=='col'), id9))[[1]])){
      idcol<-as.character(filter(df, id3=='col', id4=='id', id9==i)['value'])
      df[rownames(subset(df, id3=='col' & id9==i)), 'id3']<-idcol
}

for(i in 1:nrow(df)){
      if(df[i,'id2'] == 'gg'){
            if(df[i,'id3'] == 'g' & df[i,'id4'] == 'id') {
                  idcol<-df[i, 'value']
            }
            if(df[i,'id3'] == 'g') {
                  df[i, 'id3']<- idcol
            }
      }
}

for(i in 1:nrow(df)){
      if(is.na(df[i, "id4"])) next
      if(df[i,'id4'] == 'tt'){
            if(df[i,'id5'] == 't' & df[i,'id6'] == 'id') {
                  idcol<-df[i, 'value']
            }
            if(df[i,'id5'] == 't') {
                  df[i, 'id5']<- idcol
            }
      }
}

for(i in 1:nrow(df)){
      if(is.na(df[i, "id6"])) next
      if(df[i,'id6'] == 'ff'){
            if(df[i,'id7'] == 'f' & df[i,'id8'] == 'id') {
                  idcol<-df[i, 'value']
            }
            if(df[i,'id7'] == 'f') {
                  df[i, 'id7']<- idcol
            }
      }
}

glist<-as.list(unique(select(filter(df, grepl('^g[0-9]', id3)), id3)))
gdf<-select(filter(df, grepl('^g[0-9]', id3), id4 == 'c'), id3, value)

tdf<-select(filter(df, grepl('^t[0-9]', id5), id6 == 'c'), id5, value)

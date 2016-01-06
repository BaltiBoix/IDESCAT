require(RSQLite)
require(jsonlite)
require(dplyr)
require(tidyr)

url<-'http://api.idescat.cat/emex/v1/dades.json?i=f171&lang=es'
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


coldf<- select(filter(df, id2=='cols', id3=='col'), id4, value)

coldf<-data.frame(municipio_id=as.integer(coldf[seq(2, nrow(coldf), 3), 'value']), 
                  scheme=coldf[seq(3, nrow(coldf), 3), 'value'], 
                  content=coldf[seq(1, nrow(coldf), 3), 'value'], 
                  stringsAsFactors = FALSE)

factor_id<-as.integer(substring(as.character(select(filter(df, id2=='indicadors', id3=='i', id4=='id'), value)),2))
valor<-as.integer(unlist(strsplit(as.character(select(filter(df, id2=='indicadors', id3=='i', id4=='v'), value)), '[,]')))

valdf<-data.frame(factor_id=rep(factor_id, nrow(coldf)), valor=valor, stringsAsFactors = FALSE)
valdf<-cbind(coldf, valdf)
valdf<-filter(valdf, !is.na(municipio_id))

mundf<-select(filter(valdf, scheme=='mun'), municipio_id, factor_id, valor)

db <- dbConnect(SQLite(), 'IDESCAT.sqlite')

dbSendQuery(conn = db,
            "CREATE TABLE IF NOT EXISTS `DatosMunicipios` (
            `MUNICIPIO_ID` INTEGER NOT NULL,
            `FACTOR_ID` INTEGER NOT NULL,
            `VALOR` NUMERIC,
            PRIMARY KEY(MUNICIPIO_ID, FACTOR_ID)
);")

dbWriteTable(db, 'DatosMunicipios', mundf, append = TRUE)

dbDisconnect(db)


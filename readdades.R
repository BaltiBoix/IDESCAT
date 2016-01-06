require(RSQLite)
require(jsonlite)
require(dplyr)
require(tidyr)

url<-'http://api.idescat.cat/emex/v1/dades.json?id=250019&lang=es'
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
gdf<-select(filter(df, grepl('^g[0-9]', id3), id4 == 'c'), id=id3, c=value)

tlist<-as.list(unique(select(filter(df, grepl('^t[0-9]', id5)), id5)))
tdf<-select(filter(df, grepl('^t[0-9]', id5), id6 == 'c'), id=id5, grupo_id=id3, c=value)
tdf1<-select(filter(df, grepl('^t[0-9]', id5), id6 == 'calt'), id=id5, grupo_id=id3, calt=value)
tdf<-left_join(tdf, tdf1)
remove(tdf1)

flist<-as.list(unique(select(filter(df, grepl('^f[0-9]', id7)), id7)))
fdf<-select(filter(df, grepl('^f[0-9]', id7), id8 == 'c'), id=id7, grupo_id=id3, tabla_id=id5, c=value)
fdf1<-select(filter(df, grepl('^f[0-9]', id7), id8 == 'calt'), id=id7, grupo_id=id3, tabla_id=id5, calt=value)
fdf<-left_join(fdf,fdf1)
remove(fdf1)

gdf<-mutate(gdf, id=as.integer(substring(id,2)))
tdf<-mutate(tdf, id=as.integer(substring(id,2)), grupo_id=as.integer(substring(grupo_id,2)))
fdf<-mutate(fdf, id=as.integer(substring(id,2)), grupo_id=as.integer(substring(grupo_id,2)), 
            tabla_id=as.integer(substring(grupo_id,2)))

db <- dbConnect(SQLite(), 'IDESCAT.sqlite')

dbSendQuery(conn = db, "DROP TABLE IF EXISTS 'Grupos';")

dbSendQuery(conn = db,
            "CREATE TABLE `Grupos` (
            `ID` INTEGER NOT NULL UNIQUE,
            `C`	TEXT NOT NULL UNIQUE,
            PRIMARY KEY(ID)
);")

dbWriteTable(db, 'Grupos', gdf, append = TRUE)

dbSendQuery(conn = db, "DROP TABLE IF EXISTS 'Tablas';")

dbSendQuery(conn = db,
            "CREATE TABLE `Tablas` (
            `ID` INTEGER NOT NULL UNIQUE,
            `GRUPO_ID` INTEGER NOT NULL,
            `C` TEXT NOT NULL,
            `CALT` TEXT,
            PRIMARY KEY(ID)
);")

dbWriteTable(db, 'Tablas', tdf, append = TRUE)

dbSendQuery(conn = db, "DROP TABLE IF EXISTS 'Factores';")

dbSendQuery(conn = db,
            "CREATE TABLE `Factores` (
            `ID` INTEGER NOT NULL UNIQUE,
            `GRUPO_ID` INTEGER NOT NULL,
            `TABLA_ID` INTEGER NOT NULL,
            `C` TEXT NOT NULL,
            `CALT` TEXT,
            PRIMARY KEY(ID)
);")

dbWriteTable(db, 'Factores', fdf, append = TRUE)

dbdf<-dbGetQuery(conn = db,
      "select Grupos.C as Grupo, Tablas.C as Tabla, Factores.C as Factor 
      from Factores join Tablas join Grupos
      on Factores.GRUPO_ID=Grupos.ID and Factores.TABLA_ID=Tablas.ID
      order by Grupos.C, Tablas.C, Factores.C;"
      )

dbdf$Grupo <- iconv(dbdf$Grupo, "UTF-8", "UTF-8")
dbdf$Tabla <- iconv(dbdf$Tabla, "UTF-8", "UTF-8")
dbdf$Factor <- iconv(dbdf$Factor, "UTF-8", "UTF-8")

dbDisconnect(db)


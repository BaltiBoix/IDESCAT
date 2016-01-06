require(RSQLite)
require(jsonlite)

url<-'http://api.idescat.cat/emex/v1/nodes.json?lang=es'
raw.data <- readLines(url, warn = "F", encoding = "UTF-8")
rd <- fromJSON(raw.data)
remove(raw.data)

mundf<-data.frame(id=integer(), municipio=character(), comarca_id=integer(), ccaa_id=integer(), stringsAsFactors=FALSE)

n=1
for(i in 1:length(rd$fitxes$v$v$content)){
      for(j in 1:nrow(rd$fitxes$v$v$v[[i]])){
            mundf[n, 1] = as.integer(rd$fitxes$v$v$v[[i]][j,2])
            mundf[n, 2] = rd$fitxes$v$v$v[[i]][j,1]
            mundf[n, 3] = i
            mundf[n, 4] = as.integer(rd$fitxes$v$id)
            n<-n + 1}
}

comdf<-data.frame(id=integer(), comarca=character(), stringsAsFactors=FALSE)

for(i in 1:length(rd$fitxes$v$v$content)){
      comdf[i, 1] = as.integer(rd$fitxes$v$v$id[i])
      comdf[i, 2] = rd$fitxes$v$v$content[i]
}


ccaadf<-data.frame(id=as.integer(rd$fitxes$v$id), ccaa=rd$fitxes$v$content, stringsAsFactors=FALSE)

db <- dbConnect(SQLite(), 'IDESCAT.sqlite')

dbSendQuery(conn = db, "DROP TABLE IF EXISTS 'CCAA';")

dbSendQuery(conn = db,
            "CREATE TABLE `CCAA` (
            `ID` INTEGER NOT NULL UNIQUE,
            `CCAA` TEXT NOT NULL UNIQUE,
            PRIMARY KEY(ID)
);")

dbWriteTable(db, 'CCAA', ccaadf, append = TRUE)

dbSendQuery(conn = db, "DROP TABLE IF EXISTS 'Municipios';")

dbSendQuery(conn = db,
            "CREATE TABLE `Municipios` (
          	`ID` INTEGER NOT NULL UNIQUE,
            `Municipio`	TEXT NOT NULL UNIQUE,
            `Comarca_ID` INTEGER NOT NULL,
            `CCAA_ID` INTEGER NOT NULL,
            PRIMARY KEY(ID)
            );")

dbWriteTable(db, 'Municipios', mundf, append = TRUE)

dbSendQuery(conn = db, "DROP TABLE IF EXISTS 'Comarcas';")

dbSendQuery(conn = db,
            "CREATE TABLE `Comarcas` (
            `ID` INTEGER NOT NULL UNIQUE,
            `Comarca` TEXT NOT NULL UNIQUE,
            PRIMARY KEY(ID)
);")

dbWriteTable(db, 'Comarcas', comdf, append = TRUE)

df<-dbGetQuery(conn = db,
      "select Municipios.Municipio, Comarcas.Comarca 
      from Municipios join Comarcas
      on Municipios.Comarca_ID = Comarcas.ID
      order by  Comarcas.Comarca;")

df$Municipio <- iconv(df$Municipio, "UTF-8", "UTF-8")
df$Comarca <- iconv(df$Comarca, "UTF-8", "UTF-8")

dbDisconnect(db)

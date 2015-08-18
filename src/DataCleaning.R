library('RJDBC')
library('ggplot2')
library('plyr')
library('scales')
library('forecast')
dbPassword = "PcL233yW"
drv = JDBC("org.firebirdsql.jdbc.FBDriver",
           "./jdbc-driver/jaybird-full-2.2.7.jar",
           identifier.quote="`")
# Firebird is very sensitive to the URL format. This should be used: jdbc:firebirdsql://host:port//path/to/teh/shit.fdb
connection = dbConnect(drv, 
                       paste("jdbc:firebirdsql://127.0.0.1:3050//databases/", "dbs_bosz_2015.fdb", sep=""),
                       "SYSDBA", dbPassword)
connection_2014 = dbConnect(drv, 
                       paste("jdbc:firebirdsql://127.0.0.1:3050//databases/", "dbs_bosz_2014.fdb", sep=""),
                       "SYSDBA", dbPassword)
connection_2013 = dbConnect(drv, 
                       paste("jdbc:firebirdsql://127.0.0.1:3050//databases/", "dbs_bosz_2013.fdb", sep=""),
                       "SYSDBA", dbPassword)
connection_2010_2012 = dbConnect(drv, 
                       paste("jdbc:firebirdsql://127.0.0.1:3050//databases/", "dbs_bosz_2010-2012.fdb", sep=""),
                       "SYSDBA", dbPassword)

temp = data.frame(ID_VEVO=numeric(), NEV=character(), IRSZAM=numeric(), VAROS=character(), CIM=character(), ADOSZAM=character())
getData = function (year, connection) {
    command = paste("select v.id_vevo, lower(v.nev), v.irszam, lower(v.varos), lower(v.cim), v.adoszam",
                    " from vevo v")
    return(dbGetQuery(connection, command))
}
temp = rbind(temp, getData(2010, connection_2010_2012))
temp = rbind(temp, getData(2011, connection_2010_2012))
temp = rbind(temp, getData(2012, connection_2010_2012))
temp = rbind(temp, getData(2013, connection_2013))
temp = rbind(temp, getData(2014, connection_2014))
temp = rbind(temp, getData(2015, connection))
colnames(temp) = c("customer_id", "customer_name", "zip", "city", "address", "taxnumber")
temp$customer_name = as.factor(temp$customer_name)

agg = aggregate(temp$customer_id, list(temp$taxnumber), length)
agg[1]

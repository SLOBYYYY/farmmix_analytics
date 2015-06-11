#install.packages("RJDBC", dependencies=T)
library("RJDBC")
dbPassword = "zOMOEd5f"
dbName = "dbs_bosz_2015.fdb"
drv = JDBC("org.firebirdsql.jdbc.FBDriver",
           "./jdbc-driver/jaybird-full-2.2.7.jar",
           identifier.quote="`")
# Firebird is very sensitive to the URL format. This should be used: jdbc:firebirdsql://host:port//path/to/teh/shit.fdb
connection = dbConnect(drv, 
                       paste("jdbc:firebirdsql://127.0.0.1:3050//databases/", dbName, sep=""),
                       "SYSDBA", dbPassword)

library(plyr)

drawPieChart = function (data, text) {
    # data's first 2 columns should be labels and values
    labels = data[,1]
    slices = data[,2]
    percentages = round(slices/sum(slices)*100)
    labels = paste(labels, percentages)
    labels = paste(labels, "%", sep="")
    labels = paste(labels, " (", round(slices, digits=0), ")", sep="")
    pie(slices, labels = labels, col=rainbow(length(labels)),
        main=text)
}

drawBarChart = function (data, xText, yText) {
    data.length = dim(data)[1]
    colors = rainbow(data.length)
    data = arrange(data, -data[,2])
    data.barplot = barplot(data[,2], 
                             cex.names=0.6, 
                             xlab=xText,
                             ylab=yText, 
                             ylim=c(0,max(data[,2]) * 1.2),
                             col=colors)
    text(data.barplot, data[,2], labels=data[,2], pos=3, cex=0.7)
    legend("topright", legend = data[,1], fill=colors, cex=0.7)
}

drawTermekCsoportPieForAllTermek = function (dbConnection) {
    # Side note: If I try to merge 2 tables, use "merge" instead of mergin it by hand..
    termekcsoport.freq = dbGetQuery(connection, "select csoport.nev, count(termek.id_termek) from termek join csoport on csoport.id_csoport = termek.id_csoport group by csoport.nev")
    drawPieChart(termekcsoport.freq, "Termékcsoportok megoszlása a termékek között")
}

drawTermekCsoportPieForAllTermek(connection)

drawTermekCsoportForSoldTermekek = function (dbConnection) {
    termekcsoport.freq = dbGetQuery(dbConnection, 
               paste("select csoport.nev, count(szamlatetel.id_termek) ",
               "from szamlatetel ",
               "join termek on szamlatetel.id_termek = termek.id_termek ",
               "join csoport on csoport.id_csoport = termek.id_csoport",
               "group by csoport.nev"))
    drawPieChart(termekcsoport.freq, "Termékcsoportok megoszlása az eladott termékek között")
}

drawTermekCsoportForSoldTermekek(connection)
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


drawTopXProducts = function (dbConnection, topX) {
    if (is.numeric(topX)) {
        topXSoldTetel = dbGetQuery(dbConnection,
                                     paste("select first", topX, "termek.nev, sum(szamlatetel.eladar) as \"EladarSum\"",
                                           "from szamlatetel join termek on termek.id_termek = szamlatetel.id_termek",
                                           "group by termek.nev",
                                           "order by \"EladarSum\" desc"))
        #topXSoldTetel = rename(topXSoldTetel, c('ID_TERMEK'='Id', 'ELADAR'='Eladar.sum'))
        colors = terrain.colors(topX)
        soldTetel.barplot = barplot(topXSoldTetel[,2], 
                                 #names.arg=topXSoldTetel[,'NEV'], 
                                 cex.names=0.6, 
                                 xlab="Top Termékek",
                                 ylab="Eladott mennyiség (Ft)", 
                                 ylim=c(0,max(topXSoldTetel[,2]) * 1.2),
                                 col=colors)
        text(soldTetel.barplot, topXSoldTetel[,2], labels=topXSoldTetel[,2], pos=3, cex=0.7)
        legend("topright", legend = topXSoldTetel[,'NEV'], fill=colors, cex=0.7)
    } else {
        stop("The topX variable should be a number")
    }
}

drawTopXProducts(connection, 10)

drawTopXFarmmixProducts = function (dbConnection, topX) {
    if (is.numeric(topX)) {
        topXSoldTetel = dbGetQuery(dbConnection,
                                     paste("select first", topX, "termek.nev, sum(szamlatetel.eladar) as \"EladarSum\"",
                                           "from szamlatetel join ", 
                                           "termek on termek.id_termek = szamlatetel.id_termek join",
                                           "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo",
                                           "where forgalmazo.nev like '%FARMMIX%'",
                                           "group by termek.nev",
                                           "order by \"EladarSum\" desc"))
        #topXSoldTetel = rename(topXSoldTetel, c('ID_TERMEK'='Id', 'ELADAR'='Eladar.sum'))
        colors = terrain.colors(topX)
        soldTetel.barplot = barplot(topXSoldTetel[,2], 
                                 #names.arg=topXSoldTetel[,'NEV'], 
                                 cex.names=0.6, 
                                 xlab="Top Termékek",
                                 ylab="Eladott mennyiség (Ft)", 
                                 ylim=c(0,max(topXSoldTetel[,2]) * 1.2),
                                 col=colors)
        text(soldTetel.barplot, topXSoldTetel[,2], labels=topXSoldTetel[,2], pos=3, cex=0.7)
        legend("topright", legend = topXSoldTetel[,'NEV'], fill=colors, cex=0.7)
    } else {
        stop("The topX variable should be a number")
    }
}

drawTopXFarmmixProducts(connection, 10)

topXIsWhatPercentOfAllProducts = function (dbConnection, topX) {
    if (is.numeric(topX)) {
        topXSoldTetel = dbGetQuery(dbConnection,
                                     paste("select first", topX, "termek.nev, sum(szamlatetel.eladar) as \"EladarSum\"",
                                           "from szamlatetel join ", 
                                           "termek on termek.id_termek = szamlatetel.id_termek join",
                                           "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo",
                                           "group by termek.nev",
                                           "order by \"EladarSum\" desc"))
        sumSoldTetel = dbGetQuery(dbConnection, "select sum(szamlatetel.eladar) from szamlatetel")
        products = ""
        for (i in 1:topX) {
            products = paste(products, topXSoldTetel[i,1], sep=" | ")
        }
        print(paste("Termékek: ", products))
        print(paste("A top", topX, "termék a bevétel", round(sum(topXSoldTetel[,2], na.rm=T) / sumSoldTetel[1,1] * 100, 2), '%-át hozzák'))
    } else {
        stop("The topX variable should be a number")
    }
}

topXIsWhatPercentOfAllProducts(connection, 5)

topXFarmmixIsWhatPercentOfAllProducts = function (dbConnection, topX) {
    if (is.numeric(topX)) {
        topXSoldTetel = dbGetQuery(dbConnection,
                                     paste("select first", topX, "termek.nev, sum(szamlatetel.eladar) as \"EladarSum\"",
                                           "from szamlatetel join ", 
                                           "termek on termek.id_termek = szamlatetel.id_termek join",
                                           "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo",
                                           "where forgalmazo.nev like '%FARMMIX%'",
                                           "group by termek.nev",
                                           "order by \"EladarSum\" desc"))
        sumSoldTetel = dbGetQuery(dbConnection, "select sum(szamlatetel.eladar) from szamlatetel")
        products = ""
        for (i in 1:topX) {
            products = paste(products, topXSoldTetel[i,1], sep=" | ")
        }
        print(paste("Farmmix termékek: ", products))
        print(paste("A top", topX, "Farmmix termék a bevétel", round(sum(topXSoldTetel[,2], na.rm=T) / sumSoldTetel[1,1] * 100, 2), '%-át hozzák'))
    } else {
        stop("The topX variable should be a number")
    }
}

topXFarmmixIsWhatPercentOfAllProducts(connection, 5)


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
                                 xlab="Top eladott összes termék",
                                 ylab="Eladott mennyiség (Ft)", 
                                 ylim=c(0,max(topXSoldTetel[,2]) * 1.2),
                                 col=colors)
        text(soldTetel.barplot, topXSoldTetel[,2], labels=topXSoldTetel[,2], pos=3, cex=0.7)
        legend("topright", legend = topXSoldTetel[,'NEV'], fill=colors, cex=0.7)
    } else {
        stop("The topX variable should be a number")
    }
}

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
                                 xlab="Top eladott Farmmixes termékek",
                                 ylab="Eladott mennyiség (Ft)", 
                                 ylim=c(0,max(topXSoldTetel[,2]) * 1.2),
                                 col=colors)
        text(soldTetel.barplot, topXSoldTetel[,2], labels=topXSoldTetel[,2], pos=3, cex=0.7)
        legend("topright", legend = topXSoldTetel[,'NEV'], fill=colors, cex=0.7)
    } else {
        stop("The topX variable should be a number")
    }
}

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
        print(paste("A top", topX, "termék a bevétel", round(sum(topXSoldTetel[,2], na.rm=T) / sumSoldTetel[1,1] * 100, 2), '%-át hozzák'))
        print("Termékek: ")
        for (i in 1:topX) {
            print(paste(topXSoldTetel[i,1], " (", round(topXSoldTetel[i,2] / sumSoldTetel[1,1] * 100, 2), "%)", sep=""))
        }
    } else {
        stop("The topX variable should be a number")
    }
}

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
        print(paste("A top", topX, "Farmmix termék a bevétel", round(sum(topXSoldTetel[,2], na.rm=T) / sumSoldTetel[1,1] * 100, 2), '%-át hozzák'))
        print("Farmmix termékek: ")
        for (i in 1:topX) {
            print(paste(topXSoldTetel[i,1], " (", round(topXSoldTetel[i,2] / sumSoldTetel[1,1] * 100, 2), "%)", sep=""))
        }
    } else {
        stop("The topX variable should be a number")
    }
}

worstXProducts = function (dbConnection, topX) {
    if (is.numeric(topX)) {
        worstXProduct = dbGetQuery(dbConnection,
                                   paste("select first", topX, "termek.nev, sum(szamlatetel.eladar) as \"EladarSum\"",
                                         "from szamlatetel join ", 
                                         "termek on termek.id_termek = szamlatetel.id_termek",
                                         "group by termek.nev",
                                         "having sum(szamlatetel.eladar) > 0",
                                         "order by \"EladarSum\""))
        sumSoldTetel = dbGetQuery(dbConnection, "select sum(szamlatetel.eladar) from szamlatetel")
        worstXProductSum = sum(worstXProduct[,2], na.rm=T)
        print(paste("A legrosszabb", topX, "termék a bevétel", round(worstXProductSum / sumSoldTetel[1,1] * 100, 2), "%-át (", worstXProductSum ,"Ft) hozzák"))
        print("A legrosszabb termékek: ")
        for (i in 1:topX) {
            print(paste(worstXProduct[i,1],
                        " (",
                        worstXProduct[i,2],
                        " - ",
                        round(worstXProduct[i,2] / sumSoldTetel[1,1] * 100, 2),
                        "%)",
                        sep=""))
        }
    } else {
        stop("The topX variable should be a number")
    }
}

worstXFarmmixProducts = function (dbConnection, topX) {
    if (is.numeric(topX)) {
        worstXProduct = dbGetQuery(dbConnection,
                                   paste("select first", topX, "termek.nev, sum(szamlatetel.eladar) as \"EladarSum\"",
                                         "from szamlatetel join ", 
                                         "termek on termek.id_termek = szamlatetel.id_termek join",
                                         "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo",
                                         "where forgalmazo.nev like '%FARMMIX%'",
                                         "group by termek.nev",
                                         "having sum(szamlatetel.eladar) > 0",
                                         "order by \"EladarSum\""))
        sumSoldTetel = dbGetQuery(dbConnection, "select sum(szamlatetel.eladar) from szamlatetel")
        worstXProductSum = sum(worstXProduct[,2], na.rm=T)
        print(paste("A legrosszabb", topX, "Farmmix termék a bevétel", round(worstXProductSum / sumSoldTetel[1,1] * 100, 2), "%-át (", worstXProductSum ,"Ft) hozzák"))
        print("A legrosszabb Farmmix termékek: ")
        for (i in 1:topX) {
            print(paste(worstXProduct[i,1],
                        " (",
                        worstXProduct[i,2],
                        " - ",
                        round(worstXProduct[i,2] / sumSoldTetel[1,1] * 100, 2),
                        "%)",
                        sep=""))
        }
    } else {
        stop("The topX variable should be a number")
    }
}

drawTopXProducts(connection, 10)
drawTopXFarmmixProducts(connection, 10)
topXIsWhatPercentOfAllProducts(connection, 5)
topXFarmmixIsWhatPercentOfAllProducts(connection, 5)
worstXProducts(connection, 20)
worstXFarmmixProducts(connection, 20)

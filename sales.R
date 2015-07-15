drawTopXProducts = function (dbConnection, topX) {
    if (is.numeric(topX)) {
        topXSoldTetel = dbGetQuery(dbConnection,
                                     paste("select first", topX, "termek.nev, sum(szamlatetel.eladar * szamlatetel.mennyiseg) as \"EladarSum\"",
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
                                     paste("select first", topX, "termek.nev, sum(szamlatetel.eladar * szamlatetel.mennyiseg) as \"EladarSum\"",
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
                                     paste("select first", topX, "termek.nev, sum(szamlatetel.eladar * szamlatetel.mennyiseg) as \"EladarSum\"",
                                           "from szamlatetel join ", 
                                           "termek on termek.id_termek = szamlatetel.id_termek join",
                                           "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo",
                                           "group by termek.nev",
                                           "order by \"EladarSum\" desc"))
        sumSoldTetel = dbGetQuery(dbConnection, "select sum(szamlatetel.eladar * szamlatetel.mennyiseg) from szamlatetel")
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
                                     paste("select first", topX, "termek.nev, sum(szamlatetel.eladar * szamlatetel.mennyiseg) as \"EladarSum\"",
                                           "from szamlatetel join ", 
                                           "termek on termek.id_termek = szamlatetel.id_termek join",
                                           "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo",
                                           "where forgalmazo.nev like '%FARMMIX%'",
                                           "group by termek.nev",
                                           "order by \"EladarSum\" desc"))
        sumSoldTetel = dbGetQuery(dbConnection, "select sum(szamlatetel.eladar * szamlatetel.mennyiseg) from szamlatetel")
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
                                   paste("select first", topX, "termek.nev, sum(szamlatetel.eladar * szamlatetel.mennyiseg) as \"EladarSum\"",
                                         "from szamlatetel join ", 
                                         "termek on termek.id_termek = szamlatetel.id_termek",
                                         "group by termek.nev",
                                         "having sum(szamlatetel.eladar * szamlatetel.mennyiseg) > 0",
                                         "order by \"EladarSum\""))
        sumSoldTetel = dbGetQuery(dbConnection, "select sum(szamlatetel.eladar * szamlatetel.mennyiseg) from szamlatetel")
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
                                   paste("select first", topX, "termek.nev, sum(szamlatetel.eladar * szamlatetel.mennyiseg) as \"EladarSum\"",
                                         "from szamlatetel join ", 
                                         "termek on termek.id_termek = szamlatetel.id_termek join",
                                         "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo",
                                         "where forgalmazo.nev like '%FARMMIX%'",
                                         "group by termek.nev",
                                         "having sum(szamlatetel.eladar * szamlatetel.mennyiseg) > 0",
                                         "order by \"EladarSum\""))
        sumSoldTetel = dbGetQuery(dbConnection, "select sum(szamlatetel.eladar * szamlatetel.mennyiseg) from szamlatetel")
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

plotFarmmixProductsRatioForYears = function (connections, timeDivision="year") {
    if(is.list(connections)) {
        products = data.frame(date=numeric(),
                              type=character(),
                              value=numeric(),
                              row.names = NULL,
                              stringsAsFactors = F)
        baseCommand = paste("sum(szamlatetel.eladar * szamlatetel.mennyiseg)",
                       "from szamlatetel join ", 
                       "szamla on szamla.id_szamla = szamlatetel.id_szamla join",
                       "termek on termek.id_termek = szamlatetel.id_termek join",
                       "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo",
                       "where szamlatetel.mennyiseg >=0")
        if (timeDivision == "year") {
            baseCommand = paste("select extract(year from szamla.datum) as \"Datum\",", baseCommand)
        } else {
            baseCommand = paste("select extract(year from szamla.datum) || '-' || extract(month from szamla.datum) || '-01' as \"Datum\",", baseCommand)
        }
        for(connection in connections) {
            fmxProducts = dbGetQuery(connection$Connection,
                                 paste(baseCommand,
                                       "and forgalmazo.nev like '%FARMMIX%'",
                                       "group by \"Datum\"",
                                       "order by \"Datum\"")
            )
            names(fmxProducts) = c("date", "value")
            nonFmxProducts = dbGetQuery(connection$Connection,
                                 paste(baseCommand,
                                       "and forgalmazo.nev not like '%FARMMIX%'",
                                       "group by \"Datum\"",
                                       "order by \"Datum\"")
            )
            names(nonFmxProducts) = c("date", "value")
            fmxRowCount = nrow(fmxProducts)
            nonFmxRowCount = nrow(nonFmxProducts)
            products = rbind(products,
                             data.frame(date=fmxProducts[,1],
                                        type="Fmx",
                                        value=fmxProducts[,2],
                                        row.names = NULL,
                                        stringsAsFactors = F)
                             )
            products = rbind(products,
                             data.frame(date=nonFmxProducts[,1],
                                        type="Non-Fmx",
                                        value=nonFmxProducts[,2],
                                        row.names=NULL,
                                        stringsAsFactors = F)
                             )
            
            if (timeDivision == "month") {
                fmxProducts$date = as.Date(fmxProducts$date)
                fmxProducts = arrange(fmxProducts, date)
                writeLines(paste("Hónap: ",
                                 format(fmxProducts$date, "%y-%m"),
                                 " | Fmx eladások: ",
                                 ft.format(fmxProducts[,2], rounding="million"),
                                 " | Nem-Fmx eladások: ",
                                 ft.format(nonFmxProducts[,2], rounding="million"),
                                 " | Arány(Fmx/Nem-Fmx): ",
                                 round(fmxProducts$value / nonFmxProducts$value * 100),
                                 "% | Arány(Fmx/Összes): ",
                                 round(fmxProducts$value / (nonFmxProducts$value + fmxProducts$value) * 100), 
                                 "%"
                                 , sep=""))
            } else {
                writeLines(paste("Év: ",
                                 fmxProducts$date,
                                 " | Fmx eladások: ",
                                 ft.format(fmxProducts[,2], rounding="million"),
                                 " | Nem-Fmx eladások: ",
                                 ft.format(nonFmxProducts[,2], rounding="million"),
                                 " | Arány(Fmx/Nem-Fmx): ",
                                 round(fmxProducts$value / nonFmxProducts$value * 100),
                                 "% | Arány(Fmx/Összes): ",
                                 round(fmxProducts$value / (nonFmxProducts$value + fmxProducts$value) * 100), 
                                 '%'
                                 , sep=""))
            }
        }
        
        products$type = as.factor(products$type)
        products = arrange(products, date)
        if (timeDivision == "month") {
            products$date = as.Date(products$date)
        }
        # Fontos, hogy geom_area esetén ne legyen túl sűrű az X tengely (year).
        # Ha napira kérdeztem le, akkor nagyon szőrös lett az egész
        # Másik ami fontos, hogy ha tételesen kérdeztem le (nem group by-jal) akkor
        # szarul jelenítette meg. Nem volt kitöltött terület, csak vonalak
        plot = ggplot(products, aes(x=date, y=value, fill=type, colour=type)) +
            geom_area(position="stack") +
            ylab("Eladás (Millió Ft)") +
            xlab("Dátum") +
            scale_y_continuous(labels=plotYCont)
        if (timeDivision == "month") {
            plot = plot + scale_x_date(labels=date_format("%y-%m"))
        }
        plot(plot)
    }
}


drawTopXProducts(connection, 10)
drawTopXFarmmixProducts(connection, 10)
topXIsWhatPercentOfAllProducts(connection, 5)
topXFarmmixIsWhatPercentOfAllProducts(connection, 5)
worstXProducts(connection, 20)
worstXFarmmixProducts(connection, 20)
plotFarmmixProductsRatioForYears(connections, 'year')
plotFarmmixProductsRatioForYears(connections, 'month')

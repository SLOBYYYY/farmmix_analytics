#install.packages("RJDBC", dependencies=T)
library("RJDBC")
drv = JDBC("org.firebirdsql.jdbc.FBDriver",
           "/home/sloby/projects/sandbox/r-jdbc/jdbc-driver/jaybird-full-2.2.7.jar",
           identifier.quote="`")
# Firebird is very sensitive to the URL format. This should be used: jdbc:firebirdsql://host:port//path/to/teh/shit.fdb
connection = dbConnect(drv, 
                       "jdbc:firebirdsql://127.0.0.1:3050//databases/dbs.fdb",
                       "SYSDBA", "8ggOhIPO")

library(plyr)

drawPieChart = function (data, text) {
    # data's first 2 columns should be labels and values
    labels = data[,1]
    slices = data[,2]
    percentages = round(slices/sum(slices)*100)
    labels = paste(labels, percentages)
    labels = paste(labels, "%", sep="")
    labels = paste(labels, " (", slices, ")", sep="")
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


#iconv(termekcsoport$NEV, "latin1", "UTF-8")

###############################################################################
# We are interested in the top selling products
#termek = dbGetQuery(connection, "select * from termek")
szamla =  dbGetQuery(connection, "select * from szamla")
szamlatetel =  dbGetQuery(connection, "select * from szamlatetel")
szamlatetel.frequency = data.frame(table(szamlatetel$ID_TERMEK))
termek.length = length(unique(szamlatetel$ID_TERMEK))
# We get the top 10% selling products
szamlatetel.frequency.top10 = arrange(szamlatetel.frequency,-Freq)[1:(termek.length/10),]
szamlatetel.frequency.top10 = rename(szamlatetel.frequency.top10, c('Var1'='ID_TERMEK'))
# Get the names of these top selling products
szamlatetel.frequency.top10$NEV = termek$NEV[termek$ID_TERMEK %in% szamlatetel.frequency.top10$ID_TERMEK]
# Sum the price of all termek
termek.by.sale = aggregate(. ~ ID_TERMEK, data=szamlatetel[,c('ID_TERMEK', 'ELADAR')], FUN=sum)
termek.by.sale.top10 = arrange(termek.by.sale,-ELADAR)[1:(termek.length/10),]
termek.by.sale.top10 = rename(termek.by.sale.top10, c('ID_TERMEK'='Id', 'ELADAR'='Eladar.sum'))

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
    
}

termek.full = data.frame('Id' = termek$ID_TERMEK, 'Nev'=termek$NEV, 'Eladar.Sum'=NA, 'Eladott.Menny'=NA, 'Egyseg.Ertek'=NA)
termek.full = arrange(termek.full, Id)
termek.full$Eladar.Sum = sapply(termek.full$Id, function (x) { if (x %in% termek.by.sale$ID_TERMEK) { termek.by.sale$ELADAR[which(termek.by.sale$ID_TERMEK==x)] } else { NA } })
termek.full[!is.na(termek.full$Eladar.Sum), 'Eladott.Menny'] = szamlatetel.frequency[ ,'Freq']
termek.full[!is.na(termek.full$Eladar.Sum), 'Egyseg.Ertek'] = termek.full[!is.na(termek.full$Eladar.Sum),'Eladar.Sum'] / termek.full[!is.na(termek.full$Eladar.Sum),'Eladott.Menny']
termek.full.ordered = arrange(termek.full, -Eladar.Sum)

# We plot a barplot for the top 5 products that sell
top.x.termek = 5
termek.barplot = barplot(termek.full.ordered[1:top.x.termek,'Eladar.Sum'], 
                         names.arg=termek.full.ordered[1:top.x.termek,'Nev'], 
                         cex.names=0.6, 
                         xlab="Top Termékek",
                         ylab="Eladott mennyiség (Ft)", 
                         ylim=c(0,max(termek.full.ordered[1:5,'Eladar.Sum']) * 1.2))
text(termek.barplot, termek.full.ordered[1:5,'Eladar.Sum'], labels=termek.full.ordered[1:5,'Eladar.Sum'], pos=3)

# What percent of the top product sales is of the total sales?
paste(round(termek.full.ordered[1,'Eladar.Sum'] / sum(termek.full.ordered[,'Eladar.Sum'],na.rm=T) * 100, 2), '%')

forgalmazo.farmmix.ids = dbGetQuery(connection, "select id_forgalmazo from forgalmazo where nev like '%FARMMIX%'")
termek.farmmix = termek[termek$ID_FORGALMAZO %in% forgalmazo.farmmix.ids$ID_FORGALMAZO,]
termek.farmmix.ordered = na.omit(termek.full.ordered[termek.full.ordered$Id %in% termek.farmmix$ID_TERMEK,])

termek.farmmix.barplot = barplot(termek.farmmix.ordered[1:top.x.termek,'Eladar.Sum'], 
                         names.arg=termek.farmmix.ordered[1:top.x.termek,'Nev'], 
                         cex.names=0.5, 
                         xlab="Top Farmmix termékek",
                         ylab="Eladott mennyiség (Ft)", 
                         ylim=c(0,max(termek.farmmix.ordered[1:5,'Eladar.Sum']) * 1.2))
text(termek.farmmix.barplot, termek.farmmix.ordered[1:5,'Eladar.Sum'], labels=termek.farmmix.ordered[1:5,'Eladar.Sum'], pos=3)

# What percent of the top 20% product sales is of the total sales?
paste(round(sum(termek.full.ordered[1:(round(dim(na.omit(termek.full.ordered))[1] * 0.05)),'Eladar.Sum'],na.rm=T) / sum(termek.full.ordered[,'Eladar.Sum'],na.rm=T) * 100, 2), '%')

forgalmazo.farmmix.ids = dbGetQuery(connection, "select id_forgalmazo from forgalmazo where nev like '%FARMMIX%'")
termek.farmmix = termek[termek$ID_FORGALMAZO %in% forgalmazo.farmmix.ids$ID_FORGALMAZO,]
termek.farmmix.ordered = na.omit(termek.full.ordered[termek.full.ordered$Id %in% termek.farmmix$ID_TERMEK,])

termek.farmmix.barplot = barplot(termek.farmmix.ordered[1:top.x.termek,'Eladar.Sum'], 
                         names.arg=termek.farmmix.ordered[1:top.x.termek,'Nev'], 
                         cex.names=0.5, 
                         xlab="Top Farmmix termékek",
                         ylab="Eladott mennyiség (Ft)", 
                         ylim=c(0,max(termek.farmmix.ordered[1:5,'Eladar.Sum']) * 1.2))
text(termek.farmmix.barplot, termek.farmmix.ordered[1:5,'Eladar.Sum'], labels=termek.farmmix.ordered[1:5,'Eladar.Sum'], pos=3)


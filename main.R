#install.packages("RJDBC", dependencies=T)
library('RJDBC')
library('ggplot2')
library('plyr')
dbPassword = "PcL233yW"
the_year=2015

fmxYear = function (connection, year) {
    object = list(Connection = connection, 
                  Year = year)
    class(object) = "fmxYear"
    return(object)
}
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
connection_2012 = dbConnect(drv, 
                       paste("jdbc:firebirdsql://127.0.0.1:3050//databases/", "dbs_bosz_2012.fdb", sep=""),
                       "SYSDBA", dbPassword)
connections = list(fmxYear(connection_2012, 2012),
                fmxYear(connection_2013, 2013),
                fmxYear(connection_2014, 2014),
                fmxYear(connection, 2015))

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
    text(data.barplot, data[,2], labels=ft.format(data[,2], rounding="million"), pos=3, cex=0.7)
    legend("topright", legend = data[,1], fill=colors, cex=0.7)
}

filterOnePercentRows = function (data) {
    data.sum = sum(data[,2])
    data$percent = round(data[,2] / data.sum * 100)
    data.filtered = data[which(data$percent > 1),]
    small.group.difference = data.sum - sum(data.filtered[,2])
    small.group.percentage = round(small.group.difference / data.sum * 100)
    additional.row = data.frame("1% alattiak", small.group.difference, small.group.percentage)
    colnames(additional.row) = colnames(data)
    data.filtered = rbind(data.filtered, additional.row)
    return (data.filtered)
}

ft.format = function (szam, rounding="none") {
    switch(rounding,
           none = paste(format(round(szam,0),big.mark=","),"Ft"),
           thousand = paste(format(round(szam/1000,1),big.mark=","),"e Ft"),
           million = paste(format(round(szam/1000000,2),big.mark=","),"M Ft")
           )
}

drawTermekCsoportForSoldTermekek = function (dbConnection) {
    termekcsoport.freq = dbGetQuery(dbConnection, 
               paste("select csoport.nev, count(szamlatetel.id_termek) ",
               "from szamlatetel ",
               "join termek on szamlatetel.id_termek = termek.id_termek ",
               "join csoport on csoport.id_csoport = termek.id_csoport",
               "group by csoport.nev"))
    termekcsoport.filtered = filterOnePercentRows(termekcsoport.freq)
    drawPieChart(termekcsoport.filtered, "Termékcsoportok megoszlása az eladott termékek között")
}

printYearlySales = function (dbConnection, year, to="end") {
    command = paste("select sum(eladar * mennyiseg) ",
                                    "from szamlatetel join",
                                    "szamla on szamla.id_szamla = szamlatetel.id_szamla",
                                    "where extract(year from szamla.datum) = ", year)
    if (to == "today") {
        command = paste(command, " and szamla.datum <= '", year, "-", format(Sys.Date(), "%m-%d"), "'", sep = "")
    }
    yearly.sales = dbGetQuery(dbConnection, command)
    text = paste("Teljes éves bevétel (", year, "):")
    if (to == "today") {
        text = paste("Teljes éves bevétel (", year, "-", format(Sys.Date(), "%m-%d"), "-ig)", sep = "")
    }
    print(paste(text, ft.format(yearly.sales)))
}

plotYearlySales = function (dbConnection, year, time_divide="weeks") {
    sales.by.date = dbGetQuery(dbConnection,
               paste("select datum, sum(eladar * mennyiseg) ",
                     "from SZAMLATETEL join", 
                     "SZAMLA on SZAMLA.ID_SZAMLA = SZAMLATETEL.ID_SZAMLA",
                     "where extract(year from datum) =", year,
                     "group by datum order by datum")
               )
    names(sales.by.date) = c("datum","sum")
    sales.by.date$datum = as.Date(sales.by.date$datum, '%Y-%m-%d')
    require(ggplot2)
    require(scales)

    # Default week format
    label.format = "%m-%d"
    if (time_divide == "month") {
        label.format = '%b'
    }
    ggplot(data = sales.by.date, aes(datum, sum)) + 
        geom_line() +
        stat_smooth(method="loess") +
        ylab("Eladás (Millió Ft)") +
        xlab("Dátum") +
        scale_y_continuous(labels=function (x) { paste(round(x/1000000,1), " Millió Ft")}) +
        scale_x_date(breaks=date_breaks(time_divide), labels=date_format(label.format))
}

printYearlyReturns = function (dbConnection) {
    yearly.returns = dbGetQuery(dbConnection,
                              "select count(id_visszaaru) from visszaaru")
    print(paste("Visszáruk száma:", yearly.returns))
}

drawAreaPlot = function (connections) {
    if(is.list(connections)) {
        year.groups = data.frame(year=as.numeric(), label=as.character(), value=as.numeric(), row.names = NULL)
        for(connection in connections) {
            groups = dbGetQuery(connection$Connection,
                       #paste("select csoport.nev, count(szamlatetel.id_termek) ",
                       paste("select csoport.nev, szamlatetel.id_termek",
                       "from szamlatetel ",
                       "join termek on szamlatetel.id_termek = termek.id_termek ",
                       "join csoport on csoport.id_csoport = termek.id_csoport")
                       #"join csoport on csoport.id_csoport = termek.id_csoport",
                       #"group by csoport.nev")
            )
            year = rep(connection$Year, nrow(groups))
            year.groups = rbind(year.groups, 
                                data.frame(year=year,
                                           label=groups$NEV,
                                           #value=groups$COUNT, row.names=NULL)
                                           value=groups$ID_TERMEK, row.names=NULL)
                                )
        }
        ggplot(year.groups, aes(factor(year), fill=label)) +
            geom_bar()
    } else if(class(connections) == "fmxYear") {
            groups = dbGetQuery(connections$Connection,
                       #paste("select csoport.nev, count(szamlatetel.id_termek) ",
                       paste("select csoport.nev, szamlatetel.id_termek",
                       "from szamlatetel ",
                       "join termek on szamlatetel.id_termek = termek.id_termek ",
                       "join csoport on csoport.id_csoport = termek.id_csoport")
                       #"join csoport on csoport.id_csoport = termek.id_csoport",
                       #"group by csoport.nev")
            )
            ggplot(groups, aes(factor(connections$Year), fill=label)) +
                geom_bar()
    }
}
drawAreaPlot(connections)

drawTermekCsoportForSoldTermekek(connection)

printYearlySales(connection_2012, 2012)
printYearlySales(connection_2012, 2012, "today")
plotYearlySales(connection_2012, 2012, "month")
printYearlySales(connection_2013, 2013)
printYearlySales(connection_2013, 2013, "today")
plotYearlySales(connection_2013, 2013, "month")
printYearlySales(connection_2014, 2014)
printYearlySales(connection_2014, 2014, "today")
plotYearlySales(connection_2014, 2014, "month")
printYearlySales(connection, the_year)
printYearlySales(connection, the_year, "today")
plotYearlySales(connection, the_year, "month")
printYearlyReturns(connection_2012)
printYearlyReturns(connection_2013)
printYearlyReturns(connection_2014)
printYearlyReturns(connection)

library("RJDBC")
drv = JDBC("org.firebirdsql.jdbc.FBDriver",
           "/home/sloby/projects/sandbox/r-jdbc/jdbc-driver/jaybird-full-2.2.7.jar",
           identifier.quote="`")
# Firebird is very sensitive to the URL format. This should be used: jdbc:firebirdsql://host:port//path/to/teh/shit.fdb
connection = dbConnect(drv, 
                       "jdbc:firebirdsql://127.0.0.1:3050//databases/dbs.fdb",
                       "SYSDBA", "8ggOhIPO")

drawPieChartForUzletkotokBySales = function (dbConnection) {
    uzletkotok.sales = dbGetQuery(dbConnection, paste("select uzletkoto.nev, sum(szamlatetel.eladar)",
                                   "from szamlatetel join",  
                                   "szamla on szamla.id_szamla = szamlatetel.id_szamla join",
                                   "uzletkoto on uzletkoto.id_uzletkoto = szamla.id_uzletkoto",
                                   "group by uzletkoto.nev"))
    drawPieChart(uzletkotok.sales, "Üzletkötők eladási adatai")
}

drawPieChartForUzletkotokByFarmmixSales = function (dbConnection) {
    uzletkotok.sales = dbGetQuery(dbConnection, paste("select uzletkoto.nev, sum(szamlatetel.eladar)",
                                   "from szamlatetel join",  
                                   "szamla on szamla.id_szamla = szamlatetel.id_szamla join",
                                   "uzletkoto on uzletkoto.id_uzletkoto = szamla.id_uzletkoto join",
                                   "termek on termek.id_termek = szamlatetel.id_termek join",
                                   "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo",
                                   "where forgalmazo.nev like '%FARMMIX%'",
                                   "group by uzletkoto.nev"))
    drawPieChart(uzletkotok.sales, "Üzletkötők eladási adatai")
}

drawPieChartForUzletkotokBySales(connection)
drawPieChartForUzletkotokByFarmmixSales(connection)

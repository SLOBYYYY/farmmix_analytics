# Firebird is very sensitive to the URL format. This should be used: jdbc:firebirdsql://host:port//path/to/teh/shit.fdb
getUzletkotokSalesForAllProducts = function (dbConnection) {
    uzletkotok.sales.all = dbGetQuery(dbConnection, paste("select uzletkoto.nev, sum(szamlatetel.eladar)",
                                   "from szamlatetel join",  
                                   "szamla on szamla.id_szamla = szamlatetel.id_szamla join",
                                   "uzletkoto on uzletkoto.id_uzletkoto = szamla.id_uzletkoto",
                                   "group by uzletkoto.nev"))
    return (uzletkotok.sales.all)
}

getUzletkotokSalesForFarmmixProducts = function (dbConnection) {
    uzletkotok.sales.farmmix = dbGetQuery(dbConnection, paste("select uzletkoto.nev, sum(szamlatetel.eladar)",
                                   "from szamlatetel join",  
                                   "szamla on szamla.id_szamla = szamlatetel.id_szamla join",
                                   "uzletkoto on uzletkoto.id_uzletkoto = szamla.id_uzletkoto join",
                                   "termek on termek.id_termek = szamlatetel.id_termek join",
                                   "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo",
                                   "where forgalmazo.nev like '%FARMMIX%'",
                                   "group by uzletkoto.nev"))
    return (uzletkotok.sales.farmmix)
}

drawPieChartForUzletkotokBySales = function (dbConnection) {
    uzletkotok.sales.all = getUzletkotokSalesForAllProducts(dbConnection)
    drawPieChart(uzletkotok.sales.all, "Üzletkötők eladási adatai")
}

drawPieChartForUzletkotokByFarmmixSales = function (dbConnection) {
    uzletkotok.sales.farmmix = getUzletkotokSalesForFarmmixProducts(dbConnection)
    drawPieChart(uzletkotok.sales.farmmix, "Üzletkötők Farmmixes eladási adatai")
}

drawBarChartForUzletkotokByAllSales = function (dbConnection) {
    uzletkotok.sales.all = getUzletkotokSalesForAllProducts(dbConnection)
    drawBarChart(uzletkotok.sales.all, "Üzletkötők eladási adatai", "Forint")
}

drawBarChartForUzletkotokByFarmmixSales = function (dbConnection) {
    uzletkotok.sales.all = getUzletkotokSalesForFarmmixProducts(dbConnection)
    drawBarChart(uzletkotok.sales.all, "Üzletkötők Farmmixes eladási adatai", "Forint")
}

drawPieChartForUzletkotokBySales(connection)
drawPieChartForUzletkotokByFarmmixSales(connection)
drawBarChartForUzletkotokByAllSales(connection)
drawBarChartForUzletkotokByFarmmixSales(connection)

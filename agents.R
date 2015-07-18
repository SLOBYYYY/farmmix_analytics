getUzletkotokSalesForAllProducts = function (dbConnection) {
    uzletkotok.sales.all = dbGetQuery(dbConnection, paste("select uzletkoto.nev, sum(szamlatetel.eladar * szamlatetel.mennyiseg)",
                                   "from szamlatetel join",  
                                   "szamla on szamla.id_szamla = szamlatetel.id_szamla join",
                                   "uzletkoto on uzletkoto.id_uzletkoto = szamla.id_uzletkoto",
                                   "group by uzletkoto.nev"))
    return (uzletkotok.sales.all)
}

getUzletkotokSalesForFarmmixProducts = function (dbConnection) {
    uzletkotok.sales.farmmix = dbGetQuery(dbConnection, paste("select uzletkoto.nev, sum(szamlatetel.eladar * szamlatetel.mennyiseg)",
                                   "from szamlatetel join",  
                                   "szamla on szamla.id_szamla = szamlatetel.id_szamla join",
                                   "uzletkoto on uzletkoto.id_uzletkoto = szamla.id_uzletkoto join",
                                   "termek on termek.id_termek = szamlatetel.id_termek join",
                                   "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo",
                                   "where forgalmazo.nev like '%FARMMIX%'",
                                   "group by uzletkoto.nev"))
    return (uzletkotok.sales.farmmix)
}

plotPieChartForUzletkotokBySales = function (dbConnection) {
    uzletkotok.sales.all = getUzletkotokSalesForAllProducts(dbConnection)
    plotPieChart(uzletkotok.sales.all, "Üzletkötők eladási adatai")
}

plotPieChartForUzletkotokByFarmmixSales = function (dbConnection) {
    uzletkotok.sales.farmmix = getUzletkotokSalesForFarmmixProducts(dbConnection)
    plotPieChart(uzletkotok.sales.farmmix, "Üzletkötők Farmmixes eladási adatai")
}

plotBarChartForUzletkotokByAllSales = function (dbConnection) {
    uzletkotok.sales.all = getUzletkotokSalesForAllProducts(dbConnection)
    plotBarChart(uzletkotok.sales.all, "Üzletkötők eladási adatai", "Forint")
}

plotBarChartForUzletkotokByFarmmixSales = function (dbConnection) {
    uzletkotok.sales.all = getUzletkotokSalesForFarmmixProducts(dbConnection)
    plotBarChart(uzletkotok.sales.all, "Üzletkötők Farmmixes eladási adatai", "Forint")
}
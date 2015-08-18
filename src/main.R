#install.packages("RJDBC", dependencies=T)
library('RJDBC')
library('ggplot2')
library('plyr')
library('scales')
library('forecast')
source('helper.R')
source('agents.R')
source('sales.R')
source('customers.R')
source('forecast.R')
source('TopX.R')
dbPassword = "PcL233yW"
the_year=2015

drv = JDBC("org.firebirdsql.jdbc.FBDriver",
           "../jdbc-driver/jaybird-full-2.2.7.jar",
           identifier.quote="`")
# Firebird is very sensitive to the URL format. This should be used: jdbc:firebirdsql://host:port//path/to/teh/shit.fdb
connection_live = connect.live()
dbDisconnect(conn = connection_live)
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
connections = list(fmxYear(connection_2010_2012, 2010),
                fmxYear(connection_2013, 2013),
                fmxYear(connection_2014, 2014),
                fmxYear(connection, 2015))



# Agents
############
#plotPieChartForUzletkotokBySales(connection)
#plotPieChartForUzletkotokByFarmmixSales(connection)
plotBarChartForUzletkotokByAllSales(connection)
plotBarChartForUzletkotokByFarmmixSales(connection)

# Sales 
###############
plotTermekCsoportForSoldTermekek(connection)
plotTopXProducts(connection, 10, plot=T)
plotTopXFarmmixProducts(connection, 10, T)
fmx = plotTopXFarmmixProducts(connection, 10)
plotProductSaleForLifetime(connections, as.character(fmx$name)[1:3])
topXIsWhatPercentOfAllProducts(connection, 10)
topXFarmmixIsWhatPercentOfAllProducts(connection, 10)
# A farmmix és farmmix alternatív forgalmazót külön kéne venni és
# a nem farmmixes termékek csak a nem farmmixeseket jelenítsék meg
worstXProducts(connection, 100)
worstXFarmmixProducts(connection, 20)

printYearlySales(connection_2010_2012, 2010)
printYearlySales(connection_2010_2012, 2010, "today")
plotYearlySales(connection_2010_2012, 2010, "month")
printYearlySales(connection_2010_2012, 2011)
printYearlySales(connection_2010_2012, 2011, "today")
plotYearlySales(connection_2010_2012, 2011, "month")
# Mi ez a nagy minusz március előtt?
printYearlySales(connection_2010_2012, 2012)
printYearlySales(connection_2010_2012, 2012, "today")
plotYearlySales(connection_2010_2012, 2012, "month")
printYearlySales(connection_2013, 2013)
printYearlySales(connection_2013, 2013, "today")
plotYearlySales(connection_2013, 2013, "month")
printYearlySales(connection_2014, 2014)
printYearlySales(connection_2014, 2014, "today")
plotYearlySales(connection_2014, 2014, "month")
printYearlySales(connection, the_year)
printYearlySales(connection, the_year, "today")
plotYearlySales(connection, the_year, "month")
printYearlyReturns(connection_2010_2012)
printYearlyReturns(connection_2013)
printYearlyReturns(connection_2014)
printYearlyReturns(connection)

plotFarmmixProductsRatioForYears(connections, 'year')
plotFarmmixProductsRatioForYears(connections, 'month')
#plotAreaPlot(connections) # not good yet


# Customers
##############
topXBestCustomerForAllProducts(connection, 10)
topXBestCustomerForFarmmixProducts(connection, 10)
# Külön legyen itt is az alternatív és a sima farmmixes
topXRecurringCustomers(connection, 10)
#topXLatePayers(connection, 10) # not good


# Forecast
##############
fit = forecastSales(connections)
plot(fit)
fit


# Átlagos eladási ár kell (pl top 20 terméknél) + mennyiség

# Átküldeni holnapra:
# illetve külön kéne a vetőmagokat, növényvédőszert és műtrágyákat venni
# növényvédőszert is tovább kell bondani: farmmix, alternatív, többi
# top 30 kell és mennyi lett belőle eladva illetve az átlagár
# hány százalékot ér el a saját csoportján belül(!) -> pl top nem farmmixes
# az összes nem farmmixes közül
for(product.group in levels(factor(c("vetomag", "novenyvedoszer", "mutragya")))) {
    filename = paste("report/", product.group, "_osszesszolgaltato.csv", sep="")
    topx.products = plotTopXProducts(connection_live, 30, product.group = product.group, plot=F)
    write.csv(topx.products$result, filename)
    write.table(topx.products$top.x.percent, filename, append=T, row.names=F, col.names=F)
}
for (provider in levels(factor(c("farmmix", "alternativ", "notfarmmix")))) {
    filename = paste("report/", provider, "_osszestermekcsoport.csv", sep="")
    topx.products = plotTopXProducts(connection_live, 30, provider = provider, plot=F)
    write.csv(topx.products$result, filename)
    write.table(topx.products$top.x.percent, filename, append=T, row.names=F, col.names=F)
    for(product.group in levels(factor(c("vetomag", "novenyvedoszer", "mutragya")))) {
        filename = paste("report/", provider,"_", product.group, ".csv", sep="")
        topx.products = plotTopXProducts(connection_live, 30, provider = provider, product.group = product.group, plot=F)
        write.csv(topx.products$result, filename)
        write.table(topx.products$top.x.percent, filename, append=T, row.names=F, col.names=F)
    }
}

agent.data=dbGetQuery(connection, "select id_uzletkoto, nev from uzletkoto")
colnames(agent.data) = c('id', 'name')

#for (i in seq(along=agent.data$id)) {
for (i in 20) {
    topx.products = plotTopXProductsForAgent(connection, 30, provider=NULL, product.group=NULL, agent.data=as.vector(agent.data[i,]), plot=F)
    print(topx.products)
}

# Üzletkötő metrika: mennyit késnek a vevői
# Üzletkötő metrika 2: mennyi tartozást visz át a következő évre
# Üzletkötő metrika 3: mennyi az átlagos késése a vevőinek
### Üzletkötő metrika 4: Üzletkötőnként a top x termék átlaga, szórása, mennyisége, szummája
# Üzletkötő metrika 5: ugyanaz mint a termék metrika lent, a gyári ár és a termék átlagáraknál
#                       (megj.: ez mehet a 4. metrika mellé nyugodtan)
### Üzletkötő metrika 6: top x vevője a bevételének hány százaléka
# Üzletkötő metrika 7: hány új vevője volt az évben
# Üzletkötő metrika 8: hány inaktív vevője van (nem vásárolt semmit)
# Üzletkötő metrika 9: forecast a jövő éves bevételre


# Termék metrika: a gyári árhoz képest mennyi volt a termék átlag eladási ára százalékosan (minusz plusz x% differencia)
#                 (megjegyzés: a gyári ár kb márciustól aktuális)

source('TopX.R')
ag = TopX(connection)
ag$load()
ag$reportProduct(10)
ag$reportCustomer(10)
ag$reportCustomer(10, agent.data = agent.data[1,])
ag$reportProduct(10, product.group = "farmmix", agent.data=agent.data[1,])



command = paste("select szamlatetel.eladar * szamlatetel.mennyiseg as \"EladarSum\", ",
                " csoport.nev, forgalmazo.nev, uzletkoto.nev, termek.id_termek, termek.nev,",
                " vevo.id_vevo, termek.gyariar, szamlatetel.eladar, termek.eladar",
                " from szamlatetel join ", 
                " szamla on szamla.id_szamla = szamlatetel.id_szamla join",
                " vevo on vevo.id_vevo = szamla.id_vevo join",
                " termek on termek.id_termek = szamlatetel.id_termek join",
                " forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo join",
                " csoport on csoport.id_csoport = termek.id_csoport left join",
                " uzletkoto on uzletkoto.id_uzletkoto = szamla.id_uzletkoto",
                " where szamla.datum >='2015-01-01' and szamla.datum <='2015-06-01'", 
                sep="")
temp = dbGetQuery(connection, command)
colnames(temp) = c("totalprice", "group_name", "provider_name", "agent_name", "product_id", "product_name", "customer_id", "factory_price", "sale_price", "current_price")
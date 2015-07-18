#install.packages("RJDBC", dependencies=T)
library('RJDBC')
library('ggplot2')
library('plyr')
library('scales')
library('forecast')
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
connection_2010_2012 = dbConnect(drv, 
                       paste("jdbc:firebirdsql://127.0.0.1:3050//databases/", "dbs_bosz_2010-2012.fdb", sep=""),
                       "SYSDBA", dbPassword)
connections = list(fmxYear(connection_2010_2012, 2010),
                fmxYear(connection_2013, 2013),
                fmxYear(connection_2014, 2014),
                fmxYear(connection, 2015))

source('helper.R')
source('agents.R')
source('sales.R')
source('customers.R')
source('forecast.R')



# Agents
############
#plotPieChartForUzletkotokBySales(connection)
#plotPieChartForUzletkotokByFarmmixSales(connection)
plotBarChartForUzletkotokByAllSales(connection)
plotBarChartForUzletkotokByFarmmixSales(connection)

# Sales 
###############
plotTermekCsoportForSoldTermekek(connection)
plotTopXProducts(connection, 10)
plotTopXFarmmixProducts(connection, 10)
plotProductSaleForLifetime(connections, 'ADENGO  5')
topXIsWhatPercentOfAllProducts(connection, 5)
topXFarmmixIsWhatPercentOfAllProducts(connection, 5)
worstXProducts(connection, 20)
worstXFarmmixProducts(connection, 20)

printYearlySales(connection_2010_2012, 2010)
printYearlySales(connection_2010_2012, 2010, "today")
plotYearlySales(connection_2010_2012, 2010, "month")
printYearlySales(connection_2010_2012, 2011)
printYearlySales(connection_2010_2012, 2011, "today")
plotYearlySales(connection_2010_2012, 2011, "month")
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
topXRecurringCustomers(connection, 10)
#topXLatePayers(connection, 10) # not good


# Forecast
##############
fit = forecastSales(connections)
plot(fit)

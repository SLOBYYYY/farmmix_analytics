library('RJDBC')
library('ggplot2')
library('plyr')
library('scales')
library('forecast')
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

temp = data.frame(ID_VEVO=numeric(), NEV=character(), ID_SZAMLA=numeric(), SUM=numeric(), COUNT=numeric(), DATUM=numeric())
getData = function (year, connection) {
    command = paste("select v.id_vevo, v.nev, sz.id_szamla, sum(szt.eladar * szt.mennyiseg), count(szt.id_szamlatetel),",
                    " extract(year from sz.datum) || '-' || extract(month from sz.datum) || '-01' as \"DATUM\"",
                    " from szamlatetel szt join",
                    " szamla sz on sz.id_szamla = szt.id_szamla join",
                    " vevo v on v.id_vevo = sz.id_vevo",
                    " where extract(year from sz.datum) = ", year,
                    " group by v.id_vevo, v.nev, sz.id_szamla, \"DATUM\"")
    return(dbGetQuery(connection, command))
}
temp = rbind(temp, getData(2010, connection_2010_2012))
temp = rbind(temp, getData(2011, connection_2010_2012))
temp = rbind(temp, getData(2012, connection_2010_2012))
temp = rbind(temp, getData(2013, connection_2013))
temp = rbind(temp, getData(2014, connection_2014))
temp = rbind(temp, getData(2015, connection))
colnames(temp) = c("customer_id", "customer_name", "bill_id", "totalprice", "item_count", "date")
temp$customer_name = as.factor(temp$customer_name)
temp$date = as.Date(temp$date)

eom <- function(date) {
    # date character string containing POSIXct date
    date.lt <- as.POSIXlt(as.character(date)) # add a month, then subtract a day:
    mon <- date.lt$mon + 2 
    year <- date.lt$year
    year <- year + as.integer(mon==13) # if month was December add a year
    mon[mon==13] <- 1
    iso = ISOdate(1900+year, mon, 1, hour=0, tz="GMT")
    result = as.POSIXct(iso) - 86400 # subtract one day
    return (as.Date(result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600))
}

#starting.year = 2015
starting.year = format(Sys.Date(), "%Y")
#starting.month = 8
starting.month = format(Sys.Date(), "%m")
this.year.beginning = seq(as.Date(paste(starting.year, "-", starting.month, "-01",sep="")), length=2, by="-12 months")[2]
this.year.end = eom(seq(this.year.beginning, length=2, by="11 months")[2])
previous.year.end = seq(as.Date(paste(starting.year, "-", starting.month, "-01",sep="")), length=2, by="-13 months")[2]
previous.year.beginning = seq(previous.year.end, length=2, by="-11 months")[2]
date.limits = data.frame(from=this.year.beginning, to=this.year.end)

while (previous.year.beginning %in% temp$date) {
    date.limits = rbind(date.limits, data.frame(from=previous.year.beginning, to=eom(previous.year.end)))
    previous.year.beginning = seq(previous.year.beginning, length=2, by="-12 months")[2]
    previous.year.end = seq(previous.year.end, length=2, by="-12 months")[2]
}

subsetRebuyingCustomers = function (customer.purchase.frequency) {
    return (customer.purchase.frequency >= 3)
}

getYearlyData = function (from, to, temp) {
    subtemp = subset(temp, temp$date >= from & temp$date <= to)
    
    customers.aggregated = aggregate(subtemp$bill_id, list(subtemp$customer_id), length)
    mean.frequency = round(mean(customers.aggregated$x), 2)
    median.frequency = median(customers.aggregated$x)
    
    customers.aggregated.with.freq = subset(customers.aggregated, subsetRebuyingCustomers(customers.aggregated$x))
    customers.aggregated.with.freq = customers.aggregated.with.freq$Group.1
    cases = length(customers.aggregated.with.freq)
    
    prev.year.beginning = seq(from, length=2, by="-12 months")[2]
    rebuy.percent = NA
    if (prev.year.beginning %in% temp$date) {
        prev.year.end = seq(to, length=2, by="-12 months")[2]
        subtemp.prev.year = subset(temp, temp$date >= prev.year.beginning & temp$date <= prev.year.end)
        customers.aggregated.prev.year = aggregate(subtemp.prev.year$bill_id, list(subtemp.prev.year$customer_id), length)
        customers.aggregated.prev.year.with.freq = subset(customers.aggregated.prev.year, subsetRebuyingCustomers(customers.aggregated.prev.year$x))
        customers.aggregated.prev.year.with.freq = customers.aggregated.prev.year.with.freq$Group.1
        rebuying.customers = subset(customers.aggregated.with.freq, customers.aggregated.with.freq %in% customers.aggregated.prev.year.with.freq)
        rebuy.percent = round(length(rebuying.customers) / length(customers.aggregated.with.freq) * 100, 1)
    }
    
    total.aggregated = aggregate(subtemp$totalprice, list(subtemp$customer_id), sum)
    total.aggregated.filtered = subset(total.aggregated, total.aggregated$Group.1 %in% customers.aggregated.with.freq)
    mean.spend = round(mean(total.aggregated.filtered$x))
    median.spend = median(total.aggregated.filtered$x)
    
    items.aggregated = aggregate(subtemp$item_count, list(subtemp$customer_id), sum)
    items.aggregated.filtered = subset(items.aggregated, items.aggregated$Group.1 %in% customers.aggregated.with.freq)
    items.per.customer = round(mean(items.aggregated.filtered$x),2)
    items.per.customer.median = median(items.aggregated.filtered$x)
    
    items.per.order = round(mean(subtemp$item_count),2)
    items.per.order.median = median(subtemp$item_count)
    
    return (c(mean.frequency, median.frequency, cases, rebuy.percent, mean.spend, median.spend, items.per.customer, items.per.customer.median, items.per.order, items.per.order.median))
}

rebuying.customers = cbind(format(date.limits$from, '%Y-%m'), format(date.limits$to, '%Y-%m'), t(apply(date.limits, 1, function (x) { getYearlyData(as.Date(x[1]), as.Date(x[2]), temp) })))
colnames(rebuying.customers) = c('period.start', 'period.end', 'mean.freq', 'median.freq', 'cases', 'rebuy.percent', 'mean.spend', 'median.spend', 'items.per.customer', 'items.per.customer.median', 'items.per.order', 'items.per.order.median')
rebuying.customers

# Egyértelműen nőtt a visszatérő vásárlók száma:
# Egy vevőre eső átlagos vásárlások száma 7.79 -> 8.26
# Egy vevőre eső median vásárlások száma 2 -> 3
# 3 vagy annál többet vásárló vásárlások száma: 1563 -> 1581
# Azon vevők akik az előző évben is 3x vagy többször vásároltak és visszatértek: 55.5 -> 63.1
# Az átlagos költekezésük ezen vevőknek nem nőtt, de a median igen: 456525 -> 484275
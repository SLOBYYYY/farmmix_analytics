library('RJDBC')
library('dplyr')
dbPassword = "PcL233yW"
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
getSaleData = function (year, connection) {
    command = paste("select v.id_vevo, v.nev, sz.id_szamla, sum(szt.eladar * szt.mennyiseg), count(szt.id_szamlatetel),",
                    " extract(year from sz.datum) || '-' || extract(month from sz.datum) || '-01' as \"DATUM\"",
                    " from szamlatetel szt join",
                    " szamla sz on sz.id_szamla = szt.id_szamla join",
                    " vevo v on v.id_vevo = sz.id_vevo",
                    " where extract(year from sz.datum) = ", year,
                    " group by v.id_vevo, v.nev, sz.id_szamla, \"DATUM\"")
    return(dbGetQuery(connection, command))
}
temp = rbind(temp, getSaleData(2010, connection_2010_2012))
temp = rbind(temp, getSaleData(2011, connection_2010_2012))
temp = rbind(temp, getSaleData(2012, connection_2010_2012))
temp = rbind(temp, getSaleData(2013, connection_2013))
temp = rbind(temp, getSaleData(2014, connection_2014))
temp = rbind(temp, getSaleData(2015, connection))
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

getRebuyerData = function (from, to, temp) {
    # Filter for the current from-to date interval (12 months)
    interval.data = subset(temp, temp$date >= from & temp$date <= to)
    # Aggregate for amount of POs
    customers.aggregated = aggregate(interval.data$bill_id, list(customer_id=interval.data$customer_id), length)
    # Filter these aggregated customers for the defined conditions (at least X times bought something)
    loyal.customers = subset(customers.aggregated, subsetRebuyingCustomers(customers.aggregated$x))
    #TODO: Alternative approach, only check if they bought just once this year
    loyal.customer.ids = loyal.customers$customer_id
    
    prev.year.beginning = seq(from, length=2, by="-12 months")[2]
    rebuy.percent = NA
    # Check if there was data for the previous year. We need this to check if the user is a rebuyer
    if (prev.year.beginning %in% temp$date) {
        prev.year.end = seq(to, length=2, by="-12 months")[2]
        # Filter for the previous 12 months before the interval.data's 
        interval.data.prev.year = subset(temp, temp$date >= prev.year.beginning & temp$date <= prev.year.end)
        # Aggregate for amount of POs for previous year
        customers.aggregated.prev.year = aggregate(interval.data.prev.year$bill_id, list(customer_id=interval.data.prev.year$customer_id), length)
        # Filter these aggregated previous customers for the defined conditions (at least X times bought something)
        loyal.customer.ids.prev.year= subset(customers.aggregated.prev.year, subsetRebuyingCustomers(customers.aggregated.prev.year$x))$customer_id
        # Define the 'rebuying customer' set by checking those who can be currently considered loyal and were also loyal last year
        rebuying.customers = subset(loyal.customers, loyal.customer.ids %in% loyal.customer.ids.prev.year)
        rebuying.customer.ids = rebuying.customers$customer_id
        
        rebuy.percent = round(length(rebuying.customer.ids) / length(loyal.customer.ids) * 100, 1)
        
        purchase.per.customer.mean = round(mean(rebuying.customers$x), 2)
        purchase.per.customer.median = median(rebuying.customers$x)
        
        cases = length(rebuying.customer.ids)
        
        rebuying.customer.purchases = subset(interval.data, interval.data$customer_id %in% rebuying.customer.ids)
        
        order.value.mean = round(mean(rebuying.customer.purchases$totalprice))
        order.value.median = round(median(rebuying.customer.purchases$totalprice))
        
        total.aggregated = aggregate(rebuying.customer.purchases$totalprice, list(customer_id=rebuying.customer.purchases$customer_id), sum)
        
        order.by.customer.mean = round(mean(total.aggregated$x), 2)
        order.by.customer.median = round(median(total.aggregated$x), 2)
    
        items.aggregated = with(rebuying.customer.purchases, aggregate(item_count, list(customer_id=customer_id), sum))
        
        items.per.customer = round(mean(items.aggregated$x),2)
        items.per.customer.median = median(items.aggregated$x)
    
        items.per.order = round(mean(rebuying.customer.purchases$item_count),2)
        items.per.order.median = median(rebuying.customer.purchases$item_count)
        return (c(purchase.per.customer.mean,
                  purchase.per.customer.median,
                  cases,
                  order.value.mean,
                  order.value.median,
                  order.by.customer.mean,
                  order.by.customer.median,
                  items.per.order,
                  items.per.order.median,
                  items.per.customer,
                  items.per.customer.median,
                  rebuy.percent))
    } else {
        return (rep(NA, 12))
    }
}
rebuying.customers = cbind(format(date.limits$from, '%Y-%m'), format(date.limits$to, '%Y-%m'), t(apply(date.limits, 1, function (x) { getRebuyerData(as.Date(x[1]), as.Date(x[2]), temp) })))
colnames(rebuying.customers) = c('Időszak kezdete', 'Időszak vége', 'Vásárlások átl. száma', 
                                 'Vásárlások median száma', 'Előfordulás', 
                                 'Átl. bevétel rendelésenként', 'Median bevétel rendelésenként', 
                                 'Átl. bevétel ügyfelenként', 'Median bevétel ügyfelenként',
                                 'Átl. tétel rendelésenként', 'Median tétel rendelésenként',
                                 'Átl. tétel ügyfelenként', 'Median tétel ügyfelenként', 'Újravásárlók %')
rebuying.customers

getNewCustomerData = function (from, to, temp) {
    # Filter for the current from-to date interval (12 months)
    interval.data = subset(temp, temp$date >= from & temp$date <= to)
    # Aggregate for amount of POs
    customers.aggregated = aggregate(interval.data$bill_id, list(customer_id=interval.data$customer_id), length)
    # Filter these aggregated customers for the defined conditions (at least X times bought something)
    new.customers = subset(customers.aggregated, customers.aggregated$x == 1)
    new.customer.ids = new.customers$customer_id
    
    purchase.per.customer.mean = round(mean(new.customers$x), 2)
    purchase.per.customer.median = median(new.customers$x)
    
    cases = length(new.customer.ids)
    
    new.customer.purchases = subset(interval.data, interval.data$customer_id %in% new.customer.ids)
    
    order.value.mean = round(mean(new.customer.purchases$totalprice))
    order.value.median = round(median(new.customer.purchases$totalprice))
    
    total.aggregated = aggregate(new.customer.purchases$totalprice, list(customer_id=new.customer.purchases$customer_id), sum)
    
    order.by.customer.mean = round(mean(total.aggregated$x), 2)
    order.by.customer.median = round(median(total.aggregated$x), 2)

    items.aggregated = with(new.customer.purchases, aggregate(item_count, list(customer_id=customer_id), sum))
    
    items.per.customer = round(mean(items.aggregated$x),2)
    items.per.customer.median = median(items.aggregated$x)

    items.per.order = round(mean(new.customer.purchases$item_count),2)
    items.per.order.median = median(new.customer.purchases$item_count)
    return (c(purchase.per.customer.mean,
              purchase.per.customer.median,
              cases,
              order.value.mean,
              order.value.median,
              order.by.customer.mean,
              order.by.customer.median,
              items.per.order,
              items.per.order.median,
              items.per.customer,
              items.per.customer.median))
}
new.customers = cbind(format(date.limits$from, '%Y-%m'), format(date.limits$to, '%Y-%m'), t(apply(date.limits, 1, function (x) { getNewCustomerData(as.Date(x[1]), as.Date(x[2]), temp) })))
colnames(new.customers) = c('Időszak kezdete', 'Időszak vége', 'Vásárlások átl. száma', 
                                 'Vásárlások median száma', 'Előfordulás', 
                                 'Átl. bevétel rendelésenként', 'Median bevétel rendelésenként', 
                                 'Átl. bevétel ügyfelenként', 'Median bevétel ügyfelenként',
                                 'Átl. tétel rendelésenként', 'Median tétel rendelésenként',
                                 'Átl. tétel ügyfelenként', 'Median tétel ügyfelenként')
new.customers

getAllCustomerData = function (from, to, temp) {
    subtemp = subset(temp, temp$date >= from & temp$date <= to)
    
    customers.aggregated = aggregate(subtemp$bill_id, list(customer_id=subtemp$customer_id), length)
    mean.frequency = round(mean(customers.aggregated$x), 2)
    median.frequency = median(customers.aggregated$x)
    
    cases = length(customers.aggregated)
    
    spend.per.order.mean = round(mean(subtemp$totalprice))
    spend.per.order.median = round(median(subtemp$totalprice))
    
    total.aggregated = aggregate(subtemp$totalprice, list(subtemp$customer_id), sum)
    spend.per.customer.mean = round(mean(total.aggregated$x))
    spend.per.customer.median = round(median(total.aggregated$x))
    
    items.aggregated = aggregate(subtemp$item_count, list(customer_id=subtemp$customer_id), sum)
    items.per.customer = round(mean(items.aggregated$x),2)
    items.per.customer.median = median(items.aggregated$x)
    
    items.per.order = round(mean(subtemp$item_count),2)
    items.per.order.median = median(subtemp$item_count)
    
    return (c(mean.frequency,
              median.frequency,
              cases,
              spend.per.order.mean,
              spend.per.order.median,
              spend.per.customer.mean,
              spend.per.customer.median,
              items.per.order,
              items.per.order.median,
              items.per.customer,
              items.per.customer.median))
}

all.customers = cbind(format(date.limits$from, '%Y-%m'), format(date.limits$to, '%Y-%m'), t(apply(date.limits, 1, function (x) { getAllCustomerData(as.Date(x[1]), as.Date(x[2]), temp) })))
colnames(all.customers) = c('Időszak kezdete', 'Időszak vége', 'Vásárlások átl. száma', 
                                 'Vásárlások median száma', 'Előfordulás',
                                 'Átl. bevétel rendelésenként', 'Median bevétel rendelésenként', 
                                 'Átl. bevétel ügyfelenként', 'Median bevétel ügyfelenként',
                                 'Átl. tétel rendelésenként', 'Median tétel rendelésenként',
                                 'Átl. tétel ügyfelenként', 'Median tétel ügyfelenként')
all.customers

# Idézet a köynvből: 
# "Average order value is a terribly important sign of the health of a business, in combination with other metrics. 
# When all metrics are suffering, but average order value is growing, we surmise that Management is actively trying 
# to squeeze as much out of the business as is humanly possible, to keep the business afloat.
# Mi esetünkben:
#   ami nőtt:
#       - Vásárlások átlagos száma (átlag 6%)
#       - Újravásárlók (szignifikánsan, 13%-kal)
#       - Median bevétel ügyfelenként (6%-kal)
#   ami csökkent:
#       - Átlag és median bevétel rendelésenként (2.7%, 3%)

# Arra tudunk következtetni, hogy a visszatérő vevők száma megnőtt és ez nagyjából egyensúlyozza a csökkenést ami a 
# rendelésenkénti átlagban történt


products = data.frame(ID_TERMEK=numeric(), NEV=character(), DATUM=character())
getProductData = function (year, connection) {
    command = paste("select distinct t.id_termek, lower(t.nev) as \"nev\", sz.datum",
                    " from szamlatetel szt join",
                    " termek t on t.id_termek = szt.id_termek join",
                    " szamla sz on sz.id_szamla = szt.id_szamla")
    return(dbGetQuery(connection, command))
}
products = rbind(products, getProductData(2010, connection_2010_2012))
products = rbind(products, getProductData(2011, connection_2010_2012))
products = rbind(products, getProductData(2012, connection_2010_2012))
products = rbind(products, getProductData(2013, connection_2013))
products = rbind(products, getProductData(2014, connection_2014))
products = rbind(products, getProductData(2015, connection))
colnames(products) = c("id_product", "name", "date")
products$name = as.factor(products$name)
products$date = as.Date(products$date)
products.with.startdate = aggregate(products$date, list(name=products$name), min)
products.with.enddate = aggregate(products$date, list(name=products$name), max)
products = inner_join(products[,c('id','name')], products.with.startdate, by='name')
colnames(products)[3] = 'mindate'
products = inner_join(products, products.with.enddate, by='name')
colnames(products)[4] = 'maxdate'
products = unique(products)


getSaleForProduct = function (year, connection) {
    command = paste("select v.id_vevo, szt.id_termek, sz.datum, (szt.eladar * szt.mennyiseg) as \"mennyiseg\"",
                    " from szamlatetel szt join",
                    " szamla sz on sz.id_szamla = szt.id_szamla join",
                    " vevo v on v.id_vevo = sz.id_vevo",
                    " where extract(year from sz.datum) = ", year)
                    
    return(dbGetQuery(connection, command))
}
prod.sales = data.frame(ID_VEVO=numeric(), ID_TERMEK=character(), DATUM=character(), MENNYISEG=numeric())
prod.sales = rbind(prod.sales, getSaleForProduct(2010, connection_2010_2012))
prod.sales = rbind(prod.sales, getSaleForProduct(2011, connection_2010_2012))
prod.sales = rbind(prod.sales, getSaleForProduct(2012, connection_2010_2012))
prod.sales = rbind(prod.sales, getSaleForProduct(2013, connection_2013))
prod.sales = rbind(prod.sales, getSaleForProduct(2014, connection_2014))
prod.sales = rbind(prod.sales, getSaleForProduct(2015, connection))
colnames(prod.sales) = c("id_customer", "id_product", "date", "totalprice")
prod.sales$date = as.Date(prod.sales$date)

getProductData = function (from, to, prod.sales, products) {
    fagget = inner_join(prod.sales, products, by="id_product")
    sub.prod = subset(prod.sales, prod.sales$date >= from & prod.sales$date <= to)
    head(apply(sub.prod, 1, function (x) { x$maxdate <= to & x$mindate >= from }))
}
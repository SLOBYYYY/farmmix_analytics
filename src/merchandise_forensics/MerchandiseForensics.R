library('RJDBC')
library('dplyr')
dbPassword = "PcL233yW"
drv = JDBC("org.firebirdsql.jdbc.FBDriver",
           "../jdbc-driver/jaybird-full-2.2.7.jar",
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
#starting.month = 1
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

# Idézet a könyvből: 
# "Average order value is a terribly important sign of the health of a business, in combination with other metrics. 
# When all metrics are suffering, but average order value is growing, we surmise that Management is actively trying 
# to squeeze as much out of the business as is humanly possible, to keep the business afloat.


getProductData = function (year, connection) {
    command = paste("select distinct t.id_termek, sz.datum",
                    " from szamlatetel szt join",
                    " termek t on t.id_termek = szt.id_termek join",
                    " szamla sz on sz.id_szamla = szt.id_szamla",
                    " where extract(year from sz.datum) = ", year)
    return(dbGetQuery(connection, command))
}
products = data.frame(ID_TERMEK=numeric(), NEV=character(), DATUM=character())
products = rbind(products, getProductData(2010, connection_2010_2012))
products = rbind(products, getProductData(2011, connection_2010_2012))
products = rbind(products, getProductData(2012, connection_2010_2012))
products = rbind(products, getProductData(2013, connection_2013))
products = rbind(products, getProductData(2014, connection_2014))
products = rbind(products, getProductData(2015, connection))

setupProduct = function (products) {
    colnames(products) = c("id_product", "date")
    products$date = as.Date(products$date)
    aggregated.mindate = aggregate(products$date, list(id_product=products$id_product), min)
    aggregated.maxdate = aggregate(products$date, list(id_product=products$id_product), max)
    joined.products = inner_join(aggregated.mindate, aggregated.maxdate, by='id_product')
    colnames(joined.products)[2] = 'mindate'
    colnames(joined.products)[3] = 'maxdate'
    joined.products = unique(joined.products)
    rownames(joined.products) = NULL
    return (joined.products)
}
products = setupProduct(products)

getSaleForProduct = function (year, connection) {
    command = paste("select szt.id_termek, sz.datum, (szt.eladar * szt.mennyiseg) as \"total\"",
                    " from szamlatetel szt join",
                    " szamla sz on sz.id_szamla = szt.id_szamla join",
                    " vevo v on v.id_vevo = sz.id_vevo",
                    " where extract(year from sz.datum) = ", year)
                    
    return(dbGetQuery(connection, command))
}

prod.sales = data.frame(ID_TERMEK=character(), DATUM=character(), TOTAL=numeric())
prod.sales = rbind(prod.sales, getSaleForProduct(2010, connection_2010_2012))
prod.sales = rbind(prod.sales, getSaleForProduct(2011, connection_2010_2012))
prod.sales = rbind(prod.sales, getSaleForProduct(2012, connection_2010_2012))
prod.sales = rbind(prod.sales, getSaleForProduct(2013, connection_2013))
prod.sales = rbind(prod.sales, getSaleForProduct(2014, connection_2014))
prod.sales = rbind(prod.sales, getSaleForProduct(2015, connection))
colnames(prod.sales) = c("id_product", "date", "totalprice")
prod.sales$date = as.Date(prod.sales$date)

getProductLifeData = function (from, to, prod.sales, products) {
    prod.sales.combined = inner_join(prod.sales, products, by="id_product")
    prod.sales.combined = subset(prod.sales.combined, prod.sales.combined$date >= from & prod.sales.combined$date <= to)
    prod.sales.combined$new = with(prod.sales.combined, mindate >= from & mindate <= to)
    return (prod.sales.combined)
}

getNewProductSummary = function (date.limits, prod.sales, products) {
    result = data.frame(from=character(), to=character(), new.products=numeric(), median.sale=numeric(), mean.sale=numeric(), order.by.product.median=numeric(), order.by.product.mean=numeric())
    for (i in c(1:dim(date.limits)[1])) {
        extended.prod.sales = getProductLifeData(date.limits[i,1], date.limits[i,2], prod.sales, products)
        new.products = subset(extended.prod.sales, extended.prod.sales$new)
        
        new.product.count = 0
        median.sale = 0
        mean.sale = 0
        order.value.by.product.median = NA
        order.value.by.product.mean = NA
        
        if (dim(new.products)[1] > 0) {
            new.product.count = length(unique(new.products$id_product))
            
            median.sale = round(median(new.products$totalprice))
            mean.sale = round(mean(new.products$totalprice))
            
            order.value.by.product.median = median(aggregate(new.products$totalprice, list(id_product=new.products$id_product), sum)$x)
            order.value.by.product.mean = mean(aggregate(new.products$totalprice, list(id_product=new.products$id_product), sum)$x)
        
        }
        result = rbind(result, data.frame(from=date.limits[i,1], to=date.limits[i,2], new.products=new.product.count, median.sale=median.sale, mean.sale=mean.sale, order.value.by.product.median=order.value.by.product.median, order.value.by.product.mean=order.value.by.product.mean))
    }
    return (result)
}

print(getNewProductSummary(date.limits, prod.sales, products))

getExistingProductSummary = function (date.limits, prod.sales, products) {
    result = data.frame(from=character(), to=character(), existing.products=numeric(), median.sale=numeric(), mean.sale=numeric(), order.by.product.median=numeric(), order.by.product.mean=numeric())
    for (i in c(1:dim(date.limits)[1])) {
        extended.prod.sales = getProductLifeData(date.limits[i,1], date.limits[i,2], prod.sales, products)
        existing.products = subset(extended.prod.sales, !extended.prod.sales$new)
        
        existing.product.count = 0
        median.sale = 0
        mean.sale = 0
        order.value.by.product.median = NA
        order.value.by.product.mean = NA
        
        if (dim(existing.products)[1] > 0) {
            existing.product.count = length(unique(existing.products$id_product))
            
            median.sale = round(median(existing.products$totalprice))
            mean.sale = round(mean(existing.products$totalprice))
            
            order.value.by.product.median = median(aggregate(existing.products$totalprice, list(id_product=existing.products$id_product), sum)$x)
            order.value.by.product.mean = mean(aggregate(existing.products$totalprice, list(id_product=existing.products$id_product), sum)$x)
        }
        result = rbind(result, data.frame(from=date.limits[i,1], to=date.limits[i,2], existing.products=existing.product.count, median.sale=median.sale, mean.sale=mean.sale, order.value.by.product.median=order.value.by.product.median, order.value.by.product.mean=order.value.by.product.mean))
    }
    return (result)
}

print(getExistingProductSummary(date.limits, prod.sales, products))

# Conclusion:
# Although numerous new products were introduced in the last segment (436) the median and mean sale of them is clearly declining 
# by a huge margin (median: 17850 -> 12376, mean: 135606 -> 93063, more than 30% drop!!!)
# Idézet a könyvből:
# New item productivity is failing
# It's new items that are holding this business back. One of three things must be happening.
# 1. The rate that this company creates new "winners" is in decline.
# 2. This company is not investing in new merchandise.
# 3. This company is not investing in new merchandise, and the items this company chooses to invest in do not achieve winning status.

getGroupSale = function (year, connection) {
    command = paste("select cs.id_csoport, cs.nev, sz.datum, szt.id_termek, szt.eladar * szt.mennyiseg as \"total\"",
                    " from szamlatetel szt join",
                    " termek t on t.id_termek = szt.id_termek join",
                    " csoport cs on cs.id_csoport = t.id_csoport join",
                    " szamla sz on sz.id_szamla = szt.id_szamla",
                    " where extract(year from sz.datum) = ", year)
    return(dbGetQuery(connection, command))
}

group.sales = data.frame(ID_CSOPORT=numeric(), NEV=character(), DATUM=character(), ID_TERMEK=numeric(), TOTAL=numeric())
group.sales = rbind(group.sales, getGroupSale(2010, connection_2010_2012))
group.sales = rbind(group.sales, getGroupSale(2011, connection_2010_2012))
group.sales = rbind(group.sales, getGroupSale(2012, connection_2010_2012))
group.sales = rbind(group.sales, getGroupSale(2013, connection_2013))
group.sales = rbind(group.sales, getGroupSale(2014, connection_2014))
group.sales = rbind(group.sales, getGroupSale(2015, connection))
colnames(group.sales) = c("id_group", "name", "date", "id_product", "totalprice")


getGroupSummary = function (date.limits, group.sales, products=NULL, filter.by=NULL, FUN) {
    if (!is.null(filter.by) & is.null(products)) {
        stop("Product data with min and max date has to be passed if filter.by is set")
    }
    if (!is.null(filter.by) && filter.by != "new" && filter.by != "existing") {
        stop("filter.by can be null, 'new' or 'existing'")
    }
    column.count = dim(date.limits)[1]
    colnames = NULL
    for (date.index in c(1:column.count)) {
        colname = paste(date.limits[date.index,1], "-", date.limits[date.index,2], sep="")
        colnames = c(colnames, colname)
    }
    rownames = sort(unique(group.sales$name))
    row.count = length(unique(group.sales$name))
    
    result = matrix(nrow=row.count, ncol=column.count + 1, byrow = T)
    colnames(result) = c("Name", colnames)
    result[,1] = rownames
        
    for (date.limit.index in c(1:dim(date.limits)[1])) {
        # aggregate orders results based on the first column (name in this case)
        # make sure it is aligned with the first column that is currently in the result matrix
        from = date.limits[date.limit.index, 1]
        to = date.limits[date.limit.index, 2]
        
        group.sales.filtered = NULL
        if (!is.null(filter.by)) {
            group.sales.filtered = getProductLifeData(date.limits[date.limit.index, 1],
                                                      date.limits[date.limit.index, 2],
                                                      group.sales,
                                                      products)
            group.sales.filtered = with(group.sales.filtered, subset(group.sales.filtered, date >= from & date <= to))
            if (filter.by == "new") {
                group.sales.filtered = subset(group.sales.filtered, group.sales.filtered$new)
            } else if (filter.by == "existing") {
                group.sales.filtered = subset(group.sales.filtered, !group.sales.filtered$new)
            }
        } else {
            group.sales.filtered = with(group.sales, subset(group.sales, date >= from & date <= to))
        }
        if (nrow(group.sales.filtered) > 0) {
            groups.filtered = with(group.sales.filtered, aggregate(totalprice, list(name=name), FUN))
            filtered.group.indices = which(rownames %in% groups.filtered$name)
            result[filtered.group.indices, date.limit.index + 1] = round(groups.filtered$x)
        }
    }
    
    for (column in c(2:(dim(result)[2]-1))) {
        condition = is.na(result[, column+1])
        divider = as.numeric(ifelse(is.na(result[, column+1]), rep(1, nrow(result)), result[,column+1]))
        target = as.numeric(ifelse(is.na(result[, column]), rep(1, nrow(result)), result[, column]))
        values = ifelse(divider == 1 & target != 1, 100, (target / divider) * 100)
        result[, column] = paste(result[, column],
                                 " (",
                                 round(values, 2),
                                 "%)",
                                 sep="")
    }
    return (result)
}

print(getGroupSummary(date.limits, group.sales, products, FUN=sum))
print(getGroupSummary(date.limits, group.sales, products, "new", FUN=sum))
print(getGroupSummary(date.limits, group.sales, products, "existing", FUN=sum))
print(getGroupSummary(date.limits, group.sales, products, FUN=mean))
print(getGroupSummary(date.limits, group.sales, products, "new", FUN=mean))
print(getGroupSummary(date.limits, group.sales, products, "existing", FUN=mean))
print(getGroupSummary(date.limits, group.sales, products, FUN=median))
print(getGroupSummary(date.limits, group.sales, products, "new", FUN=median))
print(getGroupSummary(date.limits, group.sales, products, "existing", FUN=median))
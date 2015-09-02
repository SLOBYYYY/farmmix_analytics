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

plotTopXProductsForAgent = function (dbConnection, topX=NULL, provider=NULL, product.group=NULL, agent.data=NULL, plot=F) {
    providerCommand = ""
    productGroupCommand = ""
    agentGroupCommand = ""
    if (!is.null(provider)) {
        switch(provider,
               farmmix = { providerCommand = "and forgalmazo.nev like 'FARMMIX KFT'" },
               alternativ = { providerCommand = "and forgalmazo.nev like 'FARMMIX KFT ALTERNAT%'" },
               notfarmmix = { providerCommand = "and forgalmazo.nev not like '%FARMMIX%'" }
        )
    }
    if (!is.null(product.group)) {
        switch(product.group,
               vetomag = { productGroupCommand = "and csoport.nev like 'VET_MAG'" },
               novenyvedoszer = { productGroupCommand = "and csoport.nev like 'N_V_NYV_D%'" },
               mutragya = { productGroupCommand = "and csoport.nev like 'M_TR_GYA'" }
        )
    }
    if (!is.null(agent.data)) {
        agentGroupCommand = paste("and szamla.id_uzletkoto =",agent.data[1])
    }
    command = paste("select", 
                    "termek.nev, szamlatetel.eladar, szamlatetel.mennyiseg, szamlatetel.eladar * szamlatetel.mennyiseg as \"EladarSum\", szamla.id_uzletkoto",
                    "from szamlatetel join",
                    "szamla on szamla.id_szamla = szamlatetel.id_szamla join",
                    "termek on termek.id_termek = szamlatetel.id_termek join",
                    "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo join",
                    "csoport on csoport.id_csoport = termek.id_csoport",
                    "where 1=1",
                    providerCommand,
                    productGroupCommand,
                    agentGroupCommand
                    )
    sold.items = dbGetQuery(dbConnection, command)
    if (nrow(sold.items) == 0) {
        print(paste("Nincs eladás ",
                    ifelse(is.null(product.group), "az összes csoportban ", paste("a(z) ", product.group, " csoportban",sep="")),
                    ifelse(is.null(provider), "az összes szolgáltatóval", paste(" a(z) ", provider, " szolgáltatóval",sep="")),
                    sep=""))
        return(topXResult(NA,NA))
    }
                                 
    colnames(sold.items) = c('name', 'price', 'count', 'total', 'id_uzletkoto')
    sold.items$name = factor(sold.items$name, ordered=T)
    
    # We'll do the aggregation by hand because we need median, sd, etc..
    sold.items.aggregated = aggregate(sold.items$total, list(sold.items$name), sum)
    colnames(sold.items.aggregated) = c('name', 'total')
    sold.items.aggregated$total = round(sold.items.aggregated$total, 0)
    for (i in seq(along=sold.items.aggregated$name)) {
        tetel.sales = sold.items[which(
            sold.items$name == sold.items.aggregated[i,'name']),]
        sold.items.aggregated[i,'count'] = sum(tetel.sales$count)
        tetel.mean = mean(tetel.sales$price)
        sold.items.aggregated[i,'mean'] = round(tetel.mean,0)
        sold.items.aggregated[i,'median'] = round(median(tetel.sales$price),0)
        tetel.sd = sd(tetel.sales$price)
        sold.items.aggregated[i,'sd'] = round(tetel.sd,2)
        sold.items.aggregated[i,'diffratio'] = paste(round(tetel.sd / tetel.mean * 100,2),"%")
        tetel.stats = boxplot.stats(tetel.sales$price, coef=2)
        sold.items.aggregated[i,'outliers'] = length(tetel.stats$out)
    }
    
    topx.sold.items.aggregated = arrange(sold.items.aggregated,-total)[1:topX,]
    total.sold.items = sum(sold.items$total)
    topx.is.percent.of.total = round(sum(topx.sold.items.aggregated$total, na.rm = T) / total.sold.items * 100, 2)
    print(paste("A top ",
                topX,
                ifelse(is.null(product.group), " összes termék", paste(" termék a megadott típusból(", product.group, ")", sep="")),
                ifelse(is.null(provider), " az összes szolgáltatóval", paste(" a megadott szolgáltatóval(", provider, ") ", sep="")),
                ifelse(is.null(agent.data), " az összes üzletkötővel a bevétel ", paste(" a megadott üzletkötővel(", agent.data[2], ") a bevétel ", sep="")),
                topx.is.percent.of.total,
                '%-át hozza',
                sep=""))
    #topx.sold.items.aggregated$totalformatted = ft.format(topx.sold.items.aggregated$total)
    colnames(topx.sold.items.aggregated) = c("Név", "Total", "Mennyiség", "Átlag", "Median", "Eloszlás", "Eloszlás/Átlag%", "Kívülálló")
    
    if (plot) {
        ggplot(data = topx.sold.items.aggregated,
               aes(x=reorder(name,-total),
                   y=total,
                   fill=reorder(name,-total),
                   label=ft.format(total, "million"))) + 
            geom_bar(stat="identity") +
            geom_text(size=4, vjust=-1) +
            ylab("Eladott mennyiség (Millió Ft)") +
            xlab("Top eladott termék") +
            scale_y_continuous(labels=plotYCont) + 
            theme(
                axis.text.x=element_blank()
            ) + 
            guides(
                fill=guide_legend(title="Termékek")
            ) 
    } else {
        return(topXResult(topx.is.percent.of.total, 
                   topx.sold.items.aggregated))
    }
}



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

getAgentsWithCustomerData = function (year, dbConnection) {
    command = paste("select u.nev as \"UZLETKOTO\", v.id_vevo, v.nev as \"VEVO\", sz.datum",
                    "from szamla sz join",
                    "vevo v on v.id_vevo = sz.id_vevo join",
                    "uzletkoto u on u.id_uzletkoto = sz.id_uzletkoto",
                    "where extract(year from sz.datum) = ", year)
    temp = dbGetQuery(dbConnection, command)
    return (temp)
}
temp = data.frame(UZLETKOTO=character(), VEVO=character(), DATUM=numeric())
temp = rbind(temp, getAgentsWithCustomerData(2010, connection_2010_2012))
temp = rbind(temp, getAgentsWithCustomerData(2011, connection_2010_2012))
temp = rbind(temp, getAgentsWithCustomerData(2012, connection_2010_2012))
temp = rbind(temp, getAgentsWithCustomerData(2013, connection_2013))
temp = rbind(temp, getAgentsWithCustomerData(2014, connection_2014))
temp = rbind(temp, getAgentsWithCustomerData(2015, connection))
colnames(temp) = c("agent", "customer_id", "customer", "date")
temp$agent = tolower(temp$agent)
temp$agent = as.factor(temp$agent)
temp$customer = tolower(temp$customer)
temp$customer = as.factor(temp$customer)
temp$date = as.Date(temp$date)

#agents = temp

customersForAgents = function (agents, date.limits, new.or.inactive=NULL) {
    agents = unique(agents)
    min.dates = aggregate(agents$date, list(customer_id=agents$customer_id, agent=agents$agent), min)
    max.dates = aggregate(agents$date, list(customer_id=agents$customer_id, agent=agents$agent), max)
    agents = unique(agents[,c("agent", "customer_id")])
    agents = agents[order(agents$agent),]
    agents = na.omit(agents)
    agents = merge(agents, min.dates, by=c("agent", "customer_id"))
    colnames(agents)[3] = "min"
    agents = merge(agents, max.dates, by=c("agent", "customer_id"))
    colnames(agents)[4] = "max"
    
    column.count = dim(date.limits)[1]
    colnames = NULL
    for (date.index in c(1:column.count)) {
        colname = paste(date.limits[date.index,1], "-", date.limits[date.index,2], sep="")
        colnames = c(colnames, colname)
    }
    rownames = unique(as.character(agents$agent))
    row.count = length(unique(agents$agent))
    
    result = matrix(nrow=row.count, ncol=column.count + 1, byrow = T)
    colnames(result) = c("Ügynök", colnames)
    result[,1] = rownames
    
    for (date.limit.index in c(1:dim(date.limits)[1])) {
        # aggregate orders results based on the first column (name in this case)
        # make sure it is aligned with the first column that is currently in the result matrix
        from = date.limits[date.limit.index, 1]
        to = date.limits[date.limit.index, 2]
        
        customers.for.period = NULL
        if (is.null(new.or.inactive)) {
            customers.for.period = subset(agents, agents$min < to)
        } else if (new.or.inactive == "new") {
            customers.for.period = subset(agents, agents$min >= from & agents$min < to)
        } else if (new.or.inactive == "inactive") {
            customers.for.period = subset(agents, agents$max < from) 
        } else {
            stop("new.or.inactive can be NULL, 'new' or 'inactive'")
        }
        
        if (nrow(customers.for.period) > 0) {
            agent.data.filtered = with(customers.for.period, aggregate(customer_id, list(agent=agent), length))
            filtered.agent.data.indices = which(rownames %in% agent.data.filtered$agent)
            result[filtered.agent.data.indices, date.limit.index + 1] = round(agent.data.filtered$x)
        }
    }
    return (result)
}
print(customersForAgents(temp, date.limits))
print(customersForAgents(temp, date.limits, 'new'))
print(customersForAgents(temp, date.limits, 'inactive'))

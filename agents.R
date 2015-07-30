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

topXAgent = function (connection) {
    thisEnv = environment()
    if (class(connection) == "JDBCConnection") {
        localConnection = connection
        result = NULL
        aggregate.sale = function (topx = NULL, provider = NULL, product.group = NULL, agent.data = NULL) {
            if (is.null(result)) {
                stop("Use \"load\" to load data first")
            } else {
                # Narrow down variable "result" if provider, product.group or agent.data is set
                if (!is.null(provider)) {
                    # grepl returns logical vectors for pattern mathing
                    switch(provider,
                           farmmix = { result = subset(result, grepl("FARMMIX KFT", provider_name)) },
                           alternativ = { result = subset(result, grepl("FARMMIX KFT ALTERNAT", provider_name)) },
                           notfarmmix = { result = subset(result, !grepl("FARMMIX", provider_name)) }
                    )
                }
                if (!is.null(product.group)) {
                    switch(product.group,
                           vetomag = { result = subset(result, grepl("^VET.MAG$", group_name)) }, 
                           novenyvedoszer = { result = subset(result, grepl("^N.V.NYV.D", group_name)) },
                           mutragya = { result = subset(result, grepl("^M.TR.GYA$", group_name)) }
                    )
                }
                if (!is.null(agent.data) & length(agent.data) == 2) {
                    result = tryCatch( { 
                            subset(result, id_agent == as.numeric(agent.data[1])) 
                        },
                        warning = function (err) {
                            stop("agent.data\'s first value should be convertible to numeric")
                        }
                    )
                }
                # If there are no rows, print it out
                if (nrow(result) == 0) {
                    print(paste("Nincs eladás ",
                                ifelse(is.null(product.group), "az összes csoportban", paste("a(z) ", product.group, " csoportban",sep="")),
                                ifelse(is.null(provider), " az összes szolgáltatóval", paste(" a(z) ", provider, " szolgáltatóval",sep="")),
                                ifelse(is.null(agent.data), " az összes üzletkötővel", paste("'", agent.data[2], "' nevű üzletkötővel", sep="")),
                                sep=""))
                }
                
                # Make all char columns to a factor
                result$product = factor(result$product, ordered=T)
                result$group_name = factor(result$group_name, ordered=T)
                result$provider_name = factor(result$provider_name, ordered=T)
                result$agent_name = factor(result$agent_name, ordered=T)
                
                # We'll do the aggregation by hand because we need median, sd, etc..
                result.aggregated = aggregate(result$totalprice, list(result$product), sum)
                colnames(result.aggregated) = c('product', 'total')
                result.aggregated$total = round(result.aggregated$total, 0)
                topx.result.aggregated = arrange(result.aggregated,-total)
                
                total.sold.items = sum(result$total)
                loop.limit = ifelse(is.numeric(topx), topx, length(topx.result.aggregated$product))
                for (i in c(1:loop.limit)) {
                    temp = result[which(result$product == topx.result.aggregated[i,'product']),]
                    topx.result.aggregated[i,'count'] = sum(temp$amount)
                    result.mean = mean(temp$unitprice)
                    topx.result.aggregated[i,'mean'] = round(result.mean,0)
                    topx.result.aggregated[i,'median'] = round(median(temp$unitprice),0)
                    result.sd = sd(temp$unitprice)
                    if (is.na(result.sd)) {
                        result.sd=0
                    } 
                    topx.result.aggregated[i,'sd'] = round(result.sd,2)
                    topx.result.aggregated[i,'diffratio'] = paste(round(result.sd / result.mean * 100,2),"%")
                    result.stats = boxplot.stats(temp$unitprice, coef=2)
                    topx.result.aggregated[i,'outliers'] = length(result.stats$out)
                    
                    topx.result.aggregated[i,'topx'] = round(sum(topx.result.aggregated[1:i,]$total, na.rm=T) / total.sold.items * 100, 2)
                }
                
                topx.result.aggregated = topx.result.aggregated[1:topx,]
                topx.is.percent.of.total = round(sum(topx.result.aggregated$total, na.rm = T) / total.sold.items * 100, 2)
                
                colnames(topx.result.aggregated) = c("Név", "Total", "Mennyiség", "Átlag", "Median", "Eloszlás", "Eloszlás/Átlag%", "Kívülálló", "Top X%")
                return(topXResult(topx.is.percent.of.total, 
                           topx.result.aggregated))
            }
        }
        me = list (
            thisEnv = thisEnv,
            getEnv = function () {
                return(get("thisEnv", thisEnv))
            },
            load = function () {
                command = paste("select", 
                                "termek.nev, szamlatetel.eladar, szamlatetel.mennyiseg,",
                                "szamlatetel.eladar * szamlatetel.mennyiseg as \"EladarSum\", szamla.id_uzletkoto,",
                                "csoport.nev, forgalmazo.nev, uzletkoto.nev",
                                "from szamlatetel join",
                                "szamla on szamla.id_szamla = szamlatetel.id_szamla join",
                                "termek on termek.id_termek = szamlatetel.id_termek join",
                                "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo join",
                                "csoport on csoport.id_csoport = termek.id_csoport left join",
                                "uzletkoto on uzletkoto.id_uzletkoto = szamla.id_uzletkoto"
                                )
                temp = dbGetQuery(localConnection, command)
                colnames(temp) = c("product", "unitprice", "amount", "totalprice", "id_agent", "group_name", "provider_name", "agent_name")
                assign("result", temp, thisEnv)
                print("Data is loaded into memory")
            },
            report = function (topx = NULL, provider = NULL, product.group = NULL, agent.data = NULL) {
                result = aggregate.sale(topx=topx, provider=provider, product.group=product.group, agent.data=agent.data)
                
                print(paste("A top ",
                            topx,
                            ifelse(is.null(product.group), " összes termék", paste(" termék a megadott típusból(", product.group, ")", sep="")),
                            ifelse(is.null(provider), " az összes szolgáltatóval", paste(" a megadott szolgáltatóval(", provider, ")", sep="")),
                            ifelse(is.null(agent.data), " az összes üzletkötővel a bevétel ", paste(" a megadott üzletkötővel(", agent.data[2], ") a bevétel ", sep="")),
                            result$top.x.percent,
                            '%-át hozza',
                            sep=""))
                return(result)
            },
            plot = function (topx = NULL, provider = NULL, product.group = NULL, agent.data = NULL) {
                result = aggregate.sale(topx=topx, provider=provider, product.group=product.group, agent.data=agent.data)
                topx.result.aggregated = result$result
                #topx.sold.items.aggregated$totalformatted = ft.format(topx.sold.items.aggregated$total)
                ggplot(data = topx.result.aggregated,
                       aes(x=reorder(Név,-Total),
                           y=Total,
                           fill=reorder(Név,-Total),
                           label=ft.format(Total, "million"))) + 
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
            }
        )
        assign('this', me, envir = thisEnv)
        class(me) = append(class(me), "TopXAgent")
        return(me)
    }
}
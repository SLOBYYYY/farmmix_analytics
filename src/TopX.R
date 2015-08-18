TopX = function (connection) {
    thisEnv = environment()
    if (class(connection) == "JDBCConnection") {
        localConnection = connection
        result = NULL
        aggregate.sale = function (aggregate.by="product", topx = NULL, provider = NULL, product.group = NULL, agent.data = NULL) {
            # If result is not set, we fail immediately
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
                # We define first what we want to aggregate by. It can be product or customer ATM
                # This has to be done after the filtering because of the result$xxx can be different
                # after the filtering
                aggregate.by.vector = switch(aggregate.by,
                                             product = { result$product },
                                             customer = { result$customer},
                                             {
                                                stop("aggregate.by has incorrect value. Use either 'product' or 'customer'")
                                             })
                # If there are no rows, print it out
                if (nrow(result) == 0) {
                    print(paste("Nincs eladás ",
                                ifelse(is.null(product.group), "az összes csoportban", paste("a(z) ", product.group, " csoportban",sep="")),
                                ifelse(is.null(provider), " az összes szolgáltatóval", paste(" a(z) ", provider, " szolgáltatóval",sep="")),
                                ifelse(is.null(agent.data), " az összes üzletkötővel", paste("'", agent.data[2], "' nevű üzletkötővel", sep="")),
                                sep=""))
                }
                
                # Make all char columns to a factor
                result$customer = factor(result$customer, ordered=T)
                result$product = factor(result$product, ordered=T)
                result$group_name = factor(result$group_name, ordered=T)
                result$provider_name = factor(result$provider_name, ordered=T)
                result$agent_name = factor(result$agent_name, ordered=T)
                
                # We'll do the aggregation by hand because we need median, sd, etc..
                result.aggregated = aggregate(result$totalprice, list(aggregate.by.vector), sum)
                colnames(result.aggregated) = c(aggregate.by, 'total')
                # Round the total, we don't need decimals
                result.aggregated$total = round(result.aggregated$total, 0)
                # We order it in descending order by the total
                topx.result.aggregated = arrange(result.aggregated,-total)
                
                # We will use the sum of the totals to calculate percentages
                total.sold.items = sum(result$total)
                # If topx is set, we only want to calculate those values
                loop.limit = ifelse(is.numeric(topx), topx, length(topx.result.aggregated[,aggregate.by]))
                for (i in c(1:loop.limit)) {
                    # We get all the items (not aggregated) that share the same property held by the current aggregated value
                    # It can be all transactions for a certain product or customer
                    temp = result[which(aggregate.by.vector == topx.result.aggregated[i,aggregate.by]),]
                    topx.result.aggregated[i,'count'] = sum(temp$amount)
                    if (aggregate.by == "product") {
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
                        ratio = round(result.mean / mean(temp$factory_price, na.rm = T) * 100, 2)
                        topx.result.aggregated[i,'factoryratio'] = ratio
                    }
                    
                    topx.result.aggregated[i,'topx'] = round(topx.result.aggregated[i,]$total / total.sold.items * 100, 2)
                    topx.result.aggregated[i,'topxcum'] = round(sum(topx.result.aggregated[1:i,]$total, na.rm=T) / total.sold.items * 100, 2)
                }
                
                topx.result.aggregated = topx.result.aggregated[1:topx,]
                topx.is.percent.of.total = round(sum(topx.result.aggregated$total, na.rm = T) / total.sold.items * 100, 2)
                
                if (aggregate.by == "product") {
                    colnames(topx.result.aggregated) = c("Név", "Total", "Mennyiség", "Átlag", "Median", "Eloszlás", "Eloszlás/Átlag%", "Kívülálló", "Gyári ár %", "Top X%", "Top X% kumulatív")
                } else {
                    colnames(topx.result.aggregated) = c("Név", "Total", "Mennyiség", "Top X%", "Top X% kumulatív")
                }
                return(topXResult(topx.is.percent.of.total, 
                           topx.result.aggregated))
            }
        }
        get.aggregate.by.name = function (aggregate.by) {
            switch(aggregate.by,
                   product = { return('termék') },
                   customer = { return('ügyfél') },
                   {  
                        stop("aggregate.by has incorrect value. Use either 'product' or 'customer'")
                   })
        }
        report = function (aggregate.by="product", topx = NULL, provider = NULL, product.group = NULL, agent.data = NULL) {
            aggregate.by.name = get.aggregate.by.name(aggregate.by)
            result = aggregate.sale(aggregate.by, topx=topx, provider=provider, product.group=product.group, agent.data=agent.data)
            
            print(paste("A top ",
                        topx,
                        ifelse(is.null(product.group), paste(" összes ", aggregate.by.name, sep=""), paste(" ", aggregate.by.name, " a megadott típusból(", product.group, ")", sep="")),
                        ifelse(is.null(provider), " az összes szolgáltatóval", paste(" a megadott szolgáltatóval(", provider, ")", sep="")),
                        ifelse(is.null(agent.data), " az összes üzletkötővel a bevétel ", paste(" a megadott üzletkötővel(", agent.data[2], ") a bevétel ", sep="")),
                        result$top.x.percent,
                        '%-át hozza',
                        sep=""))
            return(result)
        }
        plot = function (aggregate.by="product", topx = NULL, provider = NULL, product.group = NULL, agent.data = NULL) {
            aggregate.by.name = get.aggregate.by.name(aggregate.by)
            result = aggregate.sale(aggregate.by, topx=topx, provider=provider, product.group=product.group, agent.data=agent.data)
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
                xlab(paste("Top eladott", aggregate.by.name)) +
                scale_y_continuous(labels=plotYCont) + 
                theme(
                    axis.text.x=element_blank()
                ) + 
                guides(
                    fill=guide_legend(title=aggregate.by.name)
                ) 
        }
        me = list (
            thisEnv = thisEnv,
            getEnv = function () {
                return(get("thisEnv", thisEnv))
            },
            load = function () {
                command = paste("select vevo.nev, termek.nev, szamlatetel.eladar, szamlatetel.mennyiseg,",
                                "szamlatetel.eladar * szamlatetel.mennyiseg as \"EladarSum\", szamla.id_uzletkoto,",
                                "csoport.nev, forgalmazo.nev, uzletkoto.nev, termek.gyariar",
                                "from szamlatetel join ", 
                                "szamla on szamla.id_szamla = szamlatetel.id_szamla join",
                                "vevo on vevo.id_vevo = szamla.id_vevo join",
                                "termek on termek.id_termek = szamlatetel.id_termek join",
                                "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo join",
                                "csoport on csoport.id_csoport = termek.id_csoport left join",
                                "uzletkoto on uzletkoto.id_uzletkoto = szamla.id_uzletkoto")
                temp = dbGetQuery(localConnection, command)
                colnames(temp) = c("customer", "product", "unitprice", "amount", "totalprice", "id_agent", "group_name", "provider_name", "agent_name", "factory_price")
                assign("result", temp, thisEnv)
                print("Data is loaded into memory")
            },
            reportCustomer = function (topx = NULL, provider = NULL, product.group = NULL, agent.data = NULL) {
                return(report(aggregate.by="customer", topx=topx, provider=provider, product.group=product.group, agent.data=agent.data))
            },
            reportProduct = function (topx = NULL, provider = NULL, product.group = NULL, agent.data = NULL) {
                return(report(aggregate.by="product", topx=topx, provider=provider, product.group=product.group, agent.data=agent.data))
            },
            plotCustomer = function (topx = NULL, provider = NULL, product.group = NULL, agent.data = NULL) {
                plot(aggregate.by="customer", topx=topx, provider=provider, product.group=product.group, agent.data=agent.data)
            },
            plotProduct = function (topx = NULL, provider = NULL, product.group = NULL, agent.data = NULL) {
                plot(aggregate.by="product", topx=topx, provider=provider, product.group=product.group, agent.data=agent.data)
            }
        )
        assign('this', me, envir = thisEnv)
        class(me) = append(class(me), "TopX")
        return(me)
    }
}
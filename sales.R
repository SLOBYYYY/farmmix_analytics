plotTopXProducts = function (dbConnection, topX=NULL, provider=NULL, product.group=NULL, plot=F) {
    if (is.numeric(topX)) {
        topXCommand = ""
        providerCommand = ""
        productGroupCommand = ""
        if (!is.null(topX)) {
            topXCommand = paste("first", topX)
        }
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
        command = paste("select", 
                        topXCommand,
                        "termek.nev, sum(szamlatetel.eladar * szamlatetel.mennyiseg) as \"EladarSum\"",
                        "from szamlatetel join",
                        "termek on termek.id_termek = szamlatetel.id_termek join",
                        "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo join",
                        "csoport on csoport.id_csoport = termek.id_csoport",
                        "where 1=1",
                        providerCommand,
                        productGroupCommand,
                        "group by termek.nev",
                        "order by \"EladarSum\" desc"
                        )
        topXSoldTetel = dbGetQuery(dbConnection, command)
                                     
        colnames(topXSoldTetel) = c('name', 'sale')
        topXSoldTetel$name = factor(topXSoldTetel$name, ordered=T)
        
        if (plot) {
            ggplot(data = topXSoldTetel,
                   aes(x=reorder(name,-sale),
                       y=sale,
                       fill=reorder(name,-sale),
                       label=ft.format(sale, "million"))) + 
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
            return(topXSoldTetel)
        }
    } else {
        stop("The topX variable should be a number")
    }
}
#plotTopXProducts(connection, 10, provider="farmmix", product.group="novenyvedoszer", plot=F)
    

plotTopXFarmmixProducts = function (dbConnection, topX, plot=F) {
    if (is.numeric(topX)) {
        topXSoldTetel = dbGetQuery(dbConnection,
                                     paste("select first", topX, "termek.nev, sum(szamlatetel.eladar * szamlatetel.mennyiseg) as \"EladarSum\"",
                                           "from szamlatetel join ", 
                                           "termek on termek.id_termek = szamlatetel.id_termek join",
                                           "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo",
                                           "where forgalmazo.nev like '%FARMMIX%'",
                                           "group by termek.nev",
                                           "order by \"EladarSum\" desc"))
        colnames(topXSoldTetel) = c('name', 'sale')
        topXSoldTetel$name = factor(topXSoldTetel$name, ordered=T)
        
        if (plot) {
            ggplot(data = topXSoldTetel, aes(x=reorder(name,-sale),
                                             y=sale,
                                             fill=reorder(name,-sale),
                                             label=ft.format(sale, "million"))) + 
                geom_bar(stat="identity") +
                geom_text(size=4, vjust=-1) +
                ylab("Eladott mennyiség (Millió Ft)") +
                xlab("Top eladott Farmmixes termék") +
                scale_y_continuous(labels=plotYCont) + 
                theme(
                    axis.text.x=element_blank()
                ) + 
                guides(
                    fill=guide_legend(title="Termékek")
                ) 
        } else {
            return(topXSoldTetel)
        }
    } else {
        stop("The topX variable should be a number")
    }
}

topXIsWhatPercentOfAllProducts = function (dbConnection, topX) {
    if (is.numeric(topX)) {
        topXSoldTetel = dbGetQuery(dbConnection,
                                     paste("select first", topX, "termek.nev, sum(szamlatetel.eladar * szamlatetel.mennyiseg) as \"EladarSum\"",
                                           "from szamlatetel join ", 
                                           "termek on termek.id_termek = szamlatetel.id_termek join",
                                           "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo",
                                           "group by termek.nev",
                                           "order by \"EladarSum\" desc"))
        sumSoldTetel = dbGetQuery(dbConnection, "select sum(szamlatetel.eladar * szamlatetel.mennyiseg) from szamlatetel")
        print(paste("A top", topX, "termék a bevétel", round(sum(topXSoldTetel[,2], na.rm=T) / sumSoldTetel[1,1] * 100, 2), '%-át hozzák'))
        print("Termékek: ")
        for (i in 1:topX) {
            print(paste(topXSoldTetel[i,1], " (", round(topXSoldTetel[i,2] / sumSoldTetel[1,1] * 100, 2), "%)", sep=""))
        }
    } else {
        stop("The topX variable should be a number")
    }
}

topXFarmmixIsWhatPercentOfAllProducts = function (dbConnection, topX) {
    if (is.numeric(topX)) {
        topXSoldTetel = dbGetQuery(dbConnection,
                                     paste("select first", topX, "termek.nev, sum(szamlatetel.eladar * szamlatetel.mennyiseg) as \"EladarSum\"",
                                           "from szamlatetel join ", 
                                           "termek on termek.id_termek = szamlatetel.id_termek join",
                                           "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo",
                                           "where forgalmazo.nev like '%FARMMIX%'",
                                           "group by termek.nev",
                                           "order by \"EladarSum\" desc"))
        sumSoldTetel = dbGetQuery(dbConnection, "select sum(szamlatetel.eladar * szamlatetel.mennyiseg) from szamlatetel")
        print(paste("A top", topX, "Farmmix termék a bevétel", round(sum(topXSoldTetel[,2], na.rm=T) / sumSoldTetel[1,1] * 100, 2), '%-át hozzák'))
        print("Farmmix termékek: ")
        for (i in 1:topX) {
            print(paste(topXSoldTetel[i,1], " (", round(topXSoldTetel[i,2] / sumSoldTetel[1,1] * 100, 2), "%)", sep=""))
        }
    } else {
        stop("The topX variable should be a number")
    }
}

worstXProducts = function (dbConnection, topX) {
    if (is.numeric(topX)) {
        worstXProduct = dbGetQuery(dbConnection,
                                   paste("select first", topX, "termek.nev, sum(szamlatetel.eladar * szamlatetel.mennyiseg) as \"EladarSum\"",
                                         "from szamlatetel join ", 
                                         "termek on termek.id_termek = szamlatetel.id_termek",
                                         "group by termek.nev",
                                         "having sum(szamlatetel.eladar * szamlatetel.mennyiseg) > 0",
                                         "order by \"EladarSum\""))
        sumSoldTetel = dbGetQuery(dbConnection, "select sum(szamlatetel.eladar * szamlatetel.mennyiseg) from szamlatetel")
        worstXProductSum = sum(worstXProduct[,2], na.rm=T)
        print(paste("A legrosszabb", topX, "termék a bevétel", round(worstXProductSum / sumSoldTetel[1,1] * 100, 2), "%-át (", worstXProductSum ,"Ft) hozzák"))
        print("A legrosszabb termékek: ")
        for (i in 1:topX) {
            print(paste(worstXProduct[i,1],
                        " (",
                        worstXProduct[i,2],
                        " - ",
                        round(worstXProduct[i,2] / sumSoldTetel[1,1] * 100, 2),
                        "%)",
                        sep=""))
        }
    } else {
        stop("The topX variable should be a number")
    }
}

worstXFarmmixProducts = function (dbConnection, topX) {
    if (is.numeric(topX)) {
        worstXProduct = dbGetQuery(dbConnection,
                                   paste("select first", topX, "termek.nev, sum(szamlatetel.eladar * szamlatetel.mennyiseg) as \"EladarSum\"",
                                         "from szamlatetel join ", 
                                         "termek on termek.id_termek = szamlatetel.id_termek join",
                                         "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo",
                                         "where forgalmazo.nev like '%FARMMIX%'",
                                         "group by termek.nev",
                                         "having sum(szamlatetel.eladar * szamlatetel.mennyiseg) > 0",
                                         "order by \"EladarSum\""))
        sumSoldTetel = dbGetQuery(dbConnection, "select sum(szamlatetel.eladar * szamlatetel.mennyiseg) from szamlatetel")
        worstXProductSum = sum(worstXProduct[,2], na.rm=T)
        print(paste("A legrosszabb", topX, "Farmmix termék a bevétel", round(worstXProductSum / sumSoldTetel[1,1] * 100, 2), "%-át (", worstXProductSum ,"Ft) hozzák"))
        print("A legrosszabb Farmmix termékek: ")
        for (i in 1:topX) {
            print(paste(worstXProduct[i,1],
                        " (",
                        worstXProduct[i,2],
                        " - ",
                        round(worstXProduct[i,2] / sumSoldTetel[1,1] * 100, 2),
                        "%)",
                        sep=""))
        }
    } else {
        stop("The topX variable should be a number")
    }
}

plotFarmmixProductsRatioForYears = function (connections, timeDivision="year") {
    if(is.list(connections)) {
        products = data.frame(date=numeric(),
                              type=character(),
                              value=numeric(),
                              row.names = NULL,
                              stringsAsFactors = F)
        baseCommand = paste("sum(szamlatetel.eladar * szamlatetel.mennyiseg)",
                       "from szamlatetel join ", 
                       "szamla on szamla.id_szamla = szamlatetel.id_szamla join",
                       "termek on termek.id_termek = szamlatetel.id_termek join",
                       "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo",
                       "where szamlatetel.mennyiseg >=0")
        if (timeDivision == "year") {
            baseCommand = paste("select extract(year from szamla.datum) as \"Datum\",", baseCommand)
        } else {
            baseCommand = paste("select extract(year from szamla.datum) || '-' || extract(month from szamla.datum) || '-01' as \"Datum\",", baseCommand)
        }
        for(connection in connections) {
            fmxProducts = dbGetQuery(connection$Connection,
                                 paste(baseCommand,
                                       "and forgalmazo.nev like '%FARMMIX%'",
                                       "group by \"Datum\"",
                                       "order by \"Datum\"")
            )
            names(fmxProducts) = c("date", "value")
            nonFmxProducts = dbGetQuery(connection$Connection,
                                 paste(baseCommand,
                                       "and forgalmazo.nev not like '%FARMMIX%'",
                                       "group by \"Datum\"",
                                       "order by \"Datum\"")
            )
            names(nonFmxProducts) = c("date", "value")
            fmxRowCount = nrow(fmxProducts)
            nonFmxRowCount = nrow(nonFmxProducts)
            products = rbind(products,
                             data.frame(date=fmxProducts[,1],
                                        type="Fmx",
                                        value=fmxProducts[,2],
                                        row.names = NULL,
                                        stringsAsFactors = F)
                             )
            products = rbind(products,
                             data.frame(date=nonFmxProducts[,1],
                                        type="Non-Fmx",
                                        value=nonFmxProducts[,2],
                                        row.names=NULL,
                                        stringsAsFactors = F)
                             )
            
            if (timeDivision == "month") {
                fmxProducts$date = as.Date(fmxProducts$date)
                fmxProducts = arrange(fmxProducts, date)
                writeLines(paste("Hónap: ",
                                 format(fmxProducts$date, "%y-%m"),
                                 " | Fmx eladások: ",
                                 ft.format(fmxProducts[,2], rounding="million"),
                                 " | Nem-Fmx eladások: ",
                                 ft.format(nonFmxProducts[,2], rounding="million"),
                                 " | Arány(Fmx/Nem-Fmx): ",
                                 round(fmxProducts$value / nonFmxProducts$value * 100),
                                 "% | Arány(Fmx/Összes): ",
                                 round(fmxProducts$value / (nonFmxProducts$value + fmxProducts$value) * 100), 
                                 "%"
                                 , sep=""))
            } else {
                writeLines(paste("Év: ",
                                 fmxProducts$date,
                                 " | Fmx eladások: ",
                                 ft.format(fmxProducts[,2], rounding="million"),
                                 " | Nem-Fmx eladások: ",
                                 ft.format(nonFmxProducts[,2], rounding="million"),
                                 " | Arány(Fmx/Nem-Fmx): ",
                                 round(fmxProducts$value / nonFmxProducts$value * 100),
                                 "% | Arány(Fmx/Összes): ",
                                 round(fmxProducts$value / (nonFmxProducts$value + fmxProducts$value) * 100), 
                                 '%'
                                 , sep=""))
            }
        }
        
        products$type = as.factor(products$type)
        products = arrange(products, date)
        if (timeDivision == "month") {
            products$date = as.Date(products$date)
        }
        # Fontos, hogy geom_area esetén ne legyen túl sűrű az X tengely (year).
        # Ha napira kérdeztem le, akkor nagyon szőrös lett az egész
        # Másik ami fontos, hogy ha tételesen kérdeztem le (nem group by-jal) akkor
        # szarul jelenítette meg. Nem volt kitöltött terület, csak vonalak
        plot = ggplot(products, aes(x=date, y=value, fill=type, colour=type)) +
            geom_area(position="stack") +
            ylab("Eladás (Millió Ft)") +
            xlab("Dátum") +
            scale_y_continuous(labels=plotYCont)
        if (timeDivision == "month") {
            plot = plot + scale_x_date(labels=date_format("%y-%m"))
        }
        plot(plot)
    }
}

plotTermekCsoportForSoldTermekek = function (dbConnection) {
    termekcsoport.freq = dbGetQuery(dbConnection, 
               paste("select csoport.nev, count(szamlatetel.id_termek) ",
               "from szamlatetel ",
               "join termek on szamlatetel.id_termek = termek.id_termek ",
               "join csoport on csoport.id_csoport = termek.id_csoport",
               "group by csoport.nev"))
    termekcsoport.filtered = filterOnePercentRows(termekcsoport.freq)
    drawPieChart(termekcsoport.filtered, "Termékcsoportok megoszlása az eladott termékek között")
}

plotYearlySales = function (dbConnection, year, time_divide="weeks") {
    sales.by.date = dbGetQuery(dbConnection,
               paste("select datum, sum(eladar * mennyiseg) ",
                     "from SZAMLATETEL join", 
                     "SZAMLA on SZAMLA.ID_SZAMLA = SZAMLATETEL.ID_SZAMLA",
                     "where extract(year from datum) =", year,
                     "group by datum order by datum")
               )
    names(sales.by.date) = c("datum","sum")
    sales.by.date$datum = as.Date(sales.by.date$datum, '%Y-%m-%d')

    # Default week format
    label.format = "%m-%d"
    if (time_divide == "month") {
        label.format = '%b'
    }
    ggplot(data = sales.by.date, aes(datum, sum)) + 
        geom_line() +
        stat_smooth(method="loess") +
        ylab("Eladás (Millió Ft)") +
        xlab("Dátum") +
        scale_y_continuous(labels=plotYCont) +
        scale_x_date(breaks=date_breaks(time_divide), labels=date_format(label.format))
}

printYearlySales = function (dbConnection, year, to="end") {
    command = paste("select sum(eladar * mennyiseg) ",
                                    "from szamlatetel join",
                                    "szamla on szamla.id_szamla = szamlatetel.id_szamla",
                                    "where extract(year from szamla.datum) = ", year)
    if (to == "today") {
        command = paste(command, " and szamla.datum <= '", year, "-", format(Sys.Date(), "%m-%d"), "'", sep = "")
    }
    yearly.sales = dbGetQuery(dbConnection, command)
    text = paste("Teljes éves bevétel (", year, "):")
    if (to == "today") {
        text = paste("Teljes éves bevétel (", year, "-", format(Sys.Date(), "%m-%d"), "-ig)", sep = "")
    }
    print(paste(text, ft.format(yearly.sales)))
}

printYearlyReturns = function (dbConnection) {
    yearly.returns = dbGetQuery(dbConnection,
                              "select count(id_visszaaru) from visszaaru")
    print(paste("Visszáruk száma:", yearly.returns))
}

drawAreaPlot = function (connections) {
    if(is.list(connections)) {
        year.groups = data.frame(year=as.numeric(), label=as.character(), value=as.numeric(), row.names = NULL)
        for(connection in connections) {
            groups = dbGetQuery(connection$Connection,
                       #paste("select csoport.nev, count(szamlatetel.id_termek) ",
                       paste("select csoport.nev, szamlatetel.id_termek",
                       "from szamlatetel ",
                       "join termek on szamlatetel.id_termek = termek.id_termek ",
                       "join csoport on csoport.id_csoport = termek.id_csoport")
                       #"join csoport on csoport.id_csoport = termek.id_csoport",
                       #"group by csoport.nev")
            )
            year = rep(connection$Year, nrow(groups))
            year.groups = rbind(year.groups, 
                                data.frame(year=year,
                                           label=groups$NEV,
                                           #value=groups$COUNT, row.names=NULL)
                                           value=groups$ID_TERMEK, row.names=NULL)
                                )
        }
        ggplot(year.groups, aes(factor(year), fill=label)) +
            geom_bar()
    } 
}

plotProductSaleForLifetime = function (connections, product.names) {
    if(is.list(connections) && length(connections) > 0 && is.vector(product.names) && length(product.names) > 0) {
        product.sales = data.frame(month=as.character(), product=as.character(), sale=as.numeric(), row.names = NULL)
        # Flatten the names into a comma separated string for the command
        product.names.flat = paste("'", product.names, "'", sep="", collapse=", ")
        for(connection in connections) {
            command = paste("select extract(year from szamla.datum) || '-' || extract(month from szamla.datum) || '-01' as \"Datum\",",
                            " termek.nev, ",
                            " sum(szamlatetel.eladar * szamlatetel.mennyiseg)",
                            " from szamlatetel join ", 
                            " szamla on szamla.id_szamla = szamlatetel.id_szamla join",
                            " termek on termek.id_termek = szamlatetel.id_termek",
                            " where termek.nev in (", product.names.flat, ")",
                            " group by \"Datum\", termek.nev",
                            " order by \"Datum\"",
                            sep="")
            salesByMonth = dbGetQuery(connection$Connection, command)
            product.sales = rbind(product.sales,
                             data.frame(month=salesByMonth[,1],
                                        product=salesByMonth[,2],
                                        sale=salesByMonth[,3],
                                        row.names=NULL,
                                        stringsAsFactors = F)
                             )
        }
        product.sales$month = as.Date(product.sales$month)
        product.sales$product = factor(product.sales$product)
        months = unique(product.sales$month)
        # Impute missing values with 0-s
        #month.occurrances = table(product.sales$month)
        #product.occurrances = table(product.sales$product)
        for (product in seq(along=product.names)) {
            for (month in 1:length(months)) {
                if (nrow(product.sales[product.sales$month == months[month] &
                    product.sales$product == product.names[product],]) == 0) {
                    product.sales = rbind(product.sales,
                                          data.frame(
                                                month=months[month],
                                                product=product.names[product],
                                                sale=0,
                                                row.names=NULL,
                                                stringsAsFactors = F)     
                                          )
                }
            }
        }
        
        product.sales = arrange(product.sales, month)
        
        ggplot(data=product.sales, aes(x=month,y=sale, fill=product, color=product)) +
            geom_line() + 
            ylab("Eladás (Millió Ft)") +
            xlab("Dátum") +
            scale_y_continuous(labels=plotYCont) +
            scale_x_date(breaks=date_breaks("3 month"), labels=date_format("%Y-%m"))
        #return(product.sales)
    }
}
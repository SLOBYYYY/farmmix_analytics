forecastSales = function (connections) {
    if(is.list(connections)) {
        products = data.frame(date=numeric(),
                              value=numeric(),
                              row.names = NULL,
                              stringsAsFactors = F)
        for(connection in connections) {
            command = paste("select extract(year from szamla.datum) || '-' || extract(month from szamla.datum) || '-01' as \"Datum\",",
                                "sum(szamlatetel.eladar * szamlatetel.mennyiseg)",
                                "from szamlatetel join ", 
                                "szamla on szamla.id_szamla = szamlatetel.id_szamla join",
                                "termek on termek.id_termek = szamlatetel.id_termek",
                                "where szamlatetel.mennyiseg >=0",
                                "group by \"Datum\"",
                                "order by \"Datum\"")
            salesByMonth = dbGetQuery(connection$Connection, command)
            products = rbind(products,
                             data.frame(date=salesByMonth[,1],
                                        value=salesByMonth[,2],
                                        row.names=NULL,
                                        stringsAsFactors = F)
                             )
        }
        products$date = as.Date(products$date)
        products = arrange(products, date)
        startDate = min(products$date)
        startYear = format(startDate, "%y")
        startMonth = format(startDate, "%m")
        productsTs = ts(products$value, frequency = 12, start=c(as.numeric(startYear),as.numeric(startMonth)))
        productsTsTrain = window(productsTs, end=13.99)
        productsTsTest = window(productsTs, start=14, end=14.99)
        # Box-Cox transzformáció kipróbálása variancia csökkentésére
        lambda = BoxCox.lambda(productsTsTrain)
        # forecast() alapból kiválasztja a megfelelő előrejelzés típust.
        # Ez esetünkben ETS lesz MNM modellel
        # ETS -> Error,Trend,Seasonal / ExponenTial Smoothing
        fit = forecast(productsTsTrain, h=12, lambda=lambda)
        
        # In-sample pontossag
        accuracy(fit)
        # Out-sample pontossag
        accuracy(fit, productsTsTest)
        # Másokat is kipróbáltam (MAM modell és BoxCox lambda nélküli ETS modellek,
        # mindegyik in és out-sample pontossága rosszabb volt (MAPE-t néztem))
        
        # Megnézzük, hogy milyen pontosan becsülte volna meg a múlt évet
        plot(productsTs, ylab="Eladások", xlab="Idő (év)")
        title("Becslési modell vizsgálata")
        lines(fit$mean, col="blue", lwd=3)
        legend("topleft", col="blue", lwd=3, legend="Becsült")
        # Egész jól megbecsülné, jó lesz ez a modell ezzel a lambdával
        return(forecast(productsTs, h=12, lambda=lambda))
    }
}
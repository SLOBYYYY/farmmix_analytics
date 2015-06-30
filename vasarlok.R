topXBestCustomerForAllProducts = function (dbConnection, topX) {
    if (is.numeric(topX)) {
        topXVevo = dbGetQuery(dbConnection,
                                     paste("select first", topX, "vevo.nev, sum(szamlatetel.eladar * szamlatetel.mennyiseg) as \"EladarSum\"",
                                           "from szamlatetel join ", 
                                           "szamla on szamla.id_szamla = szamlatetel.id_szamla join",
                                           "vevo on vevo.id_vevo = szamla.id_vevo",
                                           "group by vevo.nev",
                                           "order by \"EladarSum\" desc"))
        sumSoldTetel = dbGetQuery(dbConnection, "select sum(szamlatetel.eladar * szamlatetel.mennyiseg) from szamlatetel")
        topXVevoSum = sum(topXVevo[,2], na.rm=T)
        print(paste("A top", topX, "vevő a bevétel", round(topXVevoSum / sumSoldTetel[1,1] * 100, 2), "%-át (", topXVevoSum, "Ft ) hozza"))
        print("Vevők: ")
        for (i in 1:topX) {
            print(paste(topXVevo[i,1], 
                        " (", 
                        topXVevo[i,2],
                        " - ",
                        round(topXVevo[i,2] / sumSoldTetel[1,1] * 100, 2),
                        "%)",
                        sep=""))
        }
    } else {
        stop("The topX variable should be a number")
    }
}

topXBestCustomerForFarmmixProducts = function (dbConnection, topX) {
    if (is.numeric(topX)) {
        topXVevo = dbGetQuery(dbConnection,
                                     paste("select first", topX, "vevo.nev, sum(szamlatetel.eladar * szamlatetel.mennyiseg) as \"EladarSum\"",
                                           "from szamlatetel join ", 
                                           "szamla on szamla.id_szamla = szamlatetel.id_szamla join",
                                           "vevo on vevo.id_vevo = szamla.id_vevo join",
                                           "termek on termek.id_termek = szamlatetel.id_termek join",
                                           "forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo",
                                           "where forgalmazo.nev like '%FARMMIX%'",
                                           "group by vevo.nev",
                                           "order by \"EladarSum\" desc"))
        sumSoldTetel = dbGetQuery(dbConnection, "select sum(szamlatetel.eladar * szamlatetel.mennyiseg) from szamlatetel")
        topXVevoSum = sum(topXVevo[,2], na.rm=T)
        print(paste("A top", topX, " Farmmixes terméket vásárló vevő a bevétel", round(topXVevoSum / sumSoldTetel[1,1] * 100, 2), "%-át (", topXVevoSum, "Ft ) hozza"))
        print("Vevők: ")
        for (i in 1:topX) {
            print(paste(topXVevo[i,1],
                        " (",
                        topXVevo[i,2],
                        " - ",
                        round(topXVevo[i,2] / sumSoldTetel[1,1] * 100, 2),
                        "%)",
                        sep=""))
        }
    } else {
        stop("The topX variable should be a number")
    }
}

topXRecurringCustomers = function (dbConnection, topX) {
    if (is.numeric(topX)) {
        topXVevo = dbGetQuery(dbConnection, paste("select vevo.nev, count(szamla.id_szamla) as \"Count\"",
                                                   "from szamla join ", 
                                                   "vevo on vevo.id_vevo = szamla.id_vevo",
                                                   "group by vevo.nev",
                                                   "order by \"Count\" desc"))
        print("A leghűségesebb vevők: ")
        for (i in 1:topX) {
            print(paste(topXVevo[i,1], "-", topXVevo[i,2], "db számla"))
        }
    } else {
        stop("The topX variable should be a number")
    }
}

topXLatePayers = function (dbConnection, topX) {
    ## In the 2015 database, fizdatum is always null
    if (is.numeric(topX)) {
        topXVevo = dbGetQuery(dbConnection, paste("select vevo.nev, count(szamla.id_szamla) as \"Count\"",
                                                  "from szamla join ", 
                                                  "vevo on vevo.id_vevo = szamla.id_vevo",
                                                  "where szamla.fizdatum > szamla.fizhatar",
                                                  "group by vevo.nev",
                                                  "order by \"Count\" desc"))
        print("A legtöbbet késve fizető vevők: ")
        for (i in 1:topX) {
            print(paste(topXVevo[i,1], "-", topXVevo[i,2], "késés"))
        }
    } else {
        stop("The topX variable should be a number")
    }
}

topXBestCustomerForAllProducts(connection, 10)
topXBestCustomerForFarmmixProducts(connection, 10)
topXRecurringCustomers(connection, 10)
topXLatePayers(connection, 10)

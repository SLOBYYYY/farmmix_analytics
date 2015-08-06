AgentSales = function (connection) {
    thisEnv = environment()
    if (class(connection) == "JDBCConnection") {
        localConnection = connection
        result = NULL
        loadSpecialProducts = function () {
            command = paste("select id_termek",
                            "from termek",
                            "where lower(termek.nev) in ('adengo  1', 'adengo  5', 'afalon disp.  5', 'antracol wg   6', 'bactospeine  5', 'biathlon 4d', 'biscaya  3', 'bumper 25 ec  5',", 
			       							  "'calypso 480 sc  1', 'cambio          5', 'colombus  1', 'colombus  5', 'coragen 20 sc  1', 'coragen 20 sc  0,2', 'curzate super df  5',",
			       							  "'cuproxat        5', 'cuproxat        20', 'dithane dg neotec  10', 'dithane m-45      25', 'folpan 80 wdg   5', 'fontelis 20 sc  1',",
			       							  "'galben r  5', 'galben r  25', 'galera sl   5', 'galigan 240 ec  5', 'goal duplo   3', 'inazuma  1', 'kaiso eg  1', 'laudis  1',",
			       							  "'laudis  5', 'lingo  5', 'mavrik 24 ew  1', 'melody compact 49 wg 6', 'mextrol b  5', 'megysto  5', 'mildicut  10', 'mist control     5',",
			       							  "'mist-control    5', 'monsoon active  5', 'montaflow sc  10', 'mustang forte  1', 'mustang forte  5', 'mystic pro  5', 'nimrod 25 ec    1',",
			       							  "'nuflon  5', 'nurelle-d 500 ec 1', 'nurelle-d 500 ec 5', 'ordax super (0,45l c+10l ss+3l d)', 'pendigan 330 ec   10', 'perenal  5',",
			       							  "'pictor  5', 'prosaro               5', 'prolectus  0,25', 'pulsar          5', 'pyrinex 48 ec   5', 'pyrinex supreme  5', 'racer 25 ec     5',",
			       							  "'sekator od  1', 'solofol 80 wdg  10', 'stabilan sl     10', 'systhane duplo  1', 'trek p  5', 'tango star      5', 'teppeki 50 wg  0,5',",
			       							  "'warrant 200 sl  1', 'wing p  10', 'zantara ec 216  5', 'zoom 11 sc  1', 'python duplo 6ha',",
                                              "'pulsar          5', 'stellar 1+ dash 1','taltos+polyglycol  1,5+22,5', 'taltos+polyglycol  25*(0,033+0,5)') or ",
			       							  "lower(termek.nev) like 'bayer sz_l_ cs.'")
                temp = dbGetQuery(localConnection, command)
                colnames(temp) = c("id")
                return(temp)
        }
        aggregateByCriteria = function (data, agents, criteria) {
            ss = data[which(criteria),]
            if (nrow(ss) == 0) {
                return (rep(0, length(agents$agent_name)))
            }
            aggregated.sales = aggregate(ss$totalprice, 
                                         list(ss$agent_name),
                                         sum)
            colnames(aggregated.sales) = c("agent_name", "sum")
            merged = merge(agents, aggregated.sales, by="agent_name", all.x=T)[,2]
            return(merged)
        }
        aggregateByCriteriaForNovenyvedoszer = function (data, agents, criteria) {
            ss = subset(data, grepl("^GYOM.RT. SZER$|^BIOCID$|^GOMBA.L. SZER$|^ROVAR.L. SZER$|^TALAJFERT.TLEN.T. SZER$|^ADAL.K ANYAG$|^CS.V.Z. SZER$|^N.V.NYV.D. SZEREK$", result$group_name))
            return(aggregateByCriteria(ss, agents, criteria))
        }
        aggregateByCriteriaForNovenyvedoszerAndMutragya = function (data, agents, criteria) {
            ss = subset(data, grepl("^GYOM.RT. SZER$|^BIOCID$|^GOMBA.L. SZER$|^ROVAR.L. SZER$|^TALAJFERT.TLEN.T. SZER$|^ADAL.K ANYAG$|^CS.V.Z. SZER$|^N.V.NYV.D. SZEREK$|^M.TR.GYA$", result$group_name))
            return(aggregateByCriteria(ss, agents, criteria))
        }
        aggregateByCriteriaForNovenyvedoszerAndVetomag = function (data, agents, criteria) {
            ss = subset(data, grepl("^GYOM.RT. SZER$|^BIOCID$|^GOMBA.L. SZER$|^ROVAR.L. SZER$|^TALAJFERT.TLEN.T. SZER$|^ADAL.K ANYAG$|^CS.V.Z. SZER$|^N.V.NYV.D. SZEREK$|^VET.MAG$", result$group_name))
            return(aggregateByCriteria(ss, agents, criteria))
        }
        aggregateByCriteriaForVetomag = function (data, agents, criteria) {
            ss = subset(data, grepl("^VET.MAG$", result$group_name))
            return(aggregateByCriteria(ss, agents, criteria))
        }
        me = list (
            thisEnv = thisEnv,
            getEnv = function () {
                return(get("thisEnv", thisEnv))
            },
            getResult = function () {
                return(get("result", thisEnv))
            },
            load = function (from, to) {
                command = paste("select szamlatetel.eladar * szamlatetel.mennyiseg as \"EladarSum\", ",
                                " csoport.nev, forgalmazo.nev, uzletkoto.nev, termek.id_termek, termek.nev",
                                " from szamlatetel join ", 
                                " szamla on szamla.id_szamla = szamlatetel.id_szamla join",
                                " termek on termek.id_termek = szamlatetel.id_termek join",
                                " forgalmazo on forgalmazo.id_forgalmazo = termek.id_forgalmazo join",
                                " csoport on csoport.id_csoport = termek.id_csoport left join",
                                " uzletkoto on uzletkoto.id_uzletkoto = szamla.id_uzletkoto",
                                " where szamla.datum >='", from, "' and szamla.datum <='", to,"'", 
                                sep="")
                temp = dbGetQuery(localConnection, command)
                colnames(temp) = c("totalprice", "group_name", "provider_name", "agent_name", "product_id", "product_name")
                assign("result", temp, thisEnv)
                print("Data is loaded into memory")
            },
            report = function () {
                special.products = loadSpecialProducts()
                agent.sales = data.frame("agent_name"=sort(unique(result$agent_name)))
                agents = data.frame("agent_name"=sort(unique(result$agent_name)))
                agent.sales$FarmmixNovszMtVetomag = aggregateByCriteria(result, agents, grepl("^FARMMIX KFT$", result$provider_name))
                agent.sales$FAlternativNovszMt = aggregateByCriteria(result, agents, grepl("^FARMMIX KFT ALT", result$provider_name))
                agent.sales$Agrosol = aggregateByCriteria(result, agents, grepl("AGROSOL", result$provider_name))
                agent.sales$Vetco = aggregateByCriteria(result, agents, grepl("VETCO", result$provider_name))
                agent.sales$Kiemelt = aggregateByCriteria(result, agents, (result$product_id %in% special.products$id))
                
                # TODO: not done yet
                # Az "Egyéb" ez esetben tényleg a szolgáltatónál jelenti, hogy egyéb vagy az összes többi aki nincs benne 
                # a többi lekérdezésben itt? 
                # Ami maradt: Egyéb, Magyar Telekom Nyrt, Martonvásár /Elitmag KFT/, Saaten-Union
                agent.sales$EgyebNagyGyartohozNemKotheto = aggregateByCriteriaForNovenyvedoszer(result, agents, grepl("^EGY.B$|^MARTONV.S.R|^SAATEN", result$provider_name))
                agent.sales$Adama = aggregateByCriteriaForNovenyvedoszer(result, agents, grepl("^ADAMA", result$provider_name))
                agent.sales$Arysta = aggregateByCriteriaForNovenyvedoszer(result, agents, grepl("^ARYSTA", result$provider_name))
                agent.sales$BASF = aggregateByCriteriaForNovenyvedoszerAndMutragya(result, agents, grepl("^BASF", result$provider_name))
                agent.sales$Belchim = aggregateByCriteriaForNovenyvedoszer(result, agents, grepl("^BELCHIM", result$provider_name))
                agent.sales$Cheminova = aggregateByCriteriaForNovenyvedoszerAndMutragya(result, agents, grepl("^CHEMINOVA", result$provider_name))
                agent.sales$Chemtura = aggregateByCriteriaForNovenyvedoszer(result, agents, grepl("^CHEMTURA$", result$provider_name))
                agent.sales$Dow = aggregateByCriteriaForNovenyvedoszerAndVetomag(result, agents, grepl("^DOW", result$provider_name))
                agent.sales$Dupont = aggregateByCriteriaForNovenyvedoszer(result, agents, grepl("^DUPONT", result$provider_name))
                agent.sales$Kwizda = aggregateByCriteriaForNovenyvedoszerAndMutragya(result, agents, grepl("^KWIZDA", result$provider_name))
                agent.sales$Nufarm = aggregateByCriteriaForNovenyvedoszer(result, agents, grepl("^NUFARM", result$provider_name))
                agent.sales$SumiAgro = aggregateByCriteriaForNovenyvedoszer(result, agents, grepl("^SUMI AGRO", result$provider_name))
                # Van "Syngenta KFT" és "Syngenta vetőmag". Melyik legyen?
                agent.sales$Syngenta = aggregateByCriteriaForNovenyvedoszer(result, agents, grepl("^SYNGENTA", result$provider_name))
                
                agent.sales$Gabonakutato = aggregateByCriteriaForVetomag(result, agents, grepl("^GABONAKUTAT.", result$provider_name))
                agent.sales$EgyebVetomag = aggregateByCriteriaForVetomag(result, agents, (grepl("^EGY.B$|^MARTONV.S.R|^SAATEN", result$provider_name) & grepl("^VET.MAG$", result$group_name)))
                agent.sales$KWS = aggregateByCriteriaForVetomag(result, agents, grepl("^KWS", result$provider_name))
                agent.sales$Limagrain = aggregateByCriteriaForVetomag(result, agents, grepl("^LIMAGRAIN", result$provider_name))
                agent.sales$Monsanto = aggregateByCriteriaForVetomag(result, agents, grepl("^MONSANT", result$provider_name))
                # Nincs ilyen, hogy MV
                agent.sales$MV = aggregateByCriteriaForVetomag(result, agents, grepl("^MV", result$provider_name))
                agent.sales$Pioneer = aggregateByCriteriaForVetomag(result, agents, grepl("^PIONEER", result$provider_name))
                agent.sales$Ragt = aggregateByCriteriaForVetomag(result, agents, grepl("^RAGT", result$provider_name))
                agent.sales$SumiAgro = aggregateByCriteriaForVetomag(result, agents, grepl("^SUMI AGRO", result$provider_name))
                agent.sales$Syngenta = aggregateByCriteriaForVetomag(result, agents, grepl("^SYNGENTA", result$provider_name))
                
                agent.sales$EgyebMutragya = aggregateByCriteria(result, agents, (grepl("^EGY.B$", result$provider_name) &
                                                                             grepl("^M.TR.GYA$", result$group_name) &
                                                                             grepl("^MT|^YARA|^TIMAC", result$product_name) 
                                                                             ))
                return(agent.sales)
            }
        )
        assign('this', me, envir = thisEnv)
        class(me) = append(class(me), "AgentSales")
        return(me)
    }
}
as = AgentSales(connection)
as$load("2015-01-01", "2015-05-31")
as$report()

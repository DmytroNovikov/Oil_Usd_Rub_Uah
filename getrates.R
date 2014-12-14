getUsdUahNbuRates <- function(dateFrom, dateTo){
    #
    # Динамика курса USD, грн. КУРС ГРИВНЫ НБУ
    # http://www.finam.ru/analysis/profile2B02900007/?market=5&em=176164&code=RUBUSD&df=1&mf=11&yf=2014&from=01.12.2014&dt=14&mt=11&yt=2014&to=14.12.2014&p=8&f=RUBUSD_141201_141214&e=.csv&cn=RUBUSD&dtf=1&tmf=1&MSOR=1&mstimever=0&sep=1&sep2=2&datf=5&at=1&fsp=1
    #
    # Create a "GET" query to 
    #
    get_query <- "http://195.128.78.52/USD_UAH_RATES.csv?market=5&em=176169&code=USDUAH&"
    get_query <- paste(get_query, 'df=', format(dateFrom, "%d"),
                       '&mf=', toString(as.numeric(format(dateFrom, "%m")) - 1),
                       '&yf=', format(dateFrom, "%Y"),
                       '&from=', format(dateFrom, "%d.%m.%Y"),
                       '&',
                       sep='')
    get_query <- paste(get_query, 'dt=', format(dateTo, "%d"),
                       '&mt=', toString(as.numeric(format(dateTo, "%m")) - 1),
                       '&yt=', format(dateTo, "%Y"),
                       '&to=', format(dateTo, "%d.%m.%Y"),
                       '&',
                       sep='')
    get_query <- paste(get_query,
                       'p=8&f=USD_UAH_RATES&e=.csv&cn=USDUAH&dtf=1&tmf=1&MSOR=1&mstimever=0&sep=1&sep2=2&datf=5&at=1&fsp=1',
                       sep='')
    usd_uah_rates <- NULL
    if (download.file(get_query, destfile="usd_uah_nbu.csv") == 0) {
        # Загрузка данный из файла в data.frame
        usd_uah_rates <- read.csv("usd_uah_nbu.csv",  stringsAsFactors=FALSE)
        # Изменение наименований столбцов на более читаемые
        names(usd_uah_rates) <- c("Date", "Time", "Open", "High", "Low", "Close", "Volume")
        # Приведение данных в столбце Date к типу Дата
        usd_uah_rates$Date <- as.Date(sapply(usd_uah_rates$Date, toString), format='%Y%m%d')
    }
    return(usd_uah_rates)
}

getUsdRubRates <- function(dateFrom, dateTo){
    #
    # Динамика курса USD, руб.КУРС РУБЛЯ
    # http://www.finam.ru/analysis/profile0038500007/?market=5&em=176164&code=RUBUSD&df=1&mf=11&yf=2014&from=01.12.2014&dt=14&mt=11&yt=2014&to=14.12.2014&p=8&f=RUBUSD_141201_141214&e=.csv&cn=RUBUSD&dtf=1&tmf=1&MSOR=1&mstimever=0&sep=1&sep2=2&datf=5&at=1&fsp=1
    #
    # Create a "GET" query to 
    #
    get_query <- "http://195.128.78.52/USD_RUB_RATES.csv?market=5&em=901&code=USDRUB&"
    get_query <- paste(get_query, 'df=', format(dateFrom, "%d"),
                       '&mf=', toString(as.numeric(format(dateFrom, "%m")) - 1),
                       '&yf=', format(dateFrom, "%Y"),
                       '&from=', format(dateFrom, "%d.%m.%Y"),
                       '&',
                       sep='')
    get_query <- paste(get_query, 'dt=', format(dateTo, "%d"),
                       '&mt=', toString(as.numeric(format(dateTo, "%m")) - 1),
                       '&yt=', format(dateTo, "%Y"),
                       '&to=', format(dateTo, "%d.%m.%Y"),
                       '&',
                       sep='')
    get_query <- paste(get_query,
                       'p=8&f=USD_RUB_RATES&e=.csv&cn=USDRUB&dtf=1&tmf=1&MSOR=1&mstimever=0&sep=1&sep2=2&datf=5&at=1&fsp=1',
                       sep='')
    usd_rub_rates <- NULL
    if (download.file(get_query, destfile="usd_rub.csv") == 0) {
        # Загрузка данный из файла в data.frame
        usd_rub_rates <- read.csv("usd_rub.csv",  stringsAsFactors=FALSE)
        # Изменение наименований столбцов на более читаемые
        names(usd_rub_rates) <- c("Date", "Time", "Open", "High", "Low", "Close", "Volume")
        # Приведение данных в столбце Date к типу Дата
        usd_rub_rates$Date <- as.Date(sapply(usd_rub_rates$Date, toString), format='%Y%m%d')
    }
    return(usd_rub_rates)
}

getUsdRubCBRates <- function(dateFrom, dateTo){
    #
    # Динамика курса USD ЦБ РФ, руб.КУРС РУБЛЯ
    # http://www.finam.ru/analysis/profile1423500007/?market=6&em=182397&code=USDEURBASKET&df=1&mf=0&yf=2014&from=01.01.2014&dt=14&mt=11&yt=2014&to=14.12.2014&p=8&f=USDEURBASKET_140101_141214&e=.csv&cn=USDEURBASKET&dtf=1&tmf=1&MSOR=1&mstimever=0&sep=1&sep2=2&datf=1&at=1
    #
    # Create a "GET" query to 
    #
    get_query <- "http://195.128.78.52/USD_RUB_CBRATES.csv?market=41&em=82485&code=USDCB&"
    get_query <- paste(get_query, 'df=', format(dateFrom, "%d"),
                       '&mf=', toString(as.numeric(format(dateFrom, "%m")) - 1),
                       '&yf=', format(dateFrom, "%Y"),
                       '&from=', format(dateFrom, "%d.%m.%Y"),
                       '&',
                       sep='')
    get_query <- paste(get_query, 'dt=', format(dateTo, "%d"),
                       '&mt=', toString(as.numeric(format(dateTo, "%m")) - 1),
                       '&yt=', format(dateTo, "%Y"),
                       '&to=', format(dateTo, "%d.%m.%Y"),
                       '&',
                       sep='')
    get_query <- paste(get_query,
                       'p=8&f=USD_RUB_CBRATES&e=.csv&cn=USDCB&dtf=1&tmf=1&MSOR=1&mstimever=0&sep=1&sep2=2&datf=5&at=1&fsp=1',
                       sep='')    
    usd_rub_rates <- NULL
    if (download.file(get_query, destfile="usd_rub_cb.csv") == 0) {
        # Загрузка данный из файла в data.frame
        usd_rub_rates <- read.csv("usd_rub_cb.csv",  stringsAsFactors=FALSE)
        # Изменение наименований столбцов на более читаемые
        names(usd_rub_rates) <- c("Date", "Time", "Open", "High", "Low", "Close", "Volume")
        # Приведение данных в столбце Date к типу Дата
        usd_rub_rates$Date <- as.Date(sapply(usd_rub_rates$Date, toString), format='%Y%m%d')
    }
    return(usd_rub_rates)
}

getBrentRates <- function(dateFrom, dateTo){
    #
    # Динамика цен на Нефть Brent (ICE.Brent), USD/баррель
    # http://www.finam.ru/analysis/profile04C1100007/?market=41&em=82485&code=USDCB&df=1&mf=0&yf=2014&from=01.01.2014&dt=14&mt=11&yt=2014&to=14.12.2014&p=8&f=USDCB_140101_141214&e=.csv&cn=USDCB&dtf=1&tmf=1&MSOR=1&mstimever=0&sep=1&sep2=2&datf=1&at=1
    #
    # Create a "GET" query to 
    #
    #
    get_query <- "http://195.128.78.52/BRENT_RATES.csv?market=24&em=19473&code=ICE.BRN&"
    get_query <- paste(get_query, 'df=', format(dateFrom, "%d"),
                       '&mf=', toString(as.numeric(format(dateFrom, "%m")) - 1),
                       '&yf=', format(dateFrom, "%Y"),
                       '&from=', format(dateFrom, "%d.%m.%Y"),
                       '&',
                       sep='')
    get_query <- paste(get_query, 'dt=', format(dateTo, "%d"),
                       '&mt=', toString(as.numeric(format(dateTo, "%m")) - 1),
                       '&yt=', format(dateTo, "%Y"),
                       '&to=', format(dateTo, "%d.%m.%Y"),
                       '&',
                       sep='')
    get_query <- paste(get_query,
                       'p=8&f=BRENT_RATES&e=.csv&cn=ICE.BRN&dtf=1&tmf=1&MSOR=1&mstimever=0&sep=1&sep2=2&datf=5&at=1&fsp=1',
                       sep='')
    brent_rates <- NULL
    if (download.file(get_query, destfile="brent.csv") == 0) {
        # Загрузка данный из файла в data.frame
        brent_rates <- read.csv("brent.csv",  stringsAsFactors=FALSE)
        # Изменение наименований столбцов на более читаемые
        names(brent_rates) <- c("Date", "Time", "Open", "High", "Low", "Close", "Volume")
        # Приведение данных в столбце Date к типу Дата
        brent_rates$Date <- as.Date(sapply(brent_rates$Date, toString), format='%Y%m%d')
    }    
    return(brent_rates)
}

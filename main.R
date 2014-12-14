source("getrates.R")

rm(list=c("new_df", "brent", "uhnb", "urcb", "date_from", "date_to", "i", "temp", "tmpd"))

# Дата начала периода для выполнения анализа
date_from <- as.Date('01.01.2014', format='%d.%m.%Y')

# Дата окончания периода для выполнения анализа
#date_to <- as.Date('14.12.2014', format='%d.%m.%Y')
date_to <- as.Date(format(Sys.time(), '%d.%m.%Y'), format='%d.%m.%Y')

# Рассматриваем три показателя:
#   - курс USD/RUB ЦБ РФ
#   - курс USD/UAH НБУ
#   - стоимость барреля нефти Brent (ICE.Brent), USD/баррель

brent <- getBrentRates(date_from, date_to)
if (!is.null(brent)) {
    urcb <- getUsdRubCBRates(date_from, date_to)
    if (!is.null(urcb)) {
        uhnb <- getUsdUahNbuRates(date_from, date_to)
        if (!is.null(uhnb)) {
            #
            # Создание data.frame, содержащего три рассматриваемых показателя
            #
            temp <- replicate(nrow(brent), 0.0)
            new_df <- data.frame(brent$Date,
                                 brent$Close, brent$Volume,
                                 temp,
                                 temp)
            names(new_df) <- c("Date",
                               "PriceBrent", "VolumeBrent",
                               "RateRub",
                               "RateUah")
            #
            # Заполнение созданного data.frame данными о курсе USD/RUB
            #
            temp <- nrow(urcb)
            for (i in 1:temp) {
                tmpd <- urcb[i, "Date"]
                if (nrow(new_df[new_df$Date == tmpd,]) != 0) {
                    new_df[new_df$Date == tmpd, "RateRub"] <- urcb[i, "Close"]
                } else {
                    new_df <- rbind(new_df, list(tmpd,
                                              0.0, 0.0,
                                              urcb[i, "Close"],
                                              0.0))
                }
            }
            #
            # Заполнение созданного data.frame данными о курсе UAH/RUB
            #
            temp <- nrow(uhnb)
            for (i in 1:temp) {
                tmpd <- uhnb[i, "Date"]
                if (nrow(new_df[new_df$Date == tmpd,]) != 0) {
                    new_df[new_df$Date == tmpd, "RateUah"] <- uhnb[i, "Close"]
                } else {
                    new_df <- rbind(new_df, list(tmpd,
                                                 0.0, 0.0,
                                                 0.0,
                                                 uhnb[i, "Close"]))
                }
            }
            #
            # Сортировка созданного data.frame по дате
            #
            new_df <- new_df[order(new_df$Date),]
            #
            # Заполнение пустых значений в data.frame данными за предыдущий день
            #
            temp <- nrow(new_df)
            for(i in 2:temp) {
                if (new_df[i, "PriceBrent"] == 0.0) new_df[i, "PriceBrent"] <- new_df[i-1, "PriceBrent"]
                if (new_df[i, "RateRub"] == 0.0) new_df[i, "RateRub"] <- new_df[i-1, "RateRub"]
                if (new_df[i, "RateUah"] == 0.0) new_df[i, "RateUah"] <- new_df[i-1, "RateUah"]
            }
            #
            # Заполнение пустых значений в первой строке data.frame данными за следующий день
            #            
            if (new_df[1, "PriceBrent"] == 0.0) new_df[1, "PriceBrent"] <- new_df[2, "PriceBrent"]
            if (new_df[1, "RateRub"] == 0.0) new_df[1, "RateRub"] <- new_df[2, "RateRub"]
            if (new_df[1, "RateUah"] == 0.0) new_df[1, "RateUah"] <- new_df[2, "RateUah"]
            
            png(filename = "plot.png",
                width = 1920, height = 1080, units = "px")
            par(mfrow = c(2,3), bg = "transparent")
            # 
            # Crude Oil Brent Price
            #
            plot(new_df$Date, new_df$PriceBrent,
                 type='l', col='black',
                 main='Crude Oil Brent Price',
                 xlab='2014', ylab='USD / barrel')
            # 
            # USD - RUB Exchange Rate
            # 
            plot(new_df$Date, new_df$RateRub,
                 type='l', col='blue',
                 main='USD - RUB Exchange Rate',
                 xlab='2014', ylab='Price of 1 USD in RUB')
            # 
            # USD - UAH Exchange Rate
            # 
            plot(new_df$Date, new_df$RateUah,
                 type='l', col='green',
                 main='USD - UAH Exchange Rate',
                 xlab='2014', ylab='Price of 1 USD in UAH')
            #
            # Exchange rate relative to the rate on 01.01.2014
            #
            plot(new_df$Date, new_df$RateUah / new_df[1, "RateUah"],
                 type='l', col='green',
                 main='Exchange rate relative to the rate on 01.01.2014',
                 xlab='2014', ylab='')
            points(new_df$Date, new_df$RateRub / new_df[1, "RateRub"],
                 type='l', col='blue')
            legend("topleft", bty = "o", lty = 1, col = c("green", "blue"), legend = c("UAH", "RUB"))
            # 
            # Crude Oil Brent Price in RUB
            #
            plot(new_df$Date, new_df$PriceBrent * new_df$RateRub,
                 type='l', col='red',
                 main='Crude Oil Brent Price',
                 xlab='2014', ylab='RUB / barrel')
            # 
            # Crude Oil Brent Price (2000 - 2014)
            #
            urcbl <- getBrentRates(as.Date('01.01.2000', format='%d.%m.%Y'), date_to)
            plot(urcbl$Date, urcbl$Close,
                 type='l', col='black',
                 main='Crude Oil Brent Price (2000 - 2014)',
                 xlab='', ylab='USD / barrel')

            dev.off()            
        } else {
            print("Ошибка при получении файла с курсом НБУ USD относительно UAH.")
        }
    } else {
        print("Ошибка при получении файла с курсом ЦБ РФ USD относительно RUB.")
    }
} else {
    print("Ошибка при получении файла с ценами на нефть Brent.")
}

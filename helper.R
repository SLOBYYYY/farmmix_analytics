plotYCont = function (x) {
    paste(round(x/1000000,1), " Millió Ft")
}

drawPieChart = function (data, text) {
    # data's first 2 columns should be labels and values
    labels = data[,1]
    slices = data[,2]
    percentages = round(slices/sum(slices)*100)
    labels = paste(labels, percentages)
    labels = paste(labels, "%", sep="")
    labels = paste(labels, " (", round(slices, digits=0), ")", sep="")
    pie(slices, labels = labels, col=rainbow(length(labels)),
        main=text)
}

drawBarChart = function (data, xText, yText) {
    data.length = dim(data)[1]
    colors = rainbow(data.length)
    data = arrange(data, -data[,2])
    data.barplot = barplot(data[,2], 
                             cex.names=0.6, 
                             xlab=xText,
                             ylab=yText, 
                             ylim=c(0,max(data[,2]) * 1.2),
                             col=colors)
    text(data.barplot, data[,2], labels=ft.format(data[,2], rounding="million"), pos=3, cex=0.7)
    legend("topright", legend = data[,1], fill=colors, cex=0.7)
}

filterOnePercentRows = function (data) {
    data.sum = sum(data[,2])
    data$percent = round(data[,2] / data.sum * 100)
    data.filtered = data[which(data$percent > 1),]
    small.group.difference = data.sum - sum(data.filtered[,2])
    small.group.percentage = round(small.group.difference / data.sum * 100)
    additional.row = data.frame("1% alattiak", small.group.difference, small.group.percentage)
    colnames(additional.row) = colnames(data)
    data.filtered = rbind(data.filtered, additional.row)
    return (data.filtered)
}

ft.format = function (szam, rounding="none") {
    switch(rounding,
           none = paste(format(round(szam,0),big.mark=","),"Ft"),
           thousand = paste(format(round(szam/1000,1),big.mark=","),"e Ft"),
           million = paste(format(round(szam/1000000,2),big.mark=","),"M Ft")
           )
}
#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @import RCurl
#' @examples
#' cat_function()

series_bacen <- function(arg1, ...){

    datas = format(Sys.Date(), "%d/%m/%Y")
    inputs = as.character(list(arg1, ...))
    len = seq_along(inputs)
    series = mapply(paste0, "serie_", inputs, USE.NAMES = FALSE)

    for (i in len){ assign(series[i],
                             getURL(paste0('http://api.bcb.gov.br/dados/serie/bcdata.sgs.', inputs[i],
                                           '/dados?formato=csv&dataInicial=01/04/2003&dataFinal=',datas),
                                            ssl.verifyhost=FALSE, ssl.verifypeer=FALSE))}


    for (i in len){ assign(series[i], read.csv(textConnection(eval(as.symbol(
        series[i]))), header=T))}


    for(i in len) {write.csv(eval(as.symbol(series[i])), file = paste0(series[i], ".csv"))}

}

#' A function to extract series using
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @import RCurl xlsx
#' @examples
#' series_bacen(1242,2134)
#' series_bacen(1242,2134, file = "C:/Users/fernando.teixeira/Dropbox/Luciana+Maclá/Índice de Atividade Econômica do Bacen (IBC-BR) - Com ajuste.xlsm")

series_bacen <- function(arg1, ..., file = NULL, save = "csv"){

    datas = format(Sys.Date(), "%d/%m/%Y")
    # arg1 = 1242; arg2 = 2134
    inputs = as.character(list(arg1, ...))
    #inputs = as.character(list(arg1, arg2))
    len = seq_along(inputs)
    serie = mapply(paste0, "serie_", inputs, USE.NAMES = FALSE)

    for (i in len){ assign(serie[i],
                             getURL(paste0('http://api.bcb.gov.br/dados/serie/bcdata.sgs.', inputs[i],
                                           '/dados?formato=csv&dataInicial=01/01/2003&dataFinal=',datas),
                                            ssl.verifyhost=FALSE, ssl.verifypeer=FALSE))}


    for (i in len){


        texto = read.csv(textConnection(eval(as.symbol(
            serie[i]))), header=T)

        texto$datas = gsub(' .*$','', eval(texto$data))

        assign(serie[i], texto)

        # assign(serie[i], read.csv(textConnection(eval(as.symbol(
        # serie[i]))), header=T))

        #assign(eval(as.symbol(serie[i]))$dados, gsub(' .*$','', eval(as.symbol(serie[i]))$data))

    }


    if (save == "csv"){
    for(i in len) {write.csv(eval(as.symbol(serie[i])), file = paste0(serie[i], ".csv"))}
    } else if (save == "xls" | save == "xlsx") {
    for(i in len) {write.xlsx(eval(as.symbol(serie[i])), file = paste0(serie[i], ".xlsx"), row.names = FALSE)}} else{ stop("save argument must be 'csv' or 'xlsx' ")}



    if (!is.null(file)){
    #Physically open an excel file
        shell.exec(file)
    }
    # lista = list(wd = wd)
    # return(invisible(lista))

}

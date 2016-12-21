#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @import RCurl xlsx
#' @examples
#' cat_function()

series_bacen <- function(arg1, ..., file = NULL){

    datas = format(Sys.Date(), "%d/%m/%Y")
    # arg1 = 1242; arg2 = 2134
    inputs = as.character(list(arg1, ...))
    #inputs = as.character(list(arg1, arg2))
    len = seq_along(inputs)
    serie = mapply(paste0, "serie_", inputs, USE.NAMES = FALSE)

    for (i in len){ assign(serie[i],
                             getURL(paste0('http://api.bcb.gov.br/dados/serie/bcdata.sgs.', inputs[i],
                                           '/dados?formato=csv&dataInicial=01/04/1950&dataFinal=',datas),
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



    for(i in len) {write.csv(eval(as.symbol(serie[i])), file = paste0(serie[i], ".csv"))}
    for(i in len) {write.xlsx(eval(as.symbol(serie[i])), file = paste0(serie[i], ".xlsx"))}


    if (!is.null(file)){
    #Physically open an excel file
        shell.exec(file)
    }
    # lista = list(wd = wd)
    # return(invisible(lista))

}

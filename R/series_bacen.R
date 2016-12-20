#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

series_bacen <- function(arg1, ...){
    
    inputs = as.character(list(arg1, ...))
    len = seq_along(inputs)
    
    for (i in 1:len){ assign(paste0("serie_", inputs[i]),
                             getURL(paste0('http://api.bcb.gov.br/dados/serie/bcdata.sgs.', inputs[i], '/dados?formato=csv&dataInicial=01/04/2003&dataFinal=',data), ssl.verifyhost=FALSE, ssl.verifypeer=FALSE))}
    
    
    
}
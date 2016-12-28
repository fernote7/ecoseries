#' A function to extract Sidra series using their API
#' @param cod_tabela Sidra series number.
#' @param periodos Specifies the desired periods. Defaults to 'all'.
#' @param var Specifies the desired variables. Defaults to 'allxp'.
#' @param terr Specifies the desired territorial levels. Defaults to 'n1/1' - Brazil.
#' @param classe Specifies the table classifications. Defaults to "".
#' @param dec Specifies the desired decimals in table numbers.
#' @param header Logical. Either TRUE or FALSE.
#' @param save A string specifying if data should be saved in csv or xlsx format. 
#' Defaults to not saving.
#' @param formato Specifies the formatting of the table. Defaults to 'n' - metadata names only. 
#' @keywords sidra
#' @export
#' @import RCurl rjson
#' @examples
#' ibge=series_sidra(cod_tabela = "1612", formato = "n")    

series_sidra <- function(cod_tabela, periodos = "all", var = "allxp", terr = "n1/1", 
                         classe = "", dec = 2, header = FALSE, save = "", formato = "n"){
    
    terr = gsub(' ','%', terr)
    if (classe != "") {classe = paste0(classe, "/")}
    if (formato != "c" & formato != "n" & formato != "u" & formato != "a"){ 
        stop("formato argument must be 'c', 'n', 'u' or 'a' ")}
    if ( header == TRUE | T) { 
        header = "y"
    } else if (header == FALSE | F) { 
        header = "n"
    } else { stop("header assume only TRUE or FALSE values")}
    
    tabela=RCurl::getURL(paste0("http://api.sidra.ibge.gov.br/values/",
                         "t/", cod_tabela, "/", terr, "/", classe, "p/", periodos, 
                         "/v/", var, "/f/", formato, "/h/", header),
             ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
    
    
    if (strsplit(tabela, " ")[[1]][1] == "Par\uE2metro") {
        
        param = strsplit(tabela, " ")[[1]][3]
        param = substr(param, 2, nchar(param)-1)
        stop(sprintf("The parameter %s is misspecified", param))
        
        
    } else{
        t1 = paste("tabela", cod_tabela, sep="_")
        #assign(t1, tabela)
        tabela = rjson::fromJSON(tabela)
        tabela=data.frame(do.call("rbind", tabela))
        
        #Transformando a coluna V em valor
        
        valor = NULL
        
        id = which(colnames(tabela)=="V")
            
            tabela2 = tabela
            #tabela2[tabela2[,id] ==  "..",id] <- NA
            #tabela2[,id] <- as.numeric(tabela2[,id])
            tabela[,id] = suppressWarnings(ifelse(tabela[,id]!="..", as.numeric(tabela[,id]),NA))
    }

    assign(t1,tabela)
    rm(tabela)

    return(invisible(eval(as.symbol(t1))))
    
}

#' A function to extract Sidra series using their API
#' 
#' The different parameters define a table and its dimensions (periods, variables, territorial units and classification) to be consulted. The parameters that define the territorial units can be used more than once, allowing multiple distinct territorial searches with one request. The parameters that define the classes may vary from table to table, with a maximum of 6. Henceforth, a Sidra table ranges between 3 and 9 differente dimension (3 mandatory - periods, var and terr and 6 optional).
#' @param cod_tabela Sidra series number.
#' @param from A string specifying where the series shall start. Defaults to 1980.
#' @param to A string specifying where the series shall end. Defaults to current year.
#' @param territory Specifies the desired territorial levels.
#' @param dec Specifies the desired decimals in table numbers.
#' @param header Logical. Either TRUE or FALSE.
#' @param save A string specifying if data should be saved in csv or xlsx format. 
#' Defaults to not saving.
#' @keywords sidra
#' @export
#' @import RCurl rjson
#' @examples
#' series_sidra(cod_tabela = 1612; from = ""; to = ""; territory = "brazil")



series_sidra <- function(cod_tabela, from = "", to = "", territory = c(n1 = "brazil", n2 = "region", n3 = "state"), dec = 2, header = TRUE, save = ""){
    
    cod_tabela = as.character(cod_tabela)
    
    
    if (from == ""){
        data_init = "1980"
    } else {data_init = from}
    
    if (to == ""){
        data_end = format(Sys.Date(), "%Y")
    } else {data_end = to}
    
    
    # Território
    territory <- base::match.arg(territory)
    territory <- base::switch(territory,
                              brazil = "n1/all", 
                              region = "n2/all", 
                              state = "n3/all")
    
    if ( header == TRUE | header == T) { 
        header = "y"
    } else if (header == FALSE | header == F) { 
        header = "n"
    } else { stop("header assume only TRUE or FALSE values")}


    tabela=RCurl::getURL(paste0("http://api.sidra.ibge.gov.br/values/",
                         "t/", cod_tabela, "/", territory, "/", "p/", 
                         data_init, "-", data_end,  
                         "/v/", "allxp", "/f/", "a", "/h/", header),
             ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
    
    
    if (strsplit(tabela, " ")[[1]][1] == "Par\uE2metro") {
        
        param = strsplit(tabela, " ")[[1]][3]
        param = substr(param, 2, nchar(param)-1)
        stop(sprintf("The parameter %s is misspecified", param))
        
        
    } else{
        t1 = paste("tabela", cod_tabela, sep="_")
        tabela = rjson::fromJSON(tabela)
        tabela=data.frame(do.call("rbind", tabela))
        if (header == "y"){
            
            colnames(tabela) = unlist(tabela[1,])
            tabela = tabela[-1,]
        }
        
        #Transformando a coluna V em valor
        
        valor = NULL
        
        id = which(colnames(tabela)=="V" | colnames(tabela)=="Valor")
            
            #tabela2 = tabela
            #tabela2[tabela2[,id] ==  "..",id] <- NA
            #tabela2[,id] <- as.numeric(tabela2[,id])
            tabela[,id] = suppressWarnings(ifelse(tabela[,id]!="..", as.numeric(tabela[,id]),NA))
    }

    assign(t1,tabela)
    rm(tabela)

    return(invisible(eval(as.symbol(t1))))
    
}

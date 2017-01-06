#' A function to extract Sidra series using their API
#' 
#' The different parameters define a table and its dimensions (periods, variables, territorial units and classification) to be consulted. The parameters that define the territorial units can be used more than once, allowing multiple distinct territorial searches with one request. The parameters that define the classes may vary from table to table, with a maximum of 6. Henceforth, a Sidra table ranges between 3 and 9 differente dimension (3 mandatory - periodos, var and terr and 6 optional).
#' @param cod_tabela Sidra series number.
#' @param terr Specifies the desired territorial levels. Defaults to 'n1/1' - Brazil.
#' @keywords sidra
#' @import RCurl rjson
#' @examples
#' ibge=aux_sidra(cod_tabela = 1612, formato = "a", header=TRUE)    



aux_sidra <- function(cod_tabela, periods, variable, territory){
    
    # Código da tabela
    cod_tabela = as.character(cod_tabela)
    
    
    
    # Período
    periods
    
    if (periods == "all"){
        
    } else if (!is.numeric(periods) | length(periods) != 1){
        
        
        
    }
    
    
    # Variável
    variable
    
    
    # Território
    territory <- base::match.arg(territory)
    base::switch(territory,
                 brazil = "n1", 
                 region = "n2", 
                 state = "n3", 
                 municipality = "n6")
    
    #lista = list('brazil' = "n1", region = "n2", state = "n3", municipality = "n6")
    

    
    
    

    
    


    return()
    
}

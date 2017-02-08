pagina = xml2::read_html("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=3653")

anos=pagina %>%
    rvest::html_nodes("#lblPeriodoDisponibilidade")%>%
    rvest::html_text()


variavel=pagina %>%
    rvest::html_nodes("#tabPrincipal")%>%
    rvest::html_text() 

dados2=gsub("\r\n ", "", dados)    
dados2 = trimws(dados2)
dados2 = strsplit(dados2, split = "  ")



secoes=pagina %>%
    rvest::html_nodes("br+ #tabPrincipal span")%>%
    rvest::html_text()

if (!('handles' %in% ls())) { handles <- list() }

handles$renderTableBasic <- function(dat, captionTable='tabla', headerUpper=TRUE, fontSize=8) {
    
    require(knitr)
    require(kableExtra)
    require(dplyr)
    require(huxtable)
    
    # Formato Titulos
    names(dat) <- gsub('_',' ', names(dat))
    
    if (headerUpper ) {
        names(dat) <- toupper(names(dat))
    } else{
        names(dat) <- gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(names(dat)), perl=TRUE)    
    }
    
    if ('TIPO DE ENTIDAD' %in% names(dat)) {
        dat[,'TIPO DE ENTIDAD'] <- gsub('_',' ', dat[,'TIPO DE ENTIDAD'])
    }
    
    
    result <- as_hux(dat) 
    
    result <- result %>%
      set_number_format(5) %>%
      set_bold(row = 1, col = everywhere) %>%
      set_top_border(row = 1, col = everywhere) %>%
      set_bottom_border(row = 1, col = everywhere) %>%
      set_bottom_border(row = nrow(result), col = everywhere)
    
    font_size(result) <- fontSize
    
    return(result)
}

handles$renderTableBasicLatex <- function(dat, captionTable='tabla', headerUpper=TRUE, fontSize=8) {
    
    require(knitr)
    require(kableExtra)
    require(dplyr)
    
    # Formato Titulos
    names(dat) <- gsub('_',' ', names(dat))
    
    if (headerUpper ) {
        names(dat) <- toupper(names(dat))
    } else{
        names(dat) <- gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(names(dat)), perl=TRUE)    
    }
    
    if ('TIPO DE ENTIDAD' %in% names(dat)) {
        dat[,'TIPO DE ENTIDAD'] <- gsub('_',' ', dat[,'TIPO DE ENTIDAD'])
    }
    
    
    result <- 
        kable(dat, "latex", 
              booktabs = TRUE, 
              longtable = TRUE, 
              caption = captionTable, 
              digits = 6,
              format.args = list(big.mark = " ") ) %>%
        kable_styling(latex_options = c("hold_position", "repeat_header"),
                      font_size = fontSize,
                      repeat_header_text = "(Continuación)")
    
    return(result)
}

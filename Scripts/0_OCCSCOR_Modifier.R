add_occsco_class = function(df) {
    if (df$year == 1850 | 1880 | 1910){
        occscore_class = case_when(
            df$occscore <= 30 ~ 'LOW',
            df$occscore <= 54 ~ 'MIDDLE',
            df$occscore <= 80 ~ 'HIGH',
            TRUE ~ NA_character_
    )
    return(occscore_class)
    }
    else {
    occsco_class = NA_character_
    return(occscore_class)
    }
}

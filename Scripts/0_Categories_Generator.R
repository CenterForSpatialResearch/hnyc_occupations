add_occ_cat_label = function(df) {
  occ_cat_label = case_when(
    df$occ <= 12 ~ "Agriculture",
    df$occ <= 58 ~ "Professional and Personal Services",
    df$occ <= 129 ~ "Trade and Transportation",
    df$occ <= 291 ~ "Manufacturing, Mechanical, and Mining Industries",
    df$occ <= 999 ~ "Non-Occupational Response",
    TRUE ~ NA_character_
  )
  return(occ_cat_label)
}

add_occ1950_cat_label = function(df) {
  occ1950_cat_label = case_when(
    df$occ1950 <= 99 ~ "Professional, Technical",
    df$occ1950 <= 123 ~ "Farmers",
    df$occ1950 <= 290 ~ "Managers, Officials, and Proprietors",
    df$occ1950 <= 390 ~ "Clerical and Kindred",
    df$occ1950 <= 490 ~ "Sales workers",
    df$occ1950 <= 595 ~ "Craftsmen",
    df$occ1950 <= 690 ~ "Operatives",
    df$occ1950 <= 720 ~ "Service Workers (private household)",
    df$occ1950 <= 790 ~ "Service Workers (not household)",
    df$occ1950 <= 840 ~ "Farm Laborers",
    df$occ1950 <= 970 ~ "Laborers",
    df$occ1950 <= 979 ~ "Not yet classified",
    df$occ1950 <= 995 ~ "Non-occupational response",
    df$occ1950 <= 997 ~ "Occupation missing/unknown",
    df$occ1950 <= 999 ~ "N/A (blank)",
    TRUE ~ NA_character_
  )
  return(occ1950_cat_label)
}

add_ind1950_cat_label = function(df) {
  ind1950_cat_label = case_when(
    df$ind1950 <= 0 ~ "N/A or none reported",
    df$ind1950 <= 126 ~ "Agriculture, Forestry, and Fishing",
    df$ind1950 <= 239 ~ "Mining",
    df$ind1950 <= 246 ~ "Construction",
    df$ind1950 <= 499 ~ "Manufacturing",
    df$ind1950 <= 598 ~ "Transportation, Communication, and Other Utilities",
    df$ind1950 <= 699 ~ "Wholesale and Retail Trade",
    df$ind1950 <= 756 ~ "Finance, Insurance, and Real Estate",
    df$ind1950 <= 817 ~ "Business and Repair Services",
    df$ind1950 <= 849 ~ "Personal services",
    df$ind1950 <= 859 ~ "Entertainment and Recreation Services",
    df$ind1950 <= 899 ~ "Professional and Related Services",
    df$ind1950 <= 936 ~ "Public Administration",
    df$ind1950 <= 946 ~ "Public Administration, level not specified",
    df$ind1950 <= 976 ~ "Common or general laborer",
    df$ind1950 <= 979 ~ "Not yet specified",
    df$ind1950 <= 980 ~ "Unpaid domestic work",
    df$ind1950 <= 982 ~ "Housework at home",
    df$ind1950 <= 983 ~ "School response (students, etc.)",
    df$ind1950 <= 984 ~ "Retired",
    df$ind1950 <= 986 ~ "Sick/disabled",
    df$ind1950 <= 987 ~ "Institution response",
    df$ind1950 <= 999 ~ "Other",
    TRUE ~ NA_character_
  )
  return(ind1950_cat_label)
}

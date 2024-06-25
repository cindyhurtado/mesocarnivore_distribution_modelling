# This script is a little exploration of what Cindy wants for the 'DNA_columbian_3k' table.

# The hope is to make this into a list of identical tables.

head(DNA_columbian_3k)


# We need a row for unique animal IDs, columns will be unique station IDs filled by a YES (1) or NO (0)
# for detections. Note that this is for ALL occasions and thus has some duplicated data in it.
# It's a preliminary check of the code!
DNA_columbian_3k %>% 
  dplyr::ungroup() %>% 
  sf::st_drop_geometry() %>% 
  dplyr::filter(occasion == 1, Animal_ID == '1011') %>% 
  dplyr::filter(!duplicated(Animal_ID)) %>% 
  dplyr::filter(!is.na(Animal_ID)) %>% 
  dplyr::select(Animal_ID, Station_ID, Species) %>% 
  tidyr::pivot_wider(names_from = Station_ID, values_from = Species,names_prefix = 'st_') %>% 
  # Depending on the species that fills in each cell, let's replace Pekania p. with 1, and everything 
  # else we'll replace with 0.
  dplyr::mutate(across(-Animal_ID, \(x) dplyr::case_when(
    is.na(x) ~ 0,
    x == 'Pekania pennanti' ~ 1,
    T ~ 0)))

# This is the full 'frmaewokr' of the matrix - it has ALL stations and animal IDs.
matrix_frame = DNA_columbian_3k %>% 
  ungroup() %>% 
  sf::st_drop_geometry() %>% 
  dplyr::mutate(st_filler = 'goop') %>% 
  dplyr::select(Station_ID, Animal_ID, st_filler) %>%
  dplyr::distinct() %>% 
  tidyr::pivot_wider(names_from = Station_ID, values_from = st_filler, names_prefix = 'st_') %>% 
  dplyr::filter(!duplicated(Animal_ID)) %>%
  dplyr::filter(!is.na(Animal_ID))

# tbl_to_join = matrix_frame %>% 
#   dplyr::select(-all_of(stations_already_in_this_occasion)) %>% 
#   dplyr::mutate(across(-Animal_ID, \(x) x = NA))

# For each 'occasion', make a table showing 1's and 0's by unique animal ID and stations as columns.
tbls_by_occasion_list = 1:9 %>% 
  map( ~ {
    dat_for_occ = DNA_columbian_3k %>% 
      dplyr::filter(occasion == .x) %>% 
      dplyr::ungroup() %>% 
      sf::st_drop_geometry() %>% 
      dplyr::filter(!duplicated(Animal_ID)) %>% 
      dplyr::filter(!is.na(Animal_ID)) %>% 
      dplyr::select(Animal_ID, Station_ID, Species) %>% 
      tidyr::pivot_wider(names_from = Station_ID, values_from = Species,names_prefix = 'st_') %>% 
      # Depending on the species that fills in each cell, let's replace Pekania p. with 1, and everything 
      # else we'll replace with 0.
      dplyr::mutate(across(-Animal_ID, \(x) dplyr::case_when(
        is.na(x) ~ 0,
        x == 'Pekania pennanti' ~ 1,
        T ~ 0)))
    
    stations_already_in_this_occasion = names(dat_for_occ[,-1])
    
    tbl_to_join = matrix_frame %>% 
      dplyr::select(-all_of(stations_already_in_this_occasion)) %>% 
      dplyr::mutate(across(-Animal_ID, \(x) x = 0))
    
    dat_for_occ %>% 
      dplyr::left_join(tbl_to_join) %>% 
      dplyr::bind_rows(tbl_to_join[!tbl_to_join$Animal_ID %in% dat_for_occ$Animal_ID,]) %>% 
      dplyr::mutate(across(-Animal_ID, \(x) ifelse(is.na(x), 0, x)))
    
  })

names(tbls_by_occasion_list) <- paste0('occ_',c(1:9))

tbls_by_occasion_list$occ_1
# tbls_by_occasion_list$occ_8
tbls_by_occasion_list$occ_9

# Let's reorder the stations for this output based on the order in the 'matrix_frame'
# we made; the same thing for the order of rows by Animal_ID.
all_dat = tbls_by_occasion_list %>% 
  dplyr::bind_rows(.id = 'occasion') %>% 
  dplyr::arrange(occasion,Animal_ID) %>% 
  dplyr::select(occasion, Animal_ID, names(matrix_frame)[-1])

# Please use all_dat going forward!
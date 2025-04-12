## read the data from dryad (2025-04-11)
raw_data <- read.csv(rdryad::dryad_download("10.5061/dryad.kb76qj8")[[1]][5])


str(raw_data)

## save the raw data:
write.csv(raw_data, "data-raw/data_vucic_pestic_et_al_2010_j_anim_ecol.csv", row.names = FALSE)

data_vucic_pestic_et_al_2010_j_anim_ecol <- data.frame(
  n_initial = raw_data$N0,
  n_eaten = raw_data$Neaten
)

usethis::use_data(data_vucic_pestic_et_al_2010_j_anim_ecol, overwrite = TRUE)

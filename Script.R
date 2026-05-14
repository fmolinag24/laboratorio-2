
library(pacman)
p_load(tidyverse, sampling, haven)

ipm <- read.csv("HOGARES(DEPARTAMENTAL)2025.csv")

n_total <- 30000 # Tamaño muestra

# Tamaños por estrato (afijación proporcional)
tam_estrato <- ipm |> 
  count(DEPARTAMENTO) |> 
  mutate(nh = round(n / sum(n) * n_total))

# Diseño estratificado
set.seed(1234)
muestra <- strata(data = ipm,
                  stratanames = "DEPARTAMENTO",
                  size = tam_estrato$nh,
                  method = "srswor")

df_ipm <- getdata(ipm, muestra)
#preparación y agrupación de datos por departamento
 g_ipm<- df_ipm |> 
  select(DEPARTAMENTO, , analfabetismo, logro_educativo, acueducto, alcantarillado, pisos, paredes, hacinamiento, empleo_formal,desempleo_larga_duracion) |> 
  group_by(DEPARTAMENTO) |> 
  summarise(across(where(is.numeric), ~mean(., na.rm = T))) |>
  column_to_rownames("DEPARTAMENTO") 



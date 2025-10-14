library(tidyverse)
library(RSQLite)

fname <- here::here("inst", "raw", "KCB_SIGNGU_DATA5_23_202507.csv")

readr::read_csv(fname) |> 
  janitor::clean_names() -> cty_income

cty_income |> 
  select(base_ym, cty_cd = signgu_cd, incm_avg = avrg_income_price) |> 
  mutate(cty_cd = as.character(cty_cd)) |> 
  left_join(bitSpatial::cty |> select(-base_ym), by = "cty_cd") |> 
  select(base_ym, mega_cd, mega_nm, cty_cd, cty_nm, incm_avg) |> 
  mutate(incm_avg = incm_avg * 1000) |> 
  mutate(cret_dt = Sys.time()) |> 
  mutate(cret_nm = "bitPublish") |> 
  mutate(mdfy_dt = NA) |> 
  mutate(mdfy_nm = NA) -> income_cty2

names(cty_income) <- toupper(names(cty_income))

con <- dbConnect(SQLite(), here::here("inst", "dbms", "public.sqlite"))
dbWriteTable(con, "TB_KCBICM_CTY", income_cty2, overwrite = TRUE)
# dbGetQuery(con, "select * from TB_KCBICM_CTY limit 5;")
dbDisconnect(con)

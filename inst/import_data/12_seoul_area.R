################################################################################
## 12_seoul_area.R
## 서울시 상권분석서비스(영역-행정동)
## https://data.seoul.go.kr/dataList/OA-22175/S/1/datasetView.do
################################################################################
################################################################################
## 01. Prepare resources
################################################################################
##==============================================================================
## 01.01. Load R libraries
##==============================================================================
library(bitSpatial)
library(jsonlite)
library(tidyverse)
library(RSQLite)

##==============================================================================
## 01.02. Global variables
##==============================================================================
## 서울시열린데이터광장 오픈 API 인증키
api_key <- "41764f65696264623633644a704a54"
service_id <- "TbgisAdstrdRelmW"

con <- dbConnect(SQLite(), here::here("inst", "dbms", "public.sqlite"))

################################################################################
## 02. 서울시 상권분석서비스(영역-행정동)
################################################################################
##==============================================================================
## 02.01. 데이터 읽기
##==============================================================================
api_url <- glue::glue("http://openapi.seoul.go.kr:8088/{api_key}/json/{service_id}/1/5/")
df_init <- fromJSON(api_url)

total_cnt <- df_init[[service_id]]$list_total_count
patch_cnt <- total_cnt %/% 1000

df_init <- df_init[[service_id]]$row

from_patch <- c(1, seq(1, patch_cnt) * 1000 + 1)
to_patch <- c(seq(1, patch_cnt) * 1000, total_cnt) 

cli::cli_alert_info("Total records are {total_cnt}.")

purrr::map2_df(from_patch, to_patch, function(from, to) {
  api_url <- glue::glue("http://openapi.seoul.go.kr:8088/{api_key}/json/{service_id}/{formatC(from, format = 'd')}/{formatC(to, format = 'd')}/")
  
  Sys.sleep(ifelse(patch_cnt > 20, 0.5, 0))
  df_stats <- fromJSON(api_url)
  
  cli::cli_alert_info("Patch data from {from} to {to} records.")
  
  df_stats[[service_id]]$row
}) -> df_response

##==============================================================================
## 02.02. 행정구역 정보 현행화
##==============================================================================
# 2022년 12월 23일 : 강남구 일원2동이 개포3동으로 동명 변경
# 2021년 7월  1일 : 강남구 상일동이 상일1동으로 동명 변경
# 2021년 7월  1일 : 강남구 강일동의 일부가 상일2동으로 분리
#                   강동구 강일동 -> 상일2동 대체, 결측치로 놓는 것이 아닌 인근 데이터 활용

admi_temp <- df_response |> 
  filter(ADSTRD_CD %in% "11740515") |> 
  mutate(ADSTRD_CD = "11740526") |>
  mutate(ADSTRD_NM = "상일2동") |> 
  union_all(
    df_response |> 
      mutate(ADSTRD_CD = ifelse(ADSTRD_CD == "11680740", "11680675", ADSTRD_CD)) |> 
      mutate(ADSTRD_NM = ifelse(ADSTRD_CD == "11680740", "개포3동", ADSTRD_NM)) |>  
      mutate(ADSTRD_NM = ifelse(ADSTRD_CD == "11740520", "상일1동", ADSTRD_NM)) |>  
      mutate(ADSTRD_CD = ifelse(ADSTRD_CD == "11740520", "11740525", ADSTRD_CD))
  ) |> 
  rename(admi_cd = ADSTRD_CD) |> 
  distinct()


##==============================================================================
## 02.03. 광역시도 시군구 정보 가져오기
##==============================================================================
admi_seoul <- admi |> 
  filter(mega_cd == "11") |> 
  select(mega_cd, mega_nm, cty_cd, cty_nm, admi_cd, admi_nm) |> 
  as.data.frame()

admi_seoul <- admi_seoul |> 
  left_join(admi_temp, by = c("admi_cd")) |> 
  select(mega_cd, mega_nm, cty_cd, cty_nm, admi_cd, admi_nm, 
         XCNTS_VALUE:RELM_AR) 


##==============================================================================
## 02.04. Import from data frame to DBMS
##==============================================================================

pos <- bitSpatial::convert_projection(
  admi_seoul$XCNTS_VALUE, 
  admi_seoul$YDNTS_VALUE, 
  from = "EPSG:5181", 
  to = "WGS84")

admi_seoul <- admi_seoul |> 
  mutate(LON = pos$lon) |> 
  mutate(LAT = pos$lat) |> 
  mutate(cret_dt = Sys.time()) |> 
  mutate(cret_nm = "bitPublish") |> 
  mutate(mdfy_dt = as.POSIXct(NA)) |> 
  mutate(mdfy_nm = as.character(NA)) |> 
  rename_with(function(x) {stringr::str_replace_all(x, "XCNTS_VALUE", "LON_5181")}, contains("XCNTS_VALUE")) |> 
  rename_with(function(x) {stringr::str_replace_all(x, "YDNTS_VALUE", "LAT_5181")}, contains("YDNTS_VALUE")) |> 
  rename_with(function(x) {stringr::str_replace_all(x, "_AR$", "_AREA")}, ends_with("_AR")) |> 
  rename_with(toupper) |> 
  tibble::as_tibble()

dbWriteTable(con, "TB_SEOUL_REALM_ADMI", admi_seoul, overwrite = TRUE)
# dbGetQuery(con, "select * from TB_SEOUL_REALM_ADMI limit 5;")


################################################################################
## 03. 서울시 상권분석서비스(소득소비-구 레벨)
################################################################################
##==============================================================================
## 03.01. Aggregation to cty level
##==============================================================================
admi_seoul |> 
  select(MEGA_CD:CTY_NM, RELM_AREA) |> 
  group_by(MEGA_CD, MEGA_NM, CTY_CD, CTY_NM) |>
  summarise_all(function(x) round(sum(x, na.rm = TRUE))) |> 
  mutate(cret_dt = Sys.time()) |> 
  mutate(cret_nm = "bitPublish") |> 
  mutate(mdfy_dt = as.POSIXct(NA)) |> 
  mutate(mdfy_nm = as.character(NA)) |> 
  rename_with(toupper) -> admi_seoul_cty

##==============================================================================
## 03.02. Import from data frame to DBMS
##==============================================================================
dbWriteTable(con, "TB_SEOUL_REALM_CTY", admi_seoul_cty, overwrite = TRUE)
# dbGetQuery(con, "select * from TB_SEOUL_REALM_CTY limit 5;")


################################################################################
## 04. Export data from tibbles to table of DBMS
################################################################################
##==============================================================================
## 04.01. Connect DBMS
##==============================================================================
dbDisconnect(con)


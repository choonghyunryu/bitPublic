################################################################################
## 02.public_income.R
##  - 국민연금공단_자격 시구신고 평균소득월액
##    (https://www.data.go.kr/data/3046077/fileData.do)
##  - KCB 전국 시군구 단위 평균 소득
##    (https://www.bigdata-culture.kr/bigdata/user/data_market/detail.do?id=75ab3e79-6f9b-4d80-934b-07746d384096)
################################################################################
################################################################################
## 01. Prepare resources
################################################################################
##==============================================================================
## 01.01. Load R libraries
##==============================================================================
library(tidyverse)
library(jsonlite)
library(RSQLite)

##==============================================================================
## 01.02. Global variables
##==============================================================================
## 공공데이터포털 오픈 API 인증키
api_key <- "1JvhZzQWcrByiVINYsTQxlzejyyzPYOSjpND4RkRjMGupUm7pc8mTfGKqFREPDv2tP48Z64NSaj6MVJV3RH2Jg%3D%3D"
con <- dbConnect(SQLite(), here::here("inst", "dbms", "public.sqlite"))

################################################################################
## 02. 국민연금공단_자격 시구신고 평균소득월액
################################################################################
##==============================================================================
## 02.01. 데이터 읽기
##==============================================================================
## https://www.data.go.kr/data/3046077/fileData.do
## 국민연금공단_자격 시구신고 평균소득월액

page <- "1"
per_page <- "10"
data_type <- "json"

api_url <- glue::glue("https://api.odcloud.kr/api/3046077/v1/uddi:838b3f51-3221-44aa-b125-d3fe9ae04937?page={page}&perPage={per_page}&serviceKey={api_key}&dataType={data_type}")
df_stats <- fromJSON(api_url)

per_page <- df_stats$matchCount

api_url <- glue::glue("https://api.odcloud.kr/api/3046077/v1/uddi:838b3f51-3221-44aa-b125-d3fe9ae04937?page={page}&perPage={per_page}&serviceKey={api_key}&dataType={data_type}")
df_stats <- fromJSON(api_url)

income_cty <- df_stats$data

income_cty <- income_cty |> 
  mutate(mega_nm = ifelse(stringr::str_detect(시군구, "서울특별시"), "서울특별시", NA)) |> 
  mutate(mega_nm = ifelse(stringr::str_detect(시군구, "부산광역시"), "부산광역시", mega_nm)) |> 
  mutate(mega_nm = ifelse(stringr::str_detect(시군구, "인천광역시"), "인천광역시", mega_nm)) |> 
  mutate(mega_nm = ifelse(stringr::str_detect(시군구, "대구광역시"), "대구광역시", mega_nm)) |> 
  mutate(mega_nm = ifelse(stringr::str_detect(시군구, "광주광역시"), "광주광역시", mega_nm)) |> 
  mutate(mega_nm = ifelse(stringr::str_detect(시군구, "대전광역시"), "대전광역시", mega_nm)) |> 
  mutate(mega_nm = ifelse(stringr::str_detect(시군구, "울산광역시"), "울산광역시", mega_nm)) |> 
  mutate(mega_nm = ifelse(stringr::str_detect(시군구, "경기도"), "경기도", mega_nm)) |> 
  mutate(mega_nm = ifelse(stringr::str_detect(시군구, "강원특별자치도"), "강원특별자치도", mega_nm)) |> 
  mutate(mega_nm = ifelse(stringr::str_detect(시군구, "충청북도"), "충청북도", mega_nm)) |> 
  mutate(mega_nm = ifelse(stringr::str_detect(시군구, "충청남도"), "충청남도", mega_nm)) |> 
  mutate(mega_nm = ifelse(stringr::str_detect(시군구, "전북특별자치도"), "전북특별자치도", mega_nm)) |> 
  mutate(mega_nm = ifelse(stringr::str_detect(시군구, "전라남도"), "전라남도", mega_nm)) |> 
  mutate(mega_nm = ifelse(stringr::str_detect(시군구, "경상북도"), "경상북도", mega_nm)) |> 
  mutate(mega_nm = ifelse(stringr::str_detect(시군구, "경상남도"), "경상남도", mega_nm)) |> 
  mutate(mega_nm = ifelse(stringr::str_detect(시군구, "제주특별자치도"), "제주특별자치도", mega_nm)) |> 
  mutate(mega_nm = ifelse(stringr::str_detect(시군구, "세종특별자치시"), "세종특별자치시", mega_nm)) |> 
  mutate(cty_nm = stringr::str_remove(시군구, mega_nm)) |> 
  mutate(cty_nm = ifelse(cty_nm == "", "세종시", cty_nm)) |> 
  mutate(cty_nm = ifelse(mega_nm %in% "인천광역시" & cty_nm %in% "남구", "미추홀구", cty_nm)) 


##==============================================================================
## 02.02. 공공데이터의 특정 시군구 코드 오류 보정
##==============================================================================
income_cty <- income_cty |> 
  bind_rows(
    income_cty |> 
      filter(mega_nm == "경기도" & cty_nm == "고양시") |> 
      mutate(cty_nm = "고양시 일산동구")
  ) |> 
  bind_rows(
    income_cty |> 
      filter(mega_nm == "경기도" & cty_nm == "고양시") |> 
      mutate(cty_nm = "고양시 덕양구")
  ) |>   
  mutate(cty_nm = ifelse(mega_nm == "경기도" & cty_nm == "고양시", "고양시 일산서구", cty_nm)) |>
  bind_rows(
    income_cty |> 
      filter(mega_nm == "경기도" & cty_nm == "성남시") |> 
      mutate(cty_nm = "성남시 수정구")
  ) |>
  bind_rows(
    income_cty |> 
      filter(mega_nm == "경기도" & cty_nm == "성남시") |> 
      mutate(cty_nm = "성남시 중원구")
  ) |>
  mutate(cty_nm = ifelse(mega_nm == "경기도" & cty_nm == "성남시", "성남시 분당구", cty_nm)) |>
  bind_rows(
    income_cty |> 
      filter(mega_nm == "경기도" & cty_nm == "안산시") |> 
      mutate(cty_nm = "안산시 상록구")
  ) |>
  mutate(cty_nm = ifelse(mega_nm == "경기도" & cty_nm == "안산시", "안산시 단원구", cty_nm)) |>
  bind_rows(
    income_cty |> 
      filter(mega_nm == "경기도" & cty_nm == "용인시") |> 
      mutate(cty_nm = "용인시 처인구")
  ) |>
  bind_rows(
    income_cty |> 
      filter(mega_nm == "경기도" & cty_nm == "용인시") |> 
      mutate(cty_nm = "용인시 기흥구")
  ) |>
  mutate(cty_nm = ifelse(mega_nm == "경기도" & cty_nm == "용인시", "용인시 수지구", cty_nm)) |>
  bind_rows(
    income_cty |> 
      filter(mega_nm == "경기도" & cty_nm == "부천시") |> 
      mutate(cty_nm = "부천시 오정구")
  ) |>
  bind_rows(
    income_cty |> 
      filter(mega_nm == "경기도" & cty_nm == "부천시") |> 
      mutate(cty_nm = "부천시 원미구")
  ) |>
  mutate(cty_nm = ifelse(mega_nm == "경기도" & cty_nm == "부천시", "부천시 소사구", cty_nm)) |>
  bind_rows(
    income_cty |> 
      filter(mega_nm == "경기도" & cty_nm == "안양시") |> 
      mutate(cty_nm = "안양시 만안구")
  ) |>
  mutate(cty_nm = ifelse(mega_nm == "경기도" & cty_nm == "안양시", "안양시 동안구", cty_nm)) |>
  bind_rows(
    income_cty |> 
      filter(mega_nm == "경기도" & cty_nm == "수원시") |> 
      mutate(cty_nm = "수원시 장안구")
  ) |>
  bind_rows(
    income_cty |> 
      filter(mega_nm == "경기도" & cty_nm == "수원시") |> 
      mutate(cty_nm = "수원시 권선구")
  ) |>
  bind_rows(
    income_cty |> 
      filter(mega_nm == "경기도" & cty_nm == "수원시") |> 
      mutate(cty_nm = "수원시 팔달구")
  ) |>
  mutate(cty_nm = ifelse(mega_nm == "경기도" & cty_nm == "수원시", "수원시 영통구", cty_nm)) |> 
  bind_rows(
    income_cty |> 
      filter(mega_nm == "충청북도" & cty_nm == "청주시") |> 
      mutate(cty_nm = "청주시 상당구")
  ) |>
  bind_rows(
    income_cty |> 
      filter(mega_nm == "충청북도" & cty_nm == "청주시") |> 
      mutate(cty_nm = "청주시 서원구")
  ) |>
  bind_rows(
    income_cty |> 
      filter(mega_nm == "충청북도" & cty_nm == "청주시") |> 
      mutate(cty_nm = "청주시 흥덕구")
  ) |>
  mutate(cty_nm = ifelse(mega_nm == "충청북도" & cty_nm == "청주시", "청주시 청원구", cty_nm)) |>
  bind_rows(
    income_cty |> 
      filter(mega_nm == "충청남도" & cty_nm == "천안시") |> 
      mutate(cty_nm = "천안시 동남구")
  ) |>
  mutate(cty_nm = ifelse(mega_nm == "충청남도" & cty_nm == "천안시", "천안시 서북구", cty_nm)) |>
  bind_rows(
    income_cty |> 
      filter(mega_nm == "전북특별자치도" & cty_nm == "전주시") |> 
      mutate(cty_nm = "전주시 완산구")
  ) |>
  mutate(cty_nm = ifelse(mega_nm == "전북특별자치도" & cty_nm == "전주시", "전주시 덕진구", cty_nm)) |> 
  bind_rows(
    income_cty |> 
      filter(mega_nm == "경상남도" & cty_nm == "창원시") |> 
      mutate(cty_nm = "창원시 의창구")
  ) |>
  bind_rows(
    income_cty |> 
      filter(mega_nm == "경상남도" & cty_nm == "창원시") |> 
      mutate(cty_nm = "창원시 성산구")
  ) |>
  bind_rows(
    income_cty |> 
      filter(mega_nm == "경상남도" & cty_nm == "창원시") |> 
      mutate(cty_nm = "창원시 마산합포구")
  ) |>
  bind_rows(
    income_cty |> 
      filter(mega_nm == "경상남도" & cty_nm == "창원시") |> 
      mutate(cty_nm = "창원시 마산회원구")
  ) |>
  mutate(cty_nm = ifelse(mega_nm == "경상남도" & cty_nm == "창원시", "창원시 진해구", cty_nm)) |> 
  bind_rows(
    income_cty |> 
      filter(mega_nm == "경상북도" & cty_nm == "포항시") |> 
      mutate(cty_nm = "포항시 남구")
  ) |>
  mutate(cty_nm = ifelse(mega_nm == "경상북도" & cty_nm == "포항시", "포항시 북구", cty_nm))


##==============================================================================
## 02.03. 행정구역코드 병합 및 컬럼명 변경
##==============================================================================
# 
income_cty <- income_cty |> 
  left_join(
    bitSpatial::cty |> select(mega_cd, mega_nm, cty_cd, cty_nm), by = c("mega_nm", "cty_nm")
  ) |> 
  select(base_ym = 기준년월, mega_cd, mega_nm, cty_cd, cty_nm, incm_avg = 평균소득월액)


##==============================================================================
## 02.04. Import from data frame to DBMS
##==============================================================================
##------------------------------------------------------------------------------
## 02.05.01. 읍면동 레벨
##------------------------------------------------------------------------------
income_cty <- income_cty |> 
  mutate(base_ym = stringr::str_remove(base_ym, "-")) |> 
  mutate(cret_dt = Sys.time()) |> 
  mutate(cret_nm = "bitPublish") |> 
  mutate(mdfy_dt = as.POSIXct(NA)) |> 
  mutate(mdfy_nm = as.character(NA)) |> 
  rename_with(toupper)

dbWriteTable(con, "TB_INCOME_CTY", income_cty, overwrite = TRUE)
# dbGetQuery(con, "select * from TB_INCOME_CTY limit 5;")



################################################################################
## 03. 전국 시군구 단위 평균 소득
################################################################################
##==============================================================================
## 03.01. Import data from files
##==============================================================================
## KCB 전국 시군구 단위 평균 소득
##  (https://www.bigdata-culture.kr/bigdata/user/data_market/detail.do?id=75ab3e79-6f9b-4d80-934b-07746d384096)

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
  mutate(mdfy_dt = as.POSIXct(NA)) |> 
  mutate(mdfy_nm = as.character(NA)) |> 
  rename_with(toupper) -> income_cty2


##==============================================================================
## 03.02. Import from data frame to DBMS
##==============================================================================

dbWriteTable(con, "TB_KCBICM_CTY", income_cty2, overwrite = TRUE)
# dbGetQuery(con, "select * from TB_KCBICM_CTY limit 5;")


################################################################################
## 04. Export data from tibbles to table of DBMS
################################################################################
##==============================================================================
## 04.01. Connect DBMS
##==============================================================================
dbDisconnect(con)


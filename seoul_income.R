## 서울시 상권분석서비스(소득소비-행정동)
## https://data.seoul.go.kr/dataList/OA-22166/S/1/datasetView.do?tab=A

# STDR_YYQU_CD	기준_년분기_코드
# ADSTRD_CD	행정동_코드
# ADSTRD_CD_NM	행정동_코드_명
# MT_AVRG_INCOME_AMT	월_평균_소득_금액
# INCOME_SCTN_CD	소득_구간_코드
# EXPNDTR_TOTAMT	지출_총금액
# FDSTFFS_EXPNDTR_TOTAMT	식료품_지출_총금액
# CLTHS_FTWR_EXPNDTR_TOTAMT	의류_신발_지출_총금액
# LVSPL_EXPNDTR_TOTAMT	생활용품_지출_총금액
# MCP_EXPNDTR_TOTAMT	의료비_지출_총금액
# TRNSPORT_EXPNDTR_TOTAMT	교통_지출_총금액
# EDC_EXPNDTR_TOTAMT	교육_지출_총금액
# PLESR_EXPNDTR_TOTAMT	유흥_지출_총금액
# LSR_CLTUR_EXPNDTR_TOTAMT	여가_문화_지출_총금액
# ETC_EXPNDTR_TOTAMT	기타_지출_총금액
# FD_EXPNDTR_TOTAMT	음식_지출_총금액

library(dplyr)
library(bitSpatial)
library(jsonlite)

api_key <- "41764f65696264623633644a704a54"

api_url <- glue::glue("http://openapi.seoul.go.kr:8088/{api_key}/json/VwsmAdstrdNcmCnsmpW/1/5/")
df_init <- fromJSON(api_url)

total_cnt <- df_init$VwsmAdstrdNcmCnsmpW$list_total_count
patch_cnt <- total_cnt %/% 1000

df_init <- df_init$VwsmAdstrdNcmCnsmpW$row

from_patch <- c(1, seq(1, 11) * 1000 + 1)
to_patch <- c(seq(1, 11) * 1000, total_cnt) 

purrr::map2_df(from_patch, to_patch, function(from, to) {
  api_url <- glue::glue("http://openapi.seoul.go.kr:8088/{api_key}/json/VwsmAdstrdNcmCnsmpW/{from}/{to}/")
  df_stats <- fromJSON(api_url)
  df_stats$VwsmAdstrdNcmCnsmpW$row
}) -> df_income

admi_seoul <- admi |> 
  filter(mega_cd == "11")

# 2022년 12월 23일 : 강남구 일원2동이 개포3동으로 동명 변경
# 2021년 7월  1일 : 강남구 상일동이 상일1동으로 동명 변경
# 2021년 7월  1일 : 강남구 강일동의 일부가 상일2동으로 분리
#                   강동구 강일동 -> 상일2동 대체, 결측치로 놓는 것이 아닌 인근 데이터 활용

admi_temp <- df_income |> 
  filter(STDR_YYQU_CD %in% "20243") |> 
  filter(ADSTRD_CD %in% "11740515") |> 
  mutate(ADSTRD_CD = "11740526") |>
  mutate(ADSTRD_CD_NM = "상일2동") |> 
  union_all(
    df_income |> 
      filter(STDR_YYQU_CD %in% "20243") |> 
      mutate(ADSTRD_CD = ifelse(ADSTRD_CD == "11680740", "11680675", ADSTRD_CD)) |> 
      mutate(ADSTRD_CD_NM = ifelse(ADSTRD_CD == "11680740", "개포3동", ADSTRD_CD_NM)) |>  
      mutate(ADSTRD_CD_NM = ifelse(ADSTRD_CD == "11740520", "상일1동", ADSTRD_CD_NM)) |>  
      mutate(ADSTRD_CD = ifelse(ADSTRD_CD == "11740520", "11740525", ADSTRD_CD))
  ) |> 
  rename(admi_cd = ADSTRD_CD)

names(admi_temp) <- names(admi_temp) |> 
  tolower()

admi_seoul <- admi_seoul |> 
  left_join(admi_temp, by = c("admi_cd")) |> 
  select(mega_cd, mega_nm, cty_cd, cty_nm, admi_cd, admi_nm, land_area:store_cnt_service, 
         mt_avrg_income_amt:fd_expndtr_totamt) 




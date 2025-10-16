################################################################################
## 02_nhis_beneficiaries_age_gender.R
## 시군구별 연령별 성별 의료보장 적용인구 현황
## https://www.nhis.or.kr/nhis/together/wbhaec06900m01.do
################################################################################
################################################################################
## 01. Prepare resources
################################################################################
##==============================================================================
## 01.01. Load R libraries
##==============================================================================
library(tidyverse)
library(tabulapdf)
library(RSQLite)

##==============================================================================
## 01.02. Global variables
##==============================================================================
# optional: set memory for Java
options(java.parameters = "-Xmx50m")

pdf_stat <- here::here("inst", "raw", "nhis", "2023_지역별의료이용통계연보.pdf")

con <- dbConnect(SQLite(), here::here("inst", "dbms", "public.sqlite"))

##==============================================================================
## 01.03. User-defined functions
##==============================================================================
##------------------------------------------------------------------------------
## 01.03.01. 시군구별 적용인구 테이블 추출 함수
##------------------------------------------------------------------------------
get_tab2 <- function(x, type = c("left", "right")) {
  if (type == "left") {
    tmp <- x |> 
      mutate_all(~ str_remove_all(., ",")) |> 
      mutate_all(~ na_if(., "-")) |> 
      mutate(X11 = str_remove_all(X1, "[0-9,]+| ")) |> 
      mutate(X12 = str_remove_all(X1, " ") |> str_extract_all("[0-9]+") |> map_chr(~ .[1])) |> 
      select(X11, X12, 2:(ncol(x)))
    
    idx_na2 <- which(is.na(tmp[, 2][[1]]))
    
    if (length(idx_na2) > 0) {
      tmp[idx_na2, 2][[1]] <- tmp[idx_na2, 3][[1]]
      tmp[idx_na2, 3][[1]] <- NA
    }
    
  } else {
    tmp <- x |> 
      mutate_all(~ str_remove_all(., ",")) |> 
      mutate_all(~ na_if(., "-"))
  }
  
  idx_min <- seq(tmp) |> 
    purrr::map_int(function(x) {
      apply(tmp[, x], 1, function(x) length(str_split(x, " ")[[1]])) |> min()
    })
  
  idx_max <- seq(tmp) |> 
    purrr::map_int(function(x) {
      apply(tmp[, x], 1, function(x) length(str_split(x, " ")[[1]])) |> max()
    })
  
  idx_diff <- which(!idx_min == idx_max)
  idx_na <- idx_diff - 1
  
  value_na <- idx_na |> 
    purrr::walk(function(x) {
      tmp_na <- ifelse(is.na(tmp[, x][[1]]), 
                       str_split(tmp[, x + 1][[1]], " ") |> map_chr(~ .[1]), 
                       tmp[, x][[1]])
      
      tmp[, x + 1] <<- ifelse(is.na(tmp[, x][[1]]),
                              str_remove(tmp[, x + 1][[1]], "[0-9]+ "),
                              tmp[, x + 1][[1]])
      
      tmp[, x] <<- tmp_na
    })
  
  
  idx_keep <- tmp |> mutate_all(complete.cases) |> 
    summarise_all(sum, na.rm = TRUE) |> 
    as.vector() |> 
    unlist() |> 
    (function(x) x > 0)() 
  
  tmp <- tmp[, idx_keep]
  
  idx_length <- seq(tmp) |> 
    purrr::map_int(function(x) {
      apply(tmp[, x], 1, function(x) length(str_split(x, " ")[[1]])) |> max()
    })
  
  if (type == "left") {
    suppressWarnings({
      tmp <- seq(tmp) |> 
        purrr::map_dfc(function(x) {
          values <- apply(tmp[, x], 1, function(y) {
            value <- str_split(y, " ")[[1]]
            length(value) <- idx_length[x]
            
            value
          })
          
          if (idx_length[x] > 1) {
            values <- data.frame(t(values))
          } else {
            values <- data.frame(values)
          }
          values
        }) |> 
        mutate_all(~ na_if(., "-")) |> 
        select_if(~ !all(is.na(.)))
    })
    
    names(tmp) <- c("cty_nm", "medipop_cnt", "medipop_00_04_cnt", 
                    "medipop_05_09_cnt", "medipop_10_14_cnt", 
                    "medipop_15_19_cnt", "medipop_20_24_cnt", 
                    "medipop_25_29_cnt", "medipop_30_34_cnt",
                    "medipop_35_39_cnt")
    tmp <- tmp |> 
      mutate_at(vars(-cty_nm), as.numeric)
  } else {
    suppressWarnings({
      tmp <- seq(tmp) |> 
        purrr::map_dfc(function(x) {
          values <- apply(tmp[, x], 1, function(y) {
            value <- str_split(y, " ")[[1]]
            length(value) <- idx_length[x]
            
            value
          })
          
          if (idx_length[x] > 1) {
            values <- data.frame(t(values))
          } else {
            values <- data.frame(values)
          }
          values
        }) |> 
        mutate_all(~ na_if(., "-")) |> 
        mutate_all(as.numeric) |> 
        select_if(~ !all(is.na(.)))
    })
    
    names(tmp) <- c("medipop_40_44_cnt", "medipop_45_49_cnt", 
                    "medipop_50_54_cnt", "medipop_55_59_cnt", 
                    "medipop_60_64_cnt", "medipop_65_69_cnt",
                    "medipop_70_74_cnt", "medipop_75_79_cnt",
                    "medipop_80_over_cnt")
  }

  tmp
}

##------------------------------------------------------------------------------
## 01.03.02. PDF parsing 오류 보정 함수
##------------------------------------------------------------------------------
fix_number <- function(str) {
  mk_space <- function(x) {
    nums <- unlist(strsplit(x, ","))
    
    str_new <- ""
    for(i in seq(nums)) {
      if (i == 1) {
        str_new <- paste0(nums[i], ",")
      } else {
        if (nchar(nums[i]) <= 3) {
          str_new <- paste0(str_new, nums[i])
        } else {
          str_new <- paste0(str_new, substr(nums[i], 1, 3), " ", substr(nums[i], 4, nchar(nums[i])), ",")
        }
      }  
    }
    
    str_new
  }
  
  slide_str <- function(str, len) {
    str_sub(str, 
            start = seq(1, str_length(str), by = len), 
            end = seq(len, str_length(str), by = len)) |> 
      paste(collapse = " ")
  }
  
  nums_space <- str |> 
    stringr::str_count(" ")
  
  base_space <- nums_space |> 
    table() |> 
    which.max() |> 
    names() |> 
    as.integer()
  
  str <- ifelse(nums_space != base_space, stringr::str_remove_all(str, " "), str) 
  nums_space <- str |> 
    stringr::str_count(" ")
  
  fix_num <- vector("character", length(str))
  for (i in seq(str)) {
    fix_num[i] <- ifelse(nums_space[i] == base_space, str[i], mk_space(str[i]))
  }
  
  fix_num <- stringr::str_remove(fix_num, ",$")
  
  fix_num2 <- vector("character", length(str))
  for (i in seq(fix_num)) {
    fix_num2[i] <- ifelse(fix_num[i] |> stringr::str_count(" ") == 0, 
                          slide_str(fix_num[i], nchar(fix_num[i]) / (base_space + 1)), 
                          fix_num[i]) 
  }
  
  fix_num2
}



################################################################################
## 02. 서울/인천/경기/강원
################################################################################
##==============================================================================
## 02.01. 서울, 인천, 경기 일부
##==============================================================================
##------------------------------------------------------------------------------
## 02.01.01. 좌측 페이지 - 남성
##------------------------------------------------------------------------------
extract_tables(pdf_stat, pages = 9, col_names = FALSE, guess = FALSE) |> 
  (function(x) x[[1]])() |> 
  filter(row_number() <= 54) |> 
  filter(row_number() >= 6) -> tmp

tab_01_left <- get_tab2(tmp, "left")


##------------------------------------------------------------------------------
## 02.01.02. 우측 페이지 - 남성
##------------------------------------------------------------------------------
extract_tables(pdf_stat, pages = 10, col_names = FALSE, guess = FALSE) |> 
  (function(x) x[[1]])() |> 
  filter(row_number() <= 56) |> 
  filter(row_number() >= 8) -> tmp

tab_01_right <- get_tab2(tmp, "right")

tab_01_male <- tab_01_left |> 
  bind_cols(tab_01_right) |> 
  mutate(gender_cd = "01")


##------------------------------------------------------------------------------
## 02.01.03. 좌측 페이지 - 여성
##------------------------------------------------------------------------------
extract_tables(pdf_stat, pages = 13, col_names = FALSE, guess = FALSE) |> 
  (function(x) x[[1]])() |> 
  filter(row_number() <= 54) |> 
  filter(row_number() >= 6) -> tmp

tab_01_left <- get_tab2(tmp, "left")


##------------------------------------------------------------------------------
## 02.01.04. 우측 페이지 - 여성
##------------------------------------------------------------------------------
extract_tables(pdf_stat, pages = 14, col_names = FALSE, guess = FALSE) |> 
  (function(x) x[[1]])() |> 
  filter(row_number() <= 56) |> 
  filter(row_number() >= 8) -> tmp

tab_01_right <- get_tab2(tmp, "right")

tab_01_female <- tab_01_left |> 
  bind_cols(tab_01_right) |> 
  mutate(gender_cd = "02")



##==============================================================================
## 02.02. 경기 일부, 강원
##==============================================================================
##------------------------------------------------------------------------------
## 02.02.01. 좌측 페이지-남성
##------------------------------------------------------------------------------
extract_tables(pdf_stat, pages = 11, col_names = FALSE, guess = FALSE) |> 
  (function(x) x[[1]])() |> 
  filter(row_number() <= 55) |> 
  filter(row_number() >= 6) -> tmp

tab_02_left <- get_tab2(tmp, "left")


##------------------------------------------------------------------------------
## 02.01.02. 우측 페이지-남성
##------------------------------------------------------------------------------
extract_tables(pdf_stat, pages = 12, col_names = FALSE, guess = FALSE) |> 
  (function(x) x[[1]])() |> 
  filter(row_number() <= 57) |> 
  filter(row_number() >= 8) -> tmp

tab_02_right <- get_tab2(tmp, "right")

tab_02_male <- tab_02_left |> 
  bind_cols(tab_02_right) |> 
  mutate_at(vars(matches("cnt")), ~ as.numeric(.))


##------------------------------------------------------------------------------
## 02.02.03. 좌측 페이지-여성
##------------------------------------------------------------------------------
extract_tables(pdf_stat, pages = 15, col_names = FALSE, guess = FALSE) |> 
  (function(x) x[[1]])() |> 
  filter(row_number() <= 55) |> 
  filter(row_number() >= 6) -> tmp

idx_req_fix <- which(sapply(tmp, 
                            function(x) x |> stringr::str_count(" ") |> table() |> length()) > 0) 
idx_req_fix <- idx_req_fix[-1]

for (i in idx_req_fix) {
  tmp[, i] <- fix_number(tmp[, i][[1]])
}

tab_02_left <- get_tab2(tmp, "left")


##------------------------------------------------------------------------------
## 02.01.04. 우측 페이지-여성
##------------------------------------------------------------------------------
extract_tables(pdf_stat, pages = 16, col_names = FALSE, guess = FALSE) |> 
  (function(x) x[[1]])() |> 
  filter(row_number() <= 57) |> 
  filter(row_number() >= 8) -> tmp

idx_req_fix <- which(sapply(tmp, 
                            function(x) x |> stringr::str_count(" ") |> table() |> length()) > 0) 
# idx_req_fix <- idx_req_fix[-1] # 첫번째 열은 문자형이므로 제외

for (i in idx_req_fix) {
  tmp[, i] <- stringr::str_replace_all(tmp[, i][[1]], " ,", ",")
  tmp[, i] <- fix_number(tmp[, i][[1]])
}

tab_02_right <- get_tab2(tmp, "right")

tab_02_female <- tab_02_left |> 
  bind_cols(tab_02_right) |> 
  mutate_at(vars(matches("cnt")), ~ as.numeric(.))



################################################################################
## 03. 대전/충북/충남/세종
################################################################################
##==============================================================================
## 03.01. 대전, 충북, 충남, 세종
##==============================================================================
##------------------------------------------------------------------------------
## 03.01.01. 좌측 페이지
##------------------------------------------------------------------------------
extract_tables(pdf_stat, pages = 209, col_names = FALSE, guess = FALSE) |> 
  (function(x) x[[1]])() |> 
  filter(row_number() <= 44) |> 
  filter(row_number() >= 6) -> tmp

tab_03_left <- get_tab2(tmp, "left")

# tab_03_left <- tmp |> 
#   mutate_all(~ str_remove_all(., ",")) |> 
#   mutate(X11 = str_remove_all(X1, "[0-9,]+| ")) |> 
#   mutate(X12 = str_extract_all(X1, "[0-9]+") |> map_chr(~ .[1])) |> 
#   mutate(X31 = ifelse(is.na(X2), str_split(X3, " ") |> map_chr(~ .[1]), X2)) |>    
#   mutate(X32 = ifelse(is.na(X2), str_split(X3, " ") |> map_chr(~ .[2]), X3)) |> 
#   mutate(X61 = ifelse(is.na(X6), str_split(X7, " ") |> map_chr(~ .[1]), X6)) |>  
#   mutate(X71 = ifelse(is.na(X6), str_split(X7, " ") |> map_chr(~ .[2]), 
#                       str_split(X7, " ") |> map_chr(~ .[1]))) |> 
#   mutate(X81 = ifelse(is.na(X6), str_split(X7, " ") |> map_chr(~ .[3]), 
#                       str_split(X7, " ") |> map_chr(~ .[2]))) |> 
#   select(X11:X32, X4, X5, X61:X81)  |> 
#   rename(cty_nm = X11, medipop_cnt = X12, emplye_estbmt_cnt = X31, 
#          emplye_popunr_cnt = X32, emplye_insurd_cnt = X4, emplye_dpndnt_cnt = X5,
#          cvlsvt_estbmt_cnt = X61, cvlsvt_popunr_cnt = X71, cvlsvt_insurd_cnt = X81) 

##------------------------------------------------------------------------------
## 03.01.02. 우측 페이지
##------------------------------------------------------------------------------
extract_tables(pdf_stat, pages = 210, col_names = FALSE, guess = FALSE) |> 
  (function(x) x[[1]])() |> 
  filter(row_number() <= 49) |> 
  filter(row_number() >= 11) -> tmp

tab_03_right <- get_tab2(tmp, "right")

tab_03 <- tab_03_left |> 
  bind_cols(tab_03_right)


################################################################################
## 04. 광주/전북/전남/제주
################################################################################
##==============================================================================
## 04.01. 광주, 전북, 전남, 제주
##==============================================================================
##------------------------------------------------------------------------------
## 04.01.01. 좌측 페이지
##------------------------------------------------------------------------------
extract_tables(pdf_stat, pages = 317, col_names = FALSE, guess = FALSE) |> 
  (function(x) x[[1]])() |> 
  filter(row_number() <= 55) |> 
  filter(row_number() >= 8) -> tmp

tab_04_left <- get_tab2(tmp, "left")

# tab_04_left <- tmp |> 
#   mutate_all(~ str_remove_all(., ",")) |> 
#   mutate(X11 = str_remove_all(X1, "[0-9,]+| ")) |> 
#   mutate(X12 = str_extract_all(X1, "[0-9]+") |> map_chr(~ .[1])) |> 
#   mutate(X31 = ifelse(is.na(X2), str_split(X3, " ") |> map_chr(~ .[1]), X2)) |>    
#   mutate(X32 = ifelse(is.na(X2), str_split(X3, " ") |> map_chr(~ .[2]), X3)) |> 
#   mutate(X61 = ifelse(is.na(X6), str_split(X7, " ") |> map_chr(~ .[1]), X6)) |>  
#   mutate(X71 = ifelse(is.na(X6), str_split(X7, " ") |> map_chr(~ .[2]), 
#                       str_split(X7, " ") |> map_chr(~ .[1]))) |> 
#   mutate(X81 = ifelse(is.na(X6), str_split(X7, " ") |> map_chr(~ .[3]), 
#                       str_split(X7, " ") |> map_chr(~ .[2]))) |> 
#   select(X11:X32, X4, X5, X61:X81)  |> 
#   rename(cty_nm = X11, medipop_cnt = X12, emplye_estbmt_cnt = X31, 
#          emplye_popunr_cnt = X32, emplye_insurd_cnt = X4, emplye_dpndnt_cnt = X5,
#          cvlsvt_estbmt_cnt = X61, cvlsvt_popunr_cnt = X71, cvlsvt_insurd_cnt = X81) 

##------------------------------------------------------------------------------
## 04.01.02. 우측 페이지
##------------------------------------------------------------------------------
extract_tables(pdf_stat, pages = 318, col_names = FALSE, guess = FALSE) |> 
  (function(x) x[[1]])() |> 
  filter(row_number() <= 58) |> 
  filter(row_number() >= 11) -> tmp

tab_04_right <- get_tab2(tmp, "right")

tab_04 <- tab_04_left |> 
  bind_cols(tab_04_right)


################################################################################
## 05. 부산/대구/울산/경북/경남
################################################################################
##==============================================================================
## 05.01. 부산, 대구, 울산, 경북 일부
##==============================================================================
##------------------------------------------------------------------------------
## 05.01.01. 좌측 페이지
##------------------------------------------------------------------------------
extract_tables(pdf_stat, pages = 427, col_names = FALSE, guess = FALSE) |> 
  (function(x) x[[1]])() |> 
  filter(row_number() <= 48) |> 
  filter(row_number() >= 10) -> tmp

tab_05_left <- get_tab2(tmp, "left")

# tab_05_left <- tmp |> 
#   mutate_all(~ str_remove_all(., ",")) |> 
#   mutate(X11 = str_remove_all(X1, "[0-9,]+| ")) |> 
#   mutate(X12 = str_extract_all(X1, "[0-9]+") |> map_chr(~ .[1])) |> 
#   mutate(X31 = ifelse(is.na(X2), str_split(X3, " ") |> map_chr(~ .[1]), X2)) |>    
#   mutate(X32 = ifelse(is.na(X2), str_split(X3, " ") |> map_chr(~ .[2]), X3)) |> 
#   mutate(X61 = ifelse(is.na(X6), str_split(X7, " ") |> map_chr(~ .[1]), X6)) |>  
#   mutate(X71 = ifelse(is.na(X6), str_split(X7, " ") |> map_chr(~ .[2]), 
#                       str_split(X7, " ") |> map_chr(~ .[1]))) |> 
#   mutate(X81 = ifelse(is.na(X6), str_split(X7, " ") |> map_chr(~ .[3]), 
#                       str_split(X7, " ") |> map_chr(~ .[2]))) |> 
#   select(X11:X32, X4, X5, X61:X81)  |> 
#   rename(cty_nm = X11, medipop_cnt = X12, emplye_estbmt_cnt = X31, 
#          emplye_popunr_cnt = X32, emplye_insurd_cnt = X4, emplye_dpndnt_cnt = X5,
#          cvlsvt_estbmt_cnt = X61, cvlsvt_popunr_cnt = X71, cvlsvt_insurd_cnt = X81) 

##------------------------------------------------------------------------------
## 05.01.02. 우측 페이지
##------------------------------------------------------------------------------
extract_tables(pdf_stat, pages = 428, col_names = FALSE, guess = FALSE) |> 
  (function(x) x[[1]])() |> 
  filter(row_number() <= 48) |> 
  filter(row_number() >= 10) -> tmp

tab_05_right <- get_tab2(tmp, "right")

tab_05 <- tab_05_left |> 
  bind_cols(tab_05_right)

##==============================================================================
## 05.02. 경북 일부, 경남
##==============================================================================
##------------------------------------------------------------------------------
## 05.02.01. 좌측 페이지
##------------------------------------------------------------------------------
extract_tables(pdf_stat, pages = 429, col_names = FALSE, guess = FALSE) |> 
  (function(x) x[[1]])() |> 
  filter(row_number() <= 50) |> 
  filter(row_number() >= 10) -> tmp

tab_06_left <- get_tab2(tmp, "left")

# tab_06_left <- tmp |> 
#   mutate_all(~ str_remove_all(., ",")) |> 
#   mutate(X11 = str_remove_all(X1, "[0-9,]+| ")) |> 
#   mutate(X12 = str_extract_all(X1, "[0-9]+") |> map_chr(~ .[1])) |> 
#   mutate(X31 = ifelse(is.na(X2), str_split(X3, " ") |> map_chr(~ .[1]), X2)) |>    
#   mutate(X32 = ifelse(is.na(X2), str_split(X3, " ") |> map_chr(~ .[2]), X3)) |> 
#   mutate(X61 = ifelse(is.na(X6), str_split(X7, " ") |> map_chr(~ .[1]), X6)) |>  
#   mutate(X71 = ifelse(is.na(X6), str_split(X7, " ") |> map_chr(~ .[2]), 
#                       str_split(X7, " ") |> map_chr(~ .[1]))) |> 
#   mutate(X81 = ifelse(is.na(X6), str_split(X7, " ") |> map_chr(~ .[3]), 
#                       str_split(X7, " ") |> map_chr(~ .[2]))) |> 
#   select(X11:X32, X4, X5, X61:X81)  |> 
#   rename(cty_nm = X11, medipop_cnt = X12, emplye_estbmt_cnt = X31, 
#          emplye_popunr_cnt = X32, emplye_insurd_cnt = X4, emplye_dpndnt_cnt = X5,
#          cvlsvt_estbmt_cnt = X61, cvlsvt_popunr_cnt = X71, cvlsvt_insurd_cnt = X81) 

##------------------------------------------------------------------------------
## 05.02.02. 우측 페이지
##------------------------------------------------------------------------------
extract_tables(pdf_stat, pages = 430, col_names = FALSE, guess = FALSE) |> 
  (function(x) x[[1]])() |> 
  filter(row_number() <= 50) |> 
  filter(row_number() >= 10) -> tmp

tab_06_right <- get_tab2(tmp, "right")

tab_06 <- tab_06_left |> 
  bind_cols(tab_06_right)



################################################################################
## 05. 분산된 광역시도 데이터 병합 
################################################################################
##==============================================================================
## 05.01. 분산된 광역시도 데이터 병합 
##==============================================================================

tab_01 |> 
  bind_rows(tab_02) |> 
  bind_rows(tab_03) |> 
  bind_rows(tab_04) |> 
  bind_rows(tab_05) |> 
  bind_rows(tab_06) -> df_stats



################################################################################
## 06. 시군구 정보 생성
################################################################################
##==============================================================================
## 06.01. 수치지도 병합을 위한 시군구명 보정 작업
##==============================================================================
##------------------------------------------------------------------------------
## 06.01.01. 광역시도명 보정
##------------------------------------------------------------------------------
df_stats <- df_stats |> 
  mutate(cty_nm = ifelse(cty_nm == "서울", "서울특별시", cty_nm)) |> 
  mutate(cty_nm = ifelse(cty_nm == "인천", "인천광역시", cty_nm)) |>
  mutate(cty_nm = ifelse(cty_nm == "경기", "경기도", cty_nm)) |>
  mutate(cty_nm = ifelse(cty_nm == "강원", "강원특별자치도", cty_nm)) |>
  mutate(cty_nm = ifelse(cty_nm == "대전", "대전광역시", cty_nm)) |>
  mutate(cty_nm = ifelse(cty_nm == "충북", "충청북도", cty_nm)) |>
  mutate(cty_nm = ifelse(cty_nm == "충남", "충청남도", cty_nm)) |>
  mutate(cty_nm = ifelse(cty_nm == "세종", "세종특별자치시", cty_nm)) |>
  mutate(cty_nm = ifelse(cty_nm == "광주", "광주광역시", cty_nm)) |>
  mutate(cty_nm = ifelse(cty_nm == "전북", "전북특별자치도", cty_nm)) |>
  mutate(cty_nm = ifelse(cty_nm == "전남", "전라남도", cty_nm)) |>
  mutate(cty_nm = ifelse(cty_nm == "제주", "제주특별자치도", cty_nm)) |>
  mutate(cty_nm = ifelse(cty_nm == "부산", "부산광역시", cty_nm)) |>
  mutate(cty_nm = ifelse(cty_nm == "대구", "대구광역시", cty_nm)) |>
  mutate(cty_nm = ifelse(cty_nm == "울산", "울산광역시", cty_nm)) |>
  mutate(cty_nm = ifelse(cty_nm == "경북", "경상북도", cty_nm)) |>
  mutate(cty_nm = ifelse(cty_nm == "경남", "경상남도", cty_nm)) 

##------------------------------------------------------------------------------
## 06.01.02. 시군구명 보정
## 시명+구명 패턴 보정: 수원시장안구 -> 수원시 장안구
##------------------------------------------------------------------------------
df_stats <- df_stats |> 
  mutate(cty_nm = gsub("([[:alpha:]]*시)([[:alpha:]]*구)", "\\1 \\2", cty_nm))


##==============================================================================
## 06.02. 중복 시군구명 보정 작업을 통한 시군구 정보 생성
##==============================================================================  
sido_nm <- c("서울특별시", "인천광역시", "경기도", "강원특별자치도", "대전광역시", 
             "충청북도", "충청남도", "세종특별자치시", "광주광역시", 
             "전북특별자치도", "전라남도", "제주특별자치도", "부산광역시", 
             "대구광역시", "울산광역시", "경상북도", "경상남도") 
  
idx_sido <- sido_nm |> purrr::map_int(function(x) {
  which(str_detect(df_stats$cty_nm, x))
})

vec_sido <- character()

for (x in seq(length(idx_sido))) {
  if (x == length(idx_sido)) {
    vec_sido <- c(vec_sido, rep(sido_nm[x], NROW(df_stats) - idx_sido[x] + 1))
  } else {
    vec_sido <- c(vec_sido, rep(sido_nm[x], idx_sido[x + 1] - idx_sido[x])) 
  }
}

df_stats$mega_nm <- vec_sido


df_stats |> 
  mutate(cty_nm = ifelse(mega_nm %in% "세종특별자치시", "세종시", cty_nm)) -> df_stats

##==============================================================================
## 06.03. 분구안된 시레벨의 시군구 복제 생성
## 경기도 부천시 복제
##==============================================================================  
df_stats <- df_stats |> 
  bind_rows(
    df_stats |> 
      filter(mega_nm == "경기도" & cty_nm == "부천시") |> 
      mutate(cty_nm = "부천시 원미구")
  ) |> 
  bind_rows(
    df_stats |> 
      filter(mega_nm == "경기도" & cty_nm == "부천시") |> 
      mutate(cty_nm = "부천시 소사구")
  ) |>
  mutate(cty_nm = ifelse(mega_nm == "경기도" & cty_nm == "부천시", "부천시 오정구", cty_nm)) 


##==============================================================================
## 06.04. 수치지도 병합을 통한 시군구 정보 생성
##==============================================================================
df_stats <- df_stats |> left_join(
  bitSpatial::cty |> 
    select(mega_cd, mega_nm, cty_cd, cty_nm), by = c("mega_nm", "cty_nm"))



################################################################################
## 07. Export data from tibbles to table of DBMS
################################################################################

##==============================================================================
## 07.01. 광역시도별 적용인구 현황
##==============================================================================
mega_popunr <- df_stats |> 
  filter(is.na(cty_cd)) |>
  bind_rows(
    df_stats |> 
      filter(mega_nm == "세종특별자치시")
  ) |>
  select(-mega_cd, -cty_cd) |>
  left_join(
    bitSpatial::mega |> 
      select(mega_cd, mega_nm), by = c("cty_nm" = "mega_nm")) |> 
  mutate(mega_cd = ifelse(mega_nm %in% "세종특별자치시", "36", mega_cd)) |>
  mutate(base_year = "2023") |>
  select(base_year, mega_cd, mega_nm, medipop_cnt:medaid_clss2_dpndnt_cnt) |> 
  arrange(base_year, mega_cd) |>
  mutate(cret_dt = Sys.time()) |> 
  mutate(cret_nm = "bitPublish") |> 
  mutate(mdfy_dt = as.POSIXct(NA)) |> 
  mutate(mdfy_nm = as.character(NA)) |> 
  rename_with(toupper) |> 
  tibble::as_tibble()

dbWriteTable(con, "TB_NHIS_POPUNR_MEGA", mega_popunr, overwrite = TRUE)
# dbGetQuery(con, "select * from TB_NHIS_POPUNR_MEGA limit 5;")

##==============================================================================
## 07.02. 시군구별 적용인구 현황
##==============================================================================
cty_popunr <- df_stats |> 
  filter(!is.na(cty_cd)) |>
  mutate(base_year = "2023") |>
  select(base_year, mega_cd, mega_nm, cty_cd, cty_nm, medipop_cnt:medaid_clss2_dpndnt_cnt) |> 
  arrange(base_year, mega_cd, cty_cd) |>
  mutate(cret_dt = Sys.time()) |> 
  mutate(cret_nm = "bitPublish") |> 
  mutate(mdfy_dt = as.POSIXct(NA)) |> 
  mutate(mdfy_nm = as.character(NA)) |> 
  rename_with(toupper) |> 
  tibble::as_tibble()

dbWriteTable(con, "TB_NHIS_POPUNR_CTY", cty_popunr, overwrite = TRUE)
# dbGetQuery(con, "select * from TB_NHIS_POPUNR_CTY limit 5;")


##==============================================================================
## 07.03. Connect DBMS
##==============================================================================
dbDisconnect(con)


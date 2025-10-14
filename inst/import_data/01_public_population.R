################################################################################
## 01.public_population.R
##  - 행정안전부 주민등록 인구 및 세대현황
##    (https://jumin.mois.go.kr/index.jsp)
##  - 행정동별 주민등록 인구 및 세대현황
##  - 행정동별 연령별 인구현황
################################################################################
################################################################################
## 01. Prepare resources
################################################################################
##==============================================================================
## 01.01. Load R libraries
##==============================================================================
library(tidyverse)
library(RSQLite)

##==============================================================================
## 01.02. Global variables
##==============================================================================
data_path <- here::here("inst", "raw")
con <- dbConnect(SQLite(), here::here("inst", "dbms", "public.sqlite"))

################################################################################
## 02. 주민등록 인구 및 세대현황
################################################################################
##==============================================================================
## 02.01. 데이터 읽기
##==============================================================================
## https://jumin.mois.go.kr/index.jsp
## 행정안전부 > 주민등록 인구통계 > 주민등록 인구 및 세대현황

fnames <- c("202508_202508_주민등록인구및세대현황_월간.xlsx",
            "202412_202412_주민등록인구및세대현황_연간.xlsx",
            "202312_202312_주민등록인구및세대현황_연간.xlsx",
            "202212_202212_주민등록인구및세대현황_연간.xlsx",
            "202112_202112_주민등록인구및세대현황_연간.xlsx",
            "202012_202012_주민등록인구및세대현황_연간.xlsx",
            "201912_201912_주민등록인구및세대현황_연간.xlsx",
            "201812_201812_주민등록인구및세대현황_연간.xlsx",
            "201712_201712_주민등록인구및세대현황_연간.xlsx",
            "201612_201612_주민등록인구및세대현황_연간.xlsx",
            "201512_201512_주민등록인구및세대현황_연간.xlsx",
            "201412_201412_주민등록인구및세대현황_연간.xlsx",
            "201312_201312_주민등록인구및세대현황_연간.xlsx")

population_total <- fnames %>% 
  purrr::map_df(
    function(x) {
      fname <- glue::glue("{data_path}/{x}")
      
      population_total <- readxl::read_xlsx(fname, skip = 2)
      
      population_total %>% 
        rename("org_cd" = 행정기관코드,
               "org_nm" = 행정기관,
               "population" = 총인구수,
               "household" = 세대수,
               "pop_per_hosue" = `세대당 인구`,
               "pop_male" = `남자 인구수`,
               "pop_female" = `여자 인구수`,
               "male_per_female" = `남여 비율`) %>% 
        filter(!str_detect(org_cd, "000000$")) %>% 
        mutate(base_ym = stringr::str_sub(x, 1, 6)) %>% 
        mutate_at(vars(!matches("org")), function(x) {str_remove(x, ",")}) %>% 
        mutate_at(vars(!matches("org")), as.numeric) %>% 
        mutate(mega_cd = substr(org_cd, 1, 2)) %>% 
        mutate(mega_nm = str_split(org_nm, " ", n = 3) %>%
                 purrr::map_chr(function(x) x[1])) %>% 
        mutate(cty_cd = substr(org_cd, 1, 5)) %>% 
        mutate(cty_nm = str_split(org_nm, " ", n = 3) %>%
                 purrr::map_chr(function(x) x[2])) %>% 
        mutate(admi_cd = substr(org_cd, 1, 8)) %>% 
        mutate(admi_nm = str_split(org_nm, " ", n = 3) %>%
                 purrr::map_chr(function(x) x[3])) %>% 
        select(base_ym, mega_cd:admi_nm, population:male_per_female) %>% 
        filter(!is.na(admi_nm))
    }
  ) %>% 
  mutate(base_ym = as.character(base_ym))

##==============================================================================
## 02.02. 공공데이터의 특정 시군구 코드 오류 보정
##==============================================================================
## http://10.10.3.74:9003/2140096/region_report/-/issues/28
# population_total <- population_total %>% 
#   mutate(cty_cd = case_when(
#     cty_cd %in% c("4374") & cty_nm %in% "증평군" ~ "4378",
#     TRUE ~ cty_cd
#   )) %>% 
#   mutate(admi_cd = case_when(
#     cty_cd %in% c("4378") & admi_nm %in% "증평읍" ~ "43785250",
#     cty_cd %in% c("4378") & admi_nm %in% "도안면" ~ "43785310",    
#     TRUE ~ admi_cd
#   ))

population_total <- population_total |> 
  mutate(cty_nm = ifelse(str_count(admi_nm, " ") == 1, 
                         paste(cty_nm, str_split(admi_nm, " ", n = 2) |> 
                           purrr::map_chr(function(x) x[1])), cty_nm)) |> 
  mutate(admi_nm = ifelse(str_count(admi_nm, " ") == 1, 
                         str_split(admi_nm, " ", n = 2) |> 
                                 purrr::map_chr(function(x) x[2]), admi_nm)) 

## 경기도 고양시 일산서구와 같은 전국 12개 시 + 구 + 동 체계지역의 구집계 제거
population_total <- population_total %>% 
  filter(!str_detect(admi_cd, "000$")) 


##==============================================================================
## 02.03. 주소정제 로직
##==============================================================================
##------------------------------------------------------------------------------
## 출장소건 제거 (시군구/읍면동 레벨 출장소)
##------------------------------------------------------------------------------
population_total <- population_total %>% 
  filter(!str_detect(cty_nm, "출장소")) %>% 
  filter(!str_detect(admi_nm, "출장소"))

##------------------------------------------------------------------------------
## 읍면동 변경 건
##------------------------------------------------------------------------------
fname <- here::here("inst", "raw", "reorg_admi_rule.csv")
change_rule <- read_csv(fname) %>% 
  filter(is.na(reverse)) %>% 
  mutate(mega_cd = as.character(mega_cd)) %>% 
  mutate(cty_cd = as.character(cty_cd)) %>% 
  mutate(from_cd = as.character(from_cd)) %>% 
  mutate(to_cd = as.character(to_cd)) %>% 
  arrange(change_date)

NROW(change_rule) %>% 
  seq() %>% 
  purrr::walk(
    function(x) {
      population_total <<- population_total %>% 
        mutate(admi_cd = ifelse(admi_cd %in% change_rule$from_cd[x], 
                                change_rule$to_cd[x], admi_cd)) %>% 
        mutate(admi_nm = ifelse(admi_cd %in% change_rule$to_cd[x], 
                                change_rule$to_nm[x], admi_nm)) %>% 
        filter(population != 0)
    }
  ) 

## 경기도 포천시 군내면인데 파주시 군내면으로 오기건 (2020-12)
# population_total <- population_total %>% 
#   filter(!(cty_nm %in% "파주시" & admi_nm %in% "군내면")) %>% 
#   group_by(base_ym, mega_cd, mega_nm, cty_cd, cty_nm, admi_nm, admi_cd) %>% 
#   summarise(
#     population = sum(population),
#     household = sum(household),
#     pop_per_hosue = round(sum(population) / sum(household), 2),
#     pop_male = sum(pop_male),
#     pop_female = sum(pop_female),
#     male_per_female = round(sum(pop_male) / sum(pop_female), 2),
#     .groups = "drop")


## 충북 청원군이 청주시로 통폐합된 건에 대해서 시군구 수정 (2014-07-01)
# population_total <- population_total %>% 
#   mutate(cty_nm = ifelse(cty_cd %in% "4371", "청주시", cty_nm)) %>% 
#   mutate(cty_cd = ifelse(cty_cd %in% "4371", "4311", cty_cd))


## 미추홀구 코드 정제
population_total <- population_total %>% 
  mutate(cty_nm = ifelse(cty_cd %in% "2817", "미추홀구", cty_nm)) %>% 
  mutate(admi_cd = ifelse(admi_nm %in% "도화2,3동", "28177610", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_nm %in% "숭의1,3동", "28177520", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_nm %in% "용현1,4동", "28177540", admi_cd)) %>% 
  mutate(admi_nm = ifelse(admi_nm %in% "숭의1.3동", "숭의1,3동", admi_nm)) %>% 
  mutate(admi_nm = ifelse(admi_nm %in% "용현1.4동", "용현1,4동", admi_nm)) %>% 
  mutate(admi_nm = ifelse(admi_nm %in% "도화2.3동", "도화2,3동", admi_nm)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170520", "28177510", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170540", "28177530", admi_cd)) %>%   
  mutate(admi_cd = ifelse(admi_cd %in% "28170560", "28177550", admi_cd)) %>%  
  mutate(admi_cd = ifelse(admi_cd %in% "28170570", "28177560", admi_cd)) %>%  
  mutate(admi_cd = ifelse(admi_cd %in% "28170590", "28177570", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170600", "28177580", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170610", "28177590", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170630", "28177600", admi_cd)) %>%   
  mutate(admi_cd = ifelse(admi_cd %in% "28170660", "28177620", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170670", "28177630", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170680", "28177640", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170690", "28177650", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170700", "28177660", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170710", "28177670", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170720", "28177680", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170730", "28177690", admi_cd)) %>%
  mutate(admi_cd = ifelse(admi_cd %in% "28170735", "28177700", admi_cd)) %>%
  mutate(admi_cd = ifelse(admi_cd %in% "28170740", "28177710", admi_cd))


population_total <- population_total %>% 
  group_by(base_ym, mega_cd, mega_nm, cty_cd, cty_nm, admi_cd, admi_nm) %>% 
  summarise(
    population = sum(population),
    household = sum(household),
    pop_per_hosue = round(sum(population) / sum(household), 2),
    pop_male = sum(pop_male),
    pop_female = sum(pop_female),
    male_per_female = round(sum(pop_male) / sum(pop_female), 2),
    .groups = "drop")

# population_total <- population_total %>% 
#   bind_rows(
#     admi |> 
#       filter(admi_cd == "28260700") |> 
#       select(mega_cd:admi_nm, population:male_per_female)
#   )


##==============================================================================
## 02.04. 광역시도 및 시군구 레벨 집계
##==============================================================================
##------------------------------------------------------------------------------
## 02.04.01. 시군구 레벨에 집계하여 붙이기
##------------------------------------------------------------------------------

population_total_cty <- population_total %>% 
  group_by(base_ym, mega_cd, mega_nm, cty_cd, cty_nm) %>% 
  summarise(population = sum(population),
            household = sum(household),
            pop_per_hosue = round(sum(population) / sum(household), 2),
            pop_male = sum(pop_male),
            pop_female = sum(pop_female),
            male_per_female = sum(pop_male) / sum(pop_female),
            .groups = "drop")


##------------------------------------------------------------------------------
## 02.04.02. 광역시도 레벨에 집계하여 붙이기
##------------------------------------------------------------------------------
population_total_mega <- population_total %>% 
  group_by(base_ym, mega_cd, mega_nm) %>% 
  summarise(population = sum(population),
            household = sum(household),
            pop_per_hosue = round(sum(population) / sum(household), 2),
            pop_male = sum(pop_male),
            pop_female = sum(pop_female),
            male_per_female = sum(pop_male) / sum(pop_female),
            .groups = "drop")


##==============================================================================
## 02.05. Import from data frame to DBMS
##==============================================================================
##------------------------------------------------------------------------------
## 02.05.01. 읍면동 레벨
##------------------------------------------------------------------------------
population_admi <- population_total |> 
  rename(POPLTN_CNT = population) |> 
  rename(HSHLD_CNT = household) |> 
  rename(POPLTN_HSHLD_CNT = pop_per_hosue) |> 
  rename(POPLTN_MALE_CNT = pop_male) |> 
  rename(POPLTN_FEMALE_CNT = pop_female) |> 
  rename(POPLTN_GNDR_RT = male_per_female) |> 
  mutate(cret_dt = Sys.time()) |> 
  mutate(cret_nm = "bitPublish") |> 
  mutate(mdfy_dt = as.POSIXct(NA)) |> 
  mutate(mdfy_nm = as.character(NA)) |> 
  rename_with(toupper)

dbWriteTable(con, "TB_POPHUS_ADMI", population_admi, overwrite = TRUE)
# dbGetQuery(con, "select * from TB_POPHUS_ADMI limit 5;")

##------------------------------------------------------------------------------
## 02.05.02. 시군구 레벨
##------------------------------------------------------------------------------
population_cty <- population_total_cty |> 
  rename(POPLTN_CNT = population) |> 
  rename(HSHLD_CNT = household) |> 
  rename(POPLTN_HSHLD_CNT = pop_per_hosue) |> 
  rename(POPLTN_MALE_CNT = pop_male) |> 
  rename(POPLTN_FEMALE_CNT = pop_female) |> 
  rename(POPLTN_GNDR_RT = male_per_female) |> 
  mutate(cret_dt = Sys.time()) |> 
  mutate(cret_nm = "bitPublish") |> 
  mutate(mdfy_dt = as.POSIXct(NA)) |> 
  mutate(mdfy_nm = as.character(NA)) |> 
  rename_with(toupper)

dbWriteTable(con, "TB_POPHUS_CTY", population_cty, overwrite = TRUE)
# dbGetQuery(con, "select * from TB_POPHUS_ADMI limit 5;")

##------------------------------------------------------------------------------
## 02.05.03. 광역시도 레벨
##------------------------------------------------------------------------------
population_mega <- population_total_mega |> 
  rename(POPLTN_CNT = population) |> 
  rename(HSHLD_CNT = household) |> 
  rename(POPLTN_HSHLD_CNT = pop_per_hosue) |> 
  rename(POPLTN_MALE_CNT = pop_male) |> 
  rename(POPLTN_FEMALE_CNT = pop_female) |> 
  rename(POPLTN_GNDR_RT = male_per_female) |> 
  mutate(cret_dt = Sys.time()) |> 
  mutate(cret_nm = "bitPublish") |> 
  mutate(mdfy_dt = as.POSIXct(NA)) |> 
  mutate(mdfy_nm = as.character(NA)) |> 
  rename_with(toupper)

dbWriteTable(con, "TB_POPHUS_MEGA", population_mega, overwrite = TRUE)
# dbGetQuery(con, "select * from TB_POPHUS_ADMI limit 5;")


################################################################################
## 03. 연령별 인구현황 (5세 단위)
################################################################################
##==============================================================================
## 03.01. Import data from files
##==============================================================================
## https://jumin.mois.go.kr/index.jsp
## 행정안전부 > 주민등록 인구통계 > 연령별 인구현황
## 구분에서 계를 체크 해제 후 다운로드

fnames <- c("202508_202508_연령별인구현황_월간.xlsx",
            "202412_202412_연령별인구현황_연간.xlsx",
            "202312_202312_연령별인구현황_연간.xlsx",
            "202212_202212_연령별인구현황_연간.xlsx",
            "202112_202112_연령별인구현황_연간.xlsx",
            "202012_202012_연령별인구현황_연간.xlsx",
            "201912_201912_연령별인구현황_연간.xlsx",
            "201812_201812_연령별인구현황_연간.xlsx",
            "201712_201712_연령별인구현황_연간.xlsx",
            "201612_201612_연령별인구현황_연간.xlsx",
            "201512_201512_연령별인구현황_연간.xlsx",
            "201412_201412_연령별인구현황_연간.xlsx",
            "201312_201312_연령별인구현황_연간.xlsx")

population_age <- fnames %>% 
  purrr::map_df(
    function(x) {
      fname <- glue::glue("{data_path}/{x}")
      
      population_age <- readxl::read_xlsx(fname, skip = 3) %>% 
        select(-"남 인구수", -"여 인구수", -"연령구간인구수...4", -"연령구간인구수...27")
      
      population_age %>% 
        rename("org_cd" = 행정기관코드,
               "org_nm" = 행정기관) %>% 
        rename_all(str_remove_all, "\\.") %>% 
        rename_all(str_remove_all, "~") %>% 
        filter(!str_detect(org_cd, "000000$")) %>% 
        mutate(base_ym = stringr::str_sub(x, 1, 6)) %>%         
        mutate_at(vars(!matches("org")), function(x) {str_remove(x, ",")}) %>% 
        mutate_at(vars(!matches("org")), as.numeric) %>% 
        pivot_longer(`04세5`:`100세 이상48`,  
                     names_to = "age_group", values_to = "population") %>% 
        mutate(age_group = str_remove(age_group, "세[[:number:]]")) %>% 
        mutate(gender = case_when(
          age_group %in% c("04", "59", "1014", "1519", "2024", "25290", "30341", 
                           "35392", "40443", "45494", "50545", "55596", "60647",
                           "65698", "70749", "75790", "80841", "85892", "90943",
                           "95994", "100세 이상25") ~ "남",
          age_group %in% c("048", "599", "10140", "15191", "20242", "25293", "30344", 
                           "35395", "40446", "45497", "50548", "55599", "60640",
                           "65691", "70742", "75793", "80844", "85895", "90946",
                           "95997", "100세 이상48") ~ "여"
        )) %>% 
        mutate(age_group = case_when(
          str_detect(age_group, "^100") ~ "100+",
          str_detect(age_group, "^04") ~ "01-04",
          str_detect(age_group, "^59") ~ "05-09",
          str_detect(age_group, "^1014") ~ "10-14",
          str_detect(age_group, "^1519") ~ "15-19",
          str_detect(age_group, "^2024") ~ "20-24",
          str_detect(age_group, "^2529") ~ "25-29",
          str_detect(age_group, "^3034") ~ "30-34",
          str_detect(age_group, "^3539") ~ "35-39",
          str_detect(age_group, "^4044") ~ "40-44",
          str_detect(age_group, "^4549") ~ "45-49",
          str_detect(age_group, "^5054") ~ "50-54",
          str_detect(age_group, "^5559") ~ "55-59",
          str_detect(age_group, "^6064") ~ "60-64",
          str_detect(age_group, "^6569") ~ "65-69",
          str_detect(age_group, "^7074") ~ "70-74",
          str_detect(age_group, "^7579") ~ "75-79",
          str_detect(age_group, "^8084") ~ "80-84",
          str_detect(age_group, "^8589") ~ "85-89",
          str_detect(age_group, "^9094") ~ "90-94",
          str_detect(age_group, "^9599") ~ "95-99"
        )) %>%    
        mutate(mega_cd = substr(org_cd, 1, 2)) %>% 
        mutate(mega_nm = str_split(org_nm, " ", n = 3) %>%
                 purrr::map_chr(function(x) x[1])) %>% 
        mutate(cty_cd = substr(org_cd, 1, 5)) %>% 
        mutate(cty_nm = str_split(org_nm, " ", n = 3) %>%
                 purrr::map_chr(function(x) x[2])) %>% 
        mutate(admi_cd = substr(org_cd, 1, 8)) %>% 
        mutate(admi_nm = str_split(org_nm, " ", n = 3) %>%
                 purrr::map_chr(function(x) x[3])) %>% 
        select(base_ym, mega_cd:admi_nm, age_group, gender, population) %>% 
        filter(!is.na(admi_nm))
    }
  ) %>% 
  mutate(base_ym = as.character(base_ym))


##==============================================================================
## 03.02. 공공데이터의 특정 시군구 코드 오류 보정
##==============================================================================
## 공공데이터의 특정 시군구 코드 오류 보정
## http://10.10.3.74:9003/2140096/region_report/-/issues/28
# population_age <- population_age %>% 
#   mutate(cty_cd = case_when(
#     cty_cd %in% c("4374") & cty_nm %in% "증평군" ~ "4378",
#     TRUE ~ cty_cd
#   )) %>% 
#   mutate(admi_cd = case_when(
#     cty_cd %in% c("4378") & admi_nm %in% "증평읍" ~ "43785250",
#     cty_cd %in% c("4378") & admi_nm %in% "도안면" ~ "43785310",    
#     TRUE ~ admi_cd
#   ))

population_age <- population_age |> 
  mutate(cty_nm = ifelse(str_count(admi_nm, " ") == 1, 
                         paste(cty_nm, str_split(admi_nm, " ", n = 2) |> 
                                 purrr::map_chr(function(x) x[1])), cty_nm)) |> 
  mutate(admi_nm = ifelse(str_count(admi_nm, " ") == 1, 
                          str_split(admi_nm, " ", n = 2) |> 
                            purrr::map_chr(function(x) x[2]), admi_nm)) 

## 경기도 고양시 일산서구와 같은 전국 12개 시 + 구 + 동 체계지역의 구집계 제거
population_age <- population_age %>% 
  filter(!str_detect(admi_cd, "000$")) 


##==============================================================================
## 03.03. 주소정제 로직
##==============================================================================
##------------------------------------------------------------------------------
## 출장소건 제거 (시군구/읍면동 레벨 출장소)
##------------------------------------------------------------------------------
population_age <- population_age %>% 
  filter(!str_detect(cty_nm, "출장소")) %>% 
  filter(!str_detect(admi_nm, "출장소"))

##------------------------------------------------------------------------------
## 읍면동 변경 건
##------------------------------------------------------------------------------
NROW(change_rule) %>% 
  seq() %>% 
  purrr::walk(
    function(x) {
      population_age <<- population_age %>% 
        mutate(admi_cd = ifelse(admi_cd %in% change_rule$from_cd[x], 
                                change_rule$to_cd[x], admi_cd)) %>% 
        mutate(admi_nm = ifelse(admi_cd %in% change_rule$to_cd[x], 
                                change_rule$to_nm[x], admi_nm)) %>% 
        filter(population != 0)
    }
  ) 

## 경기도 포천시 군내면인데 파주시 군내면으로 오기건 (2020-12)
# population_age <- population_age %>% 
#   filter(!(cty_nm %in% "파주시" & admi_nm %in% "군내면")) %>% 
#   group_by(base_ym, mega_cd, mega_nm, cty_cd, cty_nm, admi_nm, admi_cd,
#            age_group, gender) %>% 
#   summarise(
#     population = sum(population),
#     .groups = "drop")


## 충북 청원군이 청주시로 통폐합된 건에 대해서 시군구 수정 (2014-07-01)
# population_age <- population_age %>% 
#   mutate(cty_nm = ifelse(cty_cd %in% "4371", "청주시", cty_nm)) %>% 
#   mutate(cty_cd = ifelse(cty_cd %in% "4371", "4311", cty_cd))


## 미추홀구 코드 정제
population_age <- population_age %>% 
  mutate(cty_nm = ifelse(cty_cd %in% "2817", "미추홀구", cty_nm)) %>% 
  mutate(admi_cd = ifelse(admi_nm %in% "도화2,3동", "28177610", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_nm %in% "숭의1,3동", "28177520", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_nm %in% "용현1,4동", "28177540", admi_cd)) %>% 
  mutate(admi_nm = ifelse(admi_nm %in% "숭의1.3동", "숭의1,3동", admi_nm)) %>% 
  mutate(admi_nm = ifelse(admi_nm %in% "용현1.4동", "용현1,4동", admi_nm)) %>% 
  mutate(admi_nm = ifelse(admi_nm %in% "도화2.3동", "도화2,3동", admi_nm)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170520", "28177510", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170540", "28177530", admi_cd)) %>%   
  mutate(admi_cd = ifelse(admi_cd %in% "28170560", "28177550", admi_cd)) %>%  
  mutate(admi_cd = ifelse(admi_cd %in% "28170570", "28177560", admi_cd)) %>%  
  mutate(admi_cd = ifelse(admi_cd %in% "28170590", "28177570", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170600", "28177580", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170610", "28177590", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170630", "28177600", admi_cd)) %>%   
  mutate(admi_cd = ifelse(admi_cd %in% "28170660", "28177620", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170670", "28177630", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170680", "28177640", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170690", "28177650", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170700", "28177660", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170710", "28177670", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170720", "28177680", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170730", "28177690", admi_cd)) %>%
  mutate(admi_cd = ifelse(admi_cd %in% "28170735", "28177700", admi_cd)) %>%
  mutate(admi_cd = ifelse(admi_cd %in% "28170740", "28177710", admi_cd))


population_age <- population_age %>% 
  group_by(base_ym, mega_cd, mega_nm, cty_cd, cty_nm, admi_cd, admi_nm, 
           age_group, gender) %>% 
  summarise(
    population = sum(population),
    .groups = "drop")


##==============================================================================
## 03.03. 광역시도 및 시군구 레벨 집계
##==============================================================================
##------------------------------------------------------------------------------
## 03.03.01. 시군구 레벨에 집계하여 붙이기
##------------------------------------------------------------------------------
population_age_cty <- population_age %>% 
  group_by(base_ym, mega_cd, mega_nm, cty_cd, cty_nm, age_group, gender) %>% 
  summarise(population = sum(population),
            .groups = "drop") 


##------------------------------------------------------------------------------
## 03.03.02. 광역시도 레벨에 집계하여 붙이기
##------------------------------------------------------------------------------
population_age_mega <- population_age %>% 
  group_by(base_ym, mega_cd, mega_nm, age_group, gender) %>% 
  summarise(population = sum(population),
            .groups = "drop") 


##==============================================================================
## 03.04. Import from data frame to DBMS
##==============================================================================
##------------------------------------------------------------------------------
## 03.04.01. 읍면동 레벨
##------------------------------------------------------------------------------
population_age_admi <- population_age |> 
  rename(POPLTN_CNT = population) |> 
  rename(AGEGRP_CD = age_group) |> 
  rename(GNDR_CD = gender) |> 
  mutate(cret_dt = Sys.time()) |> 
  mutate(cret_nm = "bitPublish") |> 
  mutate(mdfy_dt = as.POSIXct(NA)) |> 
  mutate(mdfy_nm = as.character(NA)) |> 
  rename_with(toupper)

dbWriteTable(con, "TB_POPAGE_ADMI", population_age_admi, overwrite = TRUE)
# dbGetQuery(con, "select * from TB_POPHUS_ADMI limit 5;")


##------------------------------------------------------------------------------
## 03.04.02. 시군구 레벨
##------------------------------------------------------------------------------
population_cty <- population_age_cty |> 
  rename(POPLTN_CNT = population) |> 
  rename(AGEGRP_CD = age_group) |> 
  rename(GNDR_CD = gender) |> 
  mutate(cret_dt = Sys.time()) |> 
  mutate(cret_nm = "bitPublish") |> 
  mutate(mdfy_dt = as.POSIXct(NA)) |> 
  mutate(mdfy_nm = as.character(NA)) |> 
  rename_with(toupper)

dbWriteTable(con, "TB_POPAGE_CTY", population_age_cty, overwrite = TRUE)
# dbGetQuery(con, "select * from TB_POPHUS_ADMI limit 5;")


##------------------------------------------------------------------------------
## 03.04.03. 광역시도 레벨
##------------------------------------------------------------------------------
population_age_mega <- population_age_mega |> 
  rename(POPLTN_CNT = population) |> 
  rename(AGEGRP_CD = age_group) |> 
  rename(GNDR_CD = gender) |> 
  mutate(cret_dt = Sys.time()) |> 
  mutate(cret_nm = "bitPublish") |> 
  mutate(mdfy_dt = as.POSIXct(NA)) |> 
  mutate(mdfy_nm = as.character(NA)) |> 
  rename_with(toupper)

dbWriteTable(con, "TB_POPAGE_MEGA", population_age_mega, overwrite = TRUE)
# dbGetQuery(con, "select * from TB_POPHUS_ADMI limit 5;")


################################################################################
## 04. 지역별 세대원수별 세대수
################################################################################
##==============================================================================
## 04.01. 데이터 읽기
##==============================================================================
## https://jumin.mois.go.kr/index.jsp
## 행정안전부 > 주민등록 인구 기타현황 > 지역별 세대원수별 세대수
## 전국 선택 조회 후, 전체시군구현황을 체크하여 다운로드

fnames <- c("202508_202508_주민등록인구기타현황(세대원수별 세대수)_households.xlsx",
            "202412_202412_주민등록인구기타현황(세대원수별 세대수)_households.xlsx",
            "202312_202312_주민등록인구기타현황(세대원수별 세대수)_households.xlsx",
            "202212_202212_주민등록인구기타현황(세대원수별 세대수)_households.xlsx",
            "202112_202112_주민등록인구기타현황(세대원수별 세대수)_households.xlsx",
            "202012_202012_주민등록인구기타현황(세대원수별 세대수)_households.xlsx",
            "201912_201912_주민등록인구기타현황(세대원수별 세대수)_households.xlsx",
            "201812_201812_주민등록인구기타현황(세대원수별 세대수)_households.xlsx",
            "201712_201712_주민등록인구기타현황(세대원수별 세대수)_households.xlsx",
            "201612_201612_주민등록인구기타현황(세대원수별 세대수)_households.xlsx")

household_total <- fnames %>% 
  purrr::map_df(
    function(x) {
      fname <- glue::glue("{data_path}/{x}")
      
      household_total <- readxl::read_xlsx(fname, skip = 2)
      
      names(household_total) <- stringr::str_remove(names(household_total), "이상")
      
      household_total %>% 
        rename("org_cd" = 행정기관코드,
               "org_nm" = 행정기관,
               "household" = 전체세대,
               "household_01" = `1인세대`,
               "household_02" = `2인세대`,
               "household_03" = `3인세대`,
               "household_04" = `4인세대`,
               "household_05" = `5인세대`,
               "household_06" = `6인세대`,
               "household_07" = `7인세대`,
               "household_08" = `8인세대`,
               "household_09" = `9인세대`,
               "household_10" = `10인세대`) %>% 
        filter(!str_detect(org_cd, "000000$")) %>% 
        mutate(base_ym = stringr::str_sub(x, 1, 6)) %>% 
        mutate_at(vars(!matches("org")), function(x) {str_remove(x, ",")}) %>% 
        mutate_at(vars(!matches("org")), as.numeric) %>% 
        mutate(mega_cd = substr(org_cd, 1, 2)) %>% 
        mutate(mega_nm = str_split(org_nm, " ", n = 3) %>%
                 purrr::map_chr(function(x) x[1])) %>% 
        mutate(cty_cd = substr(org_cd, 1, 5)) %>% 
        mutate(cty_nm = str_split(org_nm, " ", n = 3) %>%
                 purrr::map_chr(function(x) x[2])) %>% 
        mutate(admi_cd = substr(org_cd, 1, 8)) %>% 
        mutate(admi_nm = str_split(org_nm, " ", n = 3) %>%
                 purrr::map_chr(function(x) x[3])) %>%         
        select(base_ym, mega_cd:admi_nm, household:household_10)
    }
  ) %>% 
  mutate(base_ym = as.character(base_ym))

household_total |> 
  filter(base_ym == "202112")


household_total <- household_total |> 
  tidyr::pivot_longer(cols = starts_with("household"), 
                      names_to = "household_cd", 
                      values_to = "household_cnt") |> 
  mutate(household_cd = household_cd |> str_remove("household_")) |>
  mutate(household_cd = ifelse(household_cd %in% "household", "00", household_cd))

household_total |> 
  count(base_ym)

##==============================================================================
## 04.02. 주소정제
##==============================================================================
## 공공데이터의 특정 시군구 코드 오류 보정
## http://10.10.3.74:9003/2140096/region_report/-/issues/28
# household_total <- household_total %>% 
#   mutate(cty_cd = case_when(
#     cty_cd %in% c("4374") & cty_nm %in% "증평군" ~ "4378",
#     TRUE ~ cty_cd
#   ))
# 
# 
# ## 미추홀구 코드 정제
# household_total <- household_total %>% 
#   mutate(cty_nm = ifelse(cty_cd %in% "2817", "미추홀구", cty_nm))

household_total <- household_total |> 
  mutate(cty_nm = ifelse(str_count(admi_nm, " ") == 1, 
                         paste(cty_nm, str_split(admi_nm, " ", n = 2) |> 
                                 purrr::map_chr(function(x) x[1])), cty_nm)) |> 
  mutate(admi_nm = ifelse(str_count(admi_nm, " ") == 1, 
                          str_split(admi_nm, " ", n = 2) |> 
                            purrr::map_chr(function(x) x[2]), admi_nm)) 


## 경기도 고양시 일산서구와 같은 전국 12개 시 + 구 + 동 체계지역의 구집계 제거
household_total <- household_total %>% 
  filter(!str_detect(admi_cd, "000$")) 


##==============================================================================
## 04.03. 주소정제 로직
##==============================================================================
##------------------------------------------------------------------------------
## 출장소건 제거 (시군구/읍면동 레벨 출장소)
##------------------------------------------------------------------------------
household_total <- household_total %>% 
  filter(!str_detect(cty_nm, "출장소")) %>% 
  filter(!str_detect(admi_nm, "출장소"))

##------------------------------------------------------------------------------
## 읍면동 변경 건
##------------------------------------------------------------------------------
NROW(change_rule) %>% 
  seq() %>% 
  purrr::walk(
    function(x) {
      household_total <<- household_total %>% 
        mutate(admi_cd = ifelse(admi_cd %in% change_rule$from_cd[x], 
                                change_rule$to_cd[x], admi_cd)) %>% 
        mutate(admi_nm = ifelse(admi_cd %in% change_rule$to_cd[x], 
                                change_rule$to_nm[x], admi_nm)) %>% 
        filter(household_cnt != 0)
    }
  ) 

## 경기도 포천시 군내면인데 파주시 군내면으로 오기건 (2020-12)
# household_total <- household_total %>% 
#   filter(!(cty_nm %in% "파주시" & admi_nm %in% "군내면")) %>% 
#   group_by(base_ym, mega_cd, mega_nm, cty_cd, cty_nm, admi_nm, admi_cd,
#            age_group, gender) %>% 
#   summarise(
#     population = sum(population),
#     .groups = "drop")


## 충북 청원군이 청주시로 통폐합된 건에 대해서 시군구 수정 (2014-07-01)
# household_total <- household_total %>% 
#   mutate(cty_nm = ifelse(cty_cd %in% "4371", "청주시", cty_nm)) %>% 
#   mutate(cty_cd = ifelse(cty_cd %in% "4371", "4311", cty_cd))


## 미추홀구 코드 정제
household_total <- household_total %>% 
  mutate(cty_nm = ifelse(cty_cd %in% "2817", "미추홀구", cty_nm)) %>% 
  mutate(admi_cd = ifelse(admi_nm %in% "도화2,3동", "28177610", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_nm %in% "숭의1,3동", "28177520", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_nm %in% "용현1,4동", "28177540", admi_cd)) %>% 
  mutate(admi_nm = ifelse(admi_nm %in% "숭의1.3동", "숭의1,3동", admi_nm)) %>% 
  mutate(admi_nm = ifelse(admi_nm %in% "용현1.4동", "용현1,4동", admi_nm)) %>% 
  mutate(admi_nm = ifelse(admi_nm %in% "도화2.3동", "도화2,3동", admi_nm)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170520", "28177510", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170540", "28177530", admi_cd)) %>%   
  mutate(admi_cd = ifelse(admi_cd %in% "28170560", "28177550", admi_cd)) %>%  
  mutate(admi_cd = ifelse(admi_cd %in% "28170570", "28177560", admi_cd)) %>%  
  mutate(admi_cd = ifelse(admi_cd %in% "28170590", "28177570", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170600", "28177580", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170610", "28177590", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170630", "28177600", admi_cd)) %>%   
  mutate(admi_cd = ifelse(admi_cd %in% "28170660", "28177620", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170670", "28177630", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170680", "28177640", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170690", "28177650", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170700", "28177660", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170710", "28177670", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170720", "28177680", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_cd %in% "28170730", "28177690", admi_cd)) %>%
  mutate(admi_cd = ifelse(admi_cd %in% "28170735", "28177700", admi_cd)) %>%
  mutate(admi_cd = ifelse(admi_cd %in% "28170740", "28177710", admi_cd))


##==============================================================================
## 04.03. 광역시도 및 시군구 레벨 집계
##==============================================================================
##------------------------------------------------------------------------------
## 04.03.01. 시군구 레벨에 집계하여 붙이기
##------------------------------------------------------------------------------
household_cty <- household_total %>% 
  group_by(base_ym, mega_cd, mega_nm, cty_cd, cty_nm, household_cd) %>% 
  summarise_at(vars(household_cnt), sum) 


##------------------------------------------------------------------------------
## 04.03.02. 광역시도 레벨에 집계하여 붙이기
##------------------------------------------------------------------------------
household_mega <- household_total %>% 
  group_by(base_ym, mega_cd, mega_nm, household_cd) %>% 
  summarise_at(vars(household_cnt), sum) 


##==============================================================================
## 04.04. Import from data frame to DBMS
##==============================================================================
##------------------------------------------------------------------------------
## 04.04.01. 읍면동 레벨
##------------------------------------------------------------------------------
household_admi <- household_total |> 
  rename(HSHLD_CD = household_cd) |> 
  rename(HSHLD_CNT = household_cnt) |> 
  mutate(cret_dt = Sys.time()) |> 
  mutate(cret_nm = "bitPublish") |> 
  mutate(mdfy_dt = as.POSIXct(NA)) |> 
  mutate(mdfy_nm = as.character(NA)) |> 
  rename_with(toupper)

dbWriteTable(con, "TB_HSHLD_ADMI", household_admi, overwrite = TRUE)
# dbGetQuery(con, "select * from TB_HSHLD_ADMI limit 5;")


##------------------------------------------------------------------------------
## 04.04.02. 시군구 레벨
##------------------------------------------------------------------------------
household_cty <- household_cty |> 
  rename(HSHLD_CD = household_cd) |> 
  rename(HSHLD_CNT = household_cnt) |> 
  mutate(cret_dt = Sys.time()) |> 
  mutate(cret_nm = "bitPublish") |> 
  mutate(mdfy_dt = as.POSIXct(NA)) |> 
  mutate(mdfy_nm = as.character(NA)) |> 
  rename_with(toupper)

dbWriteTable(con, "TB_HSHLD_CTY", household_cty, overwrite = TRUE)
# dbGetQuery(con, "select * from TB_HSHLD_CTY limit 5;")


##------------------------------------------------------------------------------
## 04.04.03. 광역시도 레벨
##------------------------------------------------------------------------------
household_mega <- household_mega |> 
  rename(HSHLD_CD = household_cd) |> 
  rename(HSHLD_CNT = household_cnt) |> 
  mutate(cret_dt = Sys.time()) |> 
  mutate(cret_nm = "bitPublish") |> 
  mutate(mdfy_dt = as.POSIXct(NA)) |> 
  mutate(mdfy_nm = as.character(NA)) |> 
  rename_with(toupper)

dbWriteTable(con, "TB_HSHLD_MEGA", household_mega, overwrite = TRUE)
# dbGetQuery(con, "select * from TB_HSHLD_MEGA limit 5;")



################################################################################
## 03. Export data from tibbles to table of DBMS
################################################################################
##==============================================================================
## 03.01. Connect DBMS
##==============================================================================
dbDisconnect(con)


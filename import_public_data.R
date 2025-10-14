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
## 02. Import data from files to tibbles
################################################################################
##==============================================================================
## 02.01. 주민등록 인구 및 세대현황
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
        mutate(cty_cd = substr(org_cd, 1, 4)) %>% 
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

## 공공데이터의 특정 시군구 코드 오류 보정
## http://10.10.3.74:9003/2140096/region_report/-/issues/28
population_total <- population_total %>% 
  mutate(cty_cd = case_when(
    cty_cd %in% c("4374") & cty_nm %in% "증평군" ~ "4378",
    TRUE ~ cty_cd
  )) %>% 
  mutate(admi_cd = case_when(
    cty_cd %in% c("4378") & admi_nm %in% "증평읍" ~ "43785250",
    cty_cd %in% c("4378") & admi_nm %in% "도안면" ~ "43785310",    
    TRUE ~ admi_cd
  ))

##==============================================================================
## 주소정제 로직
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
population_total <- population_total %>% 
  filter(!(cty_nm %in% "파주시" & admi_nm %in% "군내면")) %>% 
  group_by(base_ym, mega_cd, mega_nm, cty_cd, cty_nm, admi_nm, admi_cd) %>% 
  summarise(
    population = sum(population),
    household = sum(household),
    pop_per_hosue = round(sum(population) / sum(household), 2),
    pop_male = sum(pop_male),
    pop_female = sum(pop_female),
    male_per_female = round(sum(pop_male) / sum(pop_female), 2),
    .groups = "drop")


## 충북 청원군이 청주시로 통폐합된 건에 대해서 시군구 수정 (2014-07-01)
population_total <- population_total %>% 
  mutate(cty_nm = ifelse(cty_cd %in% "4371", "청주시", cty_nm)) %>% 
  mutate(cty_cd = ifelse(cty_cd %in% "4371", "4311", cty_cd))


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


## 경기도 고양시 일산서구와 같은 전국 12개 시 + 구 + 동 체계지역의 구집계 제거
population_total <- population_total %>% 
  filter(!str_detect(admi_cd, "000$")) 


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

population_total <- population_total |> 
  rename(POPLTN_CNT = population) |> 
  rename(HUSHLD_CNT = household) |> 
  rename(POPLTN_HUSHLD_CNT = pop_per_hosue) |> 
  rename(POPLTN_MALE_CNT = pop_male) |> 
  rename(POPLTN_FEMALE_CNT = pop_female) |> 
  rename(POPLTN_GNDR_RT = male_per_female) |> 
  mutate(cret_dt = Sys.time()) |> 
  mutate(cret_nm = "bitPublish") |> 
  mutate(mdfy_dt = NA) |> 
  mutate(mdfy_nm = NA) |> 
  rename_with(toupper)
	
dbWriteTable(con, "TB_POPHUS_ADMI", population_total, overwrite = TRUE)
# dbGetQuery(con, "select * from TB_POPHUS_ADMI limit 5;")
dbDisconnect(con)


##==============================================================================
## 02.02. 연령별 인구현황 (5세 단위)
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
        mutate(cty_cd = substr(org_cd, 1, 4)) %>% 
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


## 공공데이터의 특정 시군구 코드 오류 보정
## http://10.10.3.74:9003/2140096/region_report/-/issues/28
population_age <- population_age %>% 
  mutate(cty_cd = case_when(
    cty_cd %in% c("4374") & cty_nm %in% "증평군" ~ "4378",
    TRUE ~ cty_cd
  )) %>% 
  mutate(admi_cd = case_when(
    cty_cd %in% c("4378") & admi_nm %in% "증평읍" ~ "43785250",
    cty_cd %in% c("4378") & admi_nm %in% "도안면" ~ "43785310",    
    TRUE ~ admi_cd
  ))


##==============================================================================
## 주소정제 로직
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
population_age <- population_age %>% 
  filter(!(cty_nm %in% "파주시" & admi_nm %in% "군내면")) %>% 
  group_by(base_ym, mega_cd, mega_nm, cty_cd, cty_nm, admi_nm, admi_cd,
           age_group, gender) %>% 
  summarise(
    population = sum(population),
    .groups = "drop")


## 충북 청원군이 청주시로 통폐합된 건에 대해서 시군구 수정 (2014-07-01)
population_age <- population_age %>% 
  mutate(cty_nm = ifelse(cty_cd %in% "4371", "청주시", cty_nm)) %>% 
  mutate(cty_cd = ifelse(cty_cd %in% "4371", "4311", cty_cd))


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


## 경기도 고양시 일산서구와 같은 전국 12개 시 + 구 + 동 체계지역의 구집계 제거
population_age <- population_age %>% 
  filter(!str_detect(admi_cd, "000$")) 


population_age <- population_age %>% 
  group_by(base_ym, mega_cd, mega_nm, cty_cd, cty_nm, admi_cd, admi_nm, 
           age_group, gender) %>% 
  summarise(
    population = sum(population),
    .groups = "drop")


population_age <- population_age |> 
  rename(POPLTN_CNT = population) |> 
  rename(AGEGRP_CD = age_group) |> 
  rename(GNDR_CD = gender) |> 
  mutate(cret_dt = Sys.time()) |> 
  mutate(cret_nm = "bitPublish") |> 
  mutate(mdfy_dt = NA) |> 
  mutate(mdfy_nm = NA) |> 
  rename_with(toupper)

dbWriteTable(con, "TB_POPHUS_ADMI", population_total, overwrite = TRUE)
# dbGetQuery(con, "select * from TB_POPHUS_ADMI limit 5;")
dbDisconnect(con)



##==============================================================================
## 02.03. 가구 소득정보
##==============================================================================
## https://www.bigdata-environment.kr/user/data_market/detail.do?id=8cee0160-2dff-11ea-9713-eb3e5186fb38#!
## 환경데이터마켓 > 사회경제 > 가구 특성정보 (+소득정보)
## 가구_특성정보_(+소득정보)_211203.csv
## 생성날짜: 2020-01-03
## 업데이트 : 2021-12-03 

fname <- "가구_특성정보_(+소득정보)_211203.csv"
fname <- glue::glue("{data_path}/{fname}")

income_amt <- read_csv(fname) %>% 
  mutate(mega_cd = substr(adstrd_cd, 1, 2)) %>% 
  mutate(mega_nm = str_split(adstrd_nm, " ", n = 3) %>%
           purrr::map_chr(function(x) x[1])) %>% 
  mutate(cty_cd = substr(adstrd_cd, 1, 4)) %>% 
  mutate(cty_nm = str_split(adstrd_nm, " ", n = 3) %>%
           purrr::map_chr(function(x) x[2])) %>% 
  mutate(admi_cd = substr(adstrd_cd, 1, 8)) %>% 
  mutate(admi_nm = str_split(adstrd_nm, " ", n = 3) %>%
           purrr::map_chr(function(x) x[3])) %>% 
  select(mega_cd:admi_nm, legaldong_cd, legaldong_nm, ave_income_amt)


## 공공데이터의 특정 시군구 코드 오류 보정
## http://10.10.3.74:9003/2140096/region_report/-/issues/28
income_amt <- income_amt %>% 
  mutate(cty_cd = case_when(
    cty_cd %in% c("4374") & cty_nm %in% "증평군" ~ "4378",
    TRUE ~ cty_cd
  )) %>% 
  mutate(admi_cd = case_when(
    cty_cd %in% c("4378") & admi_nm %in% "증평읍" ~ "43785250",
    cty_cd %in% c("4378") & admi_nm %in% "도안면" ~ "43785310",    
    TRUE ~ admi_cd
  ))


##------------------------------------------------------------------------------
## 읍면동 변경 건
##------------------------------------------------------------------------------
fname <- here::here("R", "report_mart", "data", "reorg_admi_rule.csv")
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
      income_amt <<- income_amt %>% 
        mutate(admi_cd = ifelse(admi_cd %in% change_rule$from_cd[x], 
                                change_rule$to_cd[x], admi_cd)) %>% 
        mutate(admi_nm = ifelse(admi_cd %in% change_rule$to_cd[x], 
                                change_rule$to_nm[x], admi_nm))
    }
  ) 


## 미추홀구 코드 정제
income_amt <- income_amt %>% 
  mutate(cty_nm = ifelse(cty_cd %in% "2817", "미추홀구", cty_nm)) %>% 
  mutate(admi_cd = ifelse(admi_nm %in% "도화2,3동", "28177610", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_nm %in% "숭의1,3동", "28177520", admi_cd)) %>% 
  mutate(admi_cd = ifelse(admi_nm %in% "용현1,4동", "28177540", admi_cd)) %>% 
  mutate(admi_nm = ifelse(admi_nm %in% "숭의1·3동", "숭의1,3동", admi_nm)) %>% 
  mutate(admi_nm = ifelse(admi_nm %in% "용현1·4동", "용현1,4동", admi_nm)) %>% 
  mutate(admi_nm = ifelse(admi_nm %in% "도화2·3동", "도화2,3동", admi_nm)) %>% 
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


## 가구 소득정보 집계레벨 수정 (행정동)
income_amt <- income_amt %>% 
  group_by(mega_cd, mega_nm, cty_cd, cty_nm, admi_cd, admi_nm) %>% 
  summarise(ave_income_amt = mean(ave_income_amt, na.rm = TRUE)) %>% 
  ungroup()


income_amt <- income_amt %>% 
  bind_rows(
    data.frame(
      mega_cd = c("11", "11", "11", "11", "31",
                  "42", "42", "42", "42", "42",
                  "42", "28", "28", "28", "30",
                  "30", "30", "27", "27", "27",
                  "27", "29", "29", "29", "47",
                  "47", "47", "47", "41", "41",
                  "41", "41", "41", "41", "41",
                  "41", "41", "41", "41", "41",
                  "41", "41", "41", "41", "41",
                  "41", "41", "41", "41", "41",
                  "41", "41", "41", "41", "41",
                  "41", "41", "41", "41", "41",
                  "41", "41", "41", "44", "44", "36",
                  "36", "36", "36", "36", "36",
                  "36", "36"),
      mega_nm = c("서울특별시", "서울특별시", "서울특별시", "서울특별시", "울산광역시",
                  "강원도", "강원도", "강원도", "강원도", "강원도",
                  "강원도", "인천광역시", "인천광역시", "인천광역시", "대전광역시",
                  "대전광역시", "대전광역시", "대구광역시", "대구광역시", "대구광역시",
                  "대구광역시", "광주광역시", "광주광역시", "광주광역시", "경상북도",
                  "경상북도", "경상북도", "경상북도", "경기도", "경기도",
                  "경기도", "경기도", "경기도", "경기도", "경기도",
                  "경기도", "경기도", "경기도", "경기도", "경기도",
                  "경기도", "경기도", "경기도", "경기도", "경기도",
                  "경기도", "경기도", "경기도", "경기도", "경기도",
                  "경기도", "경기도", "경기도", "경기도", "경기도",
                  "경기도", "경기도", "경기도", "경기도", "경기도",
                  "경기도", "경기도", "경기도", "충청남도", "충청남도", "세종특별자치시",
                  "세종특별자치시", "세종특별자치시", "세종특별자치시", "세종특별자치시", "세종특별자치시",
                  "세종특별자치시", "세종특별자치시"),
      cty_cd = c("1174", "1174", "1171", "1171", "3171", 
                 "4280", "4278", "4278", "4278", "4278",
                 "4282", "2818", "2818", "2826", "3017",
                 "3020", "3020", "2714", "2714", "2714",
                 "2729", "2915", "2917", "2917", "4719",
                 "4719", "4719", "4772", "4128", "4128",
                 "4128", "4128", "4128", "4128", "4128",
                 "4128", "4128", "4115", "4121", "4122",
                 "4122", "4127", "4139", "4139", "4141",
                 "4145", "4145", "4146", "4146", "4146",
                 "4146", "4146", "4146", "4146", "4148",
                 "4148", "4148", "4159", "4161", "4161",
                 "4161", "4161", "4113", "4413", "4413", "3611",
                 "3611", "3611", "3611", "3611", "3611",
                 "3611", "3611"), 
      cty_nm = c("강동구", "강동구", "송파구", "송파구", "울주군",
                 "양구군", "철원군", "철원군", "철원군", "철원군",
                 "고성군", "연수구", "연수구", "서구", "서구",
                 "유성구", "유성구", "동구", "동구", "동구",
                 "달서구", "남구", "북구", "북구", "구미시",
                 "구미시", "구미시", "군위군", "고양시", "고양시",
                 "고양시", "고양시", "고양시", "고양시", "고양시",
                 "고양시", "고양시", "의정부시", "광명시", "평택시",
                 "평택시", "안산시", "시흥시", "시흥시", "군포시",
                 "하남시", "하남시", "용인시", "용인시", "용인시", 
                 "용인시", "용인시", "용인시", "용인시", "파주시",
                 "파주시", "파주시", "화성시", "광주시", "광주시",
                 "광주시", "광주시", "성남시", "천안시", "천안시", "세종특별자치시",
                 "세종특별자치시", "세종특별자치시", "세종특별자치시", "세종특별자치시", "세종특별자치시",
                 "세종특별자치시", "세종특별자치시"),
      admi_cd = c("11740525", "11740526", "11710690", "11710710", "31710265",
                  "42800315", "42780370", "42780340", "42780350", "42780360",
                  "42820340", "28185850", "28185860", "28260700", "30170593",
                  "30200526", "30200527", "27140742", "27140747", "27140755",
                  "27290617", "29155705", "29170695", "29170697", "47190256",
                  "47190700", "47190535", "47720380", "41285525", "41285526",
                  "41287545", "41287546", "41287600", "41287610", "41281576",
                  "41281577", "41281656", "41150545", "41210655", "41220650",
                  "41220660", "41271590", "41390596", "41390597", "41410630",
                  "41450580", "41450582", "41461525", "41461526", "41463572",
                  "41463575", "41463577", "41465555", "41465585", "41480390",
                  "41480400", "41480410", "41590515", "41610540", "41610550",
                  "41610560", "41610570", "41135655", "44133566", "44133567", "36110555",
                  "36110525", "36110540", "36110560", "36110570", "36110556",
                  "36110580", "36110515"),
      admi_nm = c("상일1동", "상일2동", "잠실4동", "잠실6동", "삼납읍",
                  "국토정중앙면", "임남면", "근동면", "원동면", "원남면",
                  "수동면", "송도4동", "송도5동", "원당동", "도안동",
                  "학하동", "상대동", "안심3동", "안심4동", "혁신동",
                  "유천동", "진월동", "건국동", "신용동", "산동읍",
                  "공단동", "원평동", "삼국유사면", "일산동구 중산1동", "일산동구 중산2동",
                  "일산서구 탄현1동", "일산서구 탄현2동", "일산서구 덕이동", "일산서구 가좌동", "덕양구 삼송1동",
                  "덕양구 삼송2동", "덕양구 행신4동", "호원1동", "일직동", "동삭동",
                  "고덕동", "상록구 성포동", "배곧1동", "배곧2동", "송부동",
                  "감북동", "감일동", "처인구 역북동", "처인구 삼가동", "기흥구 동백1동",
                  "기흥구 동백2동", "기흥구 동백3동", "수지구 죽전3동", "수지구 상현3동", "장단면",
                  "진동면", "진서면", "새솔동", "쌍령동", "탄벌동",
                  "광남1동", "광남2동", "분당구 삼평동", "서북구 불당1동", "서북구 불당2동", "소담동",
                  "해밀동", "종촌동", "보람동", "대평동", "반곡동",
                  "다정동", "새롬동"),
      ave_income_amt = c(6161, 6161, 10346, 10346, 2625,
                         1739, 2038, 2038, 2038, 2038,
                         1780, 5978, 5162, 5737, 4523,
                         5323, 5459, 3294, 3294, 3098,
                         4924, 3173, 3570, 3570, 2019,
                         2855, 2591, 1581, 4554, 4554,
                         4661, 4661, 3381, 3381, 3672,
                         3672, 4543, 3004, 4571, 2830,
                         3211, 3500, 4623, 4623, 5233,
                         3750, 3750, 4164, 4164, 4190,
                         4190, 4190, 6195, 5880, 2140,
                         1721, 2140, 2484, 3092, 3592,
                         3620, 3620, 9818, 6126, 6126, 5402,
                         3693, 5402, 5402, 5402, 5402,
                         5402, 5402),
      stringsAsFactors = FALSE
    )
  )

income_amt %>%
  filter(cty_cd %in% "4113") %>%
  arrange(admi_cd) %>% 
  print(n = 200)

admi_df %>%
  filter(CTY_CD %in% "4113") %>%
  arrange(ADMI_CD) %>% 
  select(1:6) %>%
  as.data.frame()


admi_df %>%
  filter(CTY_CD %in% "4113") %>% 
  as.data.frame() %>% 
  select(ADMI_CD, ADMI_NM) %>% 
  left_join(
    income_amt %>%
      filter(cty_cd %in% "4113") %>% 
      select(ADMI_CD = admi_cd, admi_nm)
  ) %>% 
  arrange(ADMI_CD)

##==============================================================================
## 02.04. 지역별 세대원수별 세대수
##==============================================================================
## https://jumin.mois.go.kr/index.jsp
## 행정안전부 > 주민등록 인구 기타현황 > 지역별 세대원수별 세대수
## 전국 선택 조회 후, 전체시군구현황을 체크하여 다운로드

fnames <- c("202210_202210_주민등록인구기타현황(세대원수별 세대수)_households.xlsx",
            "202112_202112_주민등록인구기타현황(세대원수별 세대수)_households.xlsx",
            "202012_202012_주민등록인구기타현황(세대원수별 세대수)_households.xlsx",
            "201912_201912_주민등록인구기타현황(세대원수별 세대수)_households.xlsx",
            "201812_201812_주민등록인구기타현황(세대원수별 세대수)_households.xlsx",
            "201712_201712_주민등록인구기타현황(세대원수별 세대수)_households.xlsx",
            "201612_201612_주민등록인구기타현황(세대원수별 세대수)_households.xlsx",
            "201512_201512_주민등록인구기타현황(세대원수별 세대수)_households.xlsx",
            "201412_201412_주민등록인구기타현황(세대원수별 세대수)_households.xlsx",
            "201312_201312_주민등록인구기타현황(세대원수별 세대수)_households.xlsx")

household_total <- fnames %>% 
  purrr::map_df(
    function(x) {
      fname <- glue::glue("{data_path}/{x}")
      
      household_total <- readxl::read_xlsx(fname, skip = 2)
      
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
               "household_10" = `10인세대이상`) %>% 
        filter(!str_detect(org_cd, "00000000$")) %>% 
        mutate(base_ym = stringr::str_sub(x, 1, 6)) %>% 
        mutate_at(vars(!matches("org")), function(x) {str_remove(x, ",")}) %>% 
        mutate_at(vars(!matches("org")), as.numeric) %>% 
        mutate(mega_cd = substr(org_cd, 1, 2)) %>% 
        mutate(mega_nm = str_split(org_nm, " ", n = 3) %>%
                 purrr::map_chr(function(x) x[1])) %>% 
        mutate(cty_cd = substr(org_cd, 1, 4)) %>% 
        mutate(cty_nm = str_split(org_nm, " ", n = 3) %>%
                 purrr::map_chr(function(x) x[2])) %>% 
        select(base_ym, mega_cd:cty_nm, household:household_10)
    }
  ) %>% 
  mutate(base_ym = as.character(base_ym))

## 공공데이터의 특정 시군구 코드 오류 보정
## http://10.10.3.74:9003/2140096/region_report/-/issues/28
household_total <- household_total %>% 
  mutate(cty_cd = case_when(
    cty_cd %in% c("4374") & cty_nm %in% "증평군" ~ "4378",
    TRUE ~ cty_cd
  ))


## 미추홀구 코드 정제
household_total <- household_total %>% 
  mutate(cty_nm = ifelse(cty_cd %in% "2817", "미추홀구", cty_nm))




################################################################################
## 03. Export data from tibbles to table of DBMS
################################################################################
##==============================================================================
## 03.01. Connect DBMS
##==============================================================================
oraConnect()


##==============================================================================
## 03.02. 주민등록 인구 및 세대현황
##==============================================================================
oraWriteTable("TMP_POPULATION_TOT", population_total, overwrite = TRUE)


##==============================================================================
## 03.03. 연령별 인구현황 (5세 단위)
##==============================================================================
oraWriteTable("TMP_POPULATION_SUB", population_age, overwrite = TRUE)


##==============================================================================
## 03.04. 가구 소득정보
##==============================================================================
oraWriteTable("TMP_INCOME_INFO", income_amt, overwrite = TRUE)


##==============================================================================
## 03.04. 지역별 세대원수별 세대수
##==============================================================================
oraWriteTable("TMP_HOUSEHOLD", household_total, overwrite = TRUE)


##==============================================================================
## 03.05. Disconnect DBMS
##==============================================================================
oraClose()


oraSql("select * from TMP_POPULATION_TOT where rownum <= 10")
oraSql("select count(*) from TMP_POPULATION_TOT")


population_age %>% 
  group_by(base_ym, admi_cd, admi_nm) %>% 
  summarise(population = sum(population)) %>% 
  filter(admi_cd %in% "11560610")


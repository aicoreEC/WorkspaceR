# 
# 10일차
#
# 0. Open
# 1. Review
# 2. Topic
#   2.1 개인 프로젝트 발표
#   2.2 텍스트마이닝( word cloud )
# 3. Q & A
# 4. Next
# 5. Close
#
# 1. Review
#


#
# 2.1 개인 프로젝트 발표
#



#
# 단계구분도( Choropleth Map ) : 지역별 통계치를 색깔의 차이로 표현한 지도 시각화 도구
#
install.packages( "ggiraphExtra" )
install.packages( "maps" )
install.packages( "mapproj" )

library( tibble )

library( ggiraphExtra )

library( maps )
library( mapproj )

library( ggplot2 )


dim( USArrests )
str( USArrests )
head( USArrests )

# tibble 패키지의 rownames_to_column()를 이용하여 data( 행 )을 변수( 열 )로
# 변경해주는 함수
crime <- rownames_to_column( USArrests, var = "state" )
crime$state <- tolower( crime$state ) # 변수의 내용을 소문자로 변경

dim( crime )
str( crime )

# maps 패키지의 미국 주별 위경도를 나타낸 state데이터를 ggplot2의 map_data()를
# 이용해 data frame 생성
states_map <- map_data( "state" )
str( states_map )

# 단계 구분도 작성
ggChoropleth( data = crime,              # 지도에 표현할 데이터
              aes( fill = Murder,        # 색깔로 표현할 변수
                   map_id = state ),     # 지역 기준 변수
              map = states_map )         # 지도 데이터

# 인터렉티브 단계 구분도
ggChoropleth( data = crime,              # 지도에 표현할 데이터
              aes( fill = Murder,        # 색깔로 표현할 변수
                   map_id = state ),     # 지역 기준 변수
              map = states_map,          # 지도 데이터
              interactive = T )          # 마우스 움직임에 반응

# 한국 지도를 이용한 단계 구분도
install.packages("devtools")
install.packages( "GISTools" )

devtools::install_github("cardiomoon/kormaps2014")

devtools::install_github("cardiomoon/moonBook2")

library(kormaps2014)
library(moonBook2)

library(GISTools)

# 데이타 areacode
rm(list = ls())
areacode

areacode$name <- iconv(as.character(areacode$name),  
                       from = "UTF-8",
                       to = "CP949")
areacode$name1 <- iconv(as.character(areacode$name1),  
                        from = "UTF-8",
                        to = "CP949")
areacode2 <- lapply(areacode, function(x) factor(x, levels = x))
areacode2 <- areacode2 %>%
  as_tibble()

areacode2

# 한국행정지도 데이타
# kormap1 : 2014년 한국행정지도(시도별)
# kormap2 : 2014년 한국행정지도(시군구별)
# kormap3 : 2014년 한국행정지도(읍면동별)

# 시도 지도 데이터
str(kormap1)

# kormap1$SIDO_NM <- kormap1$SIDO_NM %>%
#         as.character() %>% #> `factor`를 먼저 `character`롤 변환. `factor`는 사실상 1, 2, 3, ...
#         iconv(from = "CP949", to = "UTF-8") %>% #> `CP949`로 인코딩되어 있는 것을 `UTF-8`으로
#         factor(levels = areacode2$name) #> 앞에서 수정한 `areacode2` 활용. `areacode`는 어떻게?
#> 위의 piping 을 한줄로 처리하면
kormap1$name <- factor(iconv(as.character(kormap1$name), 
                             from = "UTF-8", 
                             to = "CP949"), 
                       levels = areacode2$name)
str(kormap1)

# kormap1$name1 <- kormap1$name1 %>%
#         as.character() %>% 
#> `factor`를 먼저 `character`롤 변환. `factor`는 사실상 1, 2, 3, ...
#         iconv(from = "CP949", to = "UTF-8") %>% 
#> `CP949`로 인코딩되어 있는 것을 `UTF-8`으로
#         factor(levels = areacode2$name,
#                labels = areacode2$name1) 
#> 앞에서 수정한 `areacode2` 활용. `labels = ` 없이 `levels = areacode2$name1` 하면 어떻게 될까? 
#> 위의 piping 을 한줄로 처리하면
kormap1$name1 <- factor(iconv(as.character(kormap1$name1), 
                              from = "CP949", 
                              to = "UTF-8"), 
                        levels = areacode2$name,
                        labels = areacode2$name1)
str(kormap1)

kormap1$name <- kormap1$name %>%
  as.character %>%
  factor(levels = areacode2$name)
str(kormap1)

kormap1$SP_ID <- kormap1$SP_ID %>%
  as.character %>%
  factor(levels = 0:(length(areacode2$name) - 1))
str(kormap1)

saveRDS(kormap1, file = "./kormap1.RDS")

# 시군구 행정지도 데이터 수정
str(kormap2)

# 한글 인코딩 문제
kormap2_2 <- kormap2
kormap2_2$name <- kormap2_2$name %>%
  as.character() %>%
  iconv(from = "UTF-8", to = "CP949")
kormap2_2$sigungu_nm2 <- kormap2_2$sigungu_nm %>%
  as.character() 
str(kormap2_2)

# 행정지역코드 순서 확인
levels(kormap2_2$sigungu_cd)

unique(kormap2_2$sigungu_cd)

o <- order(kormap2_2$sigungu_cd)
kormap2_2 <- kormap2_2[o, ]

# 중복등장 지역명을 시도명 붙여 구분

#> 코드별로 그룹을 나누고 각 그룹의 맨 첫 레코드만 걸러냄.
#> `ggplot2`를 load하면서 `MASS` 패키지를 함께 불러오다 보니 충돌이 생기게 된다. 
#> "Error in select() : unused argument"가 뜨면,  `dplyr::select`로 명시하거나 `select <- dplyr::select`로 선언하고 `select()`를 실행하면 된다.
library( dplyr )

cd_nm <- kormap2_2 %>%
  #        as_tibble() %>%
  group_by(sigungu_cd) %>%
  #        filter(row_number() == 1) %>%
  slice(1) %>%
  #        .[, c("sigungu_cd", "sigungu_nm2")]
  select(c("sigungu_cd", "sigungu_nm2")) 
#        sample_n(size = 1) 

print(cd_nm, n = Inf)


#> 중복등장하는 지역명 위치 확인. `.`의 활용방법에 유의.
dup_nm_idx <- cd_nm %>%
  #         select("sigungu_nm2") %>% 
  .[, "sigungu_nm2"] %>%
  duplicated() %>%
  which()
dup_nm_idx







install.packages( "extrafont" )
library(ggplot2)
library(extrafont)

theme_set(theme_gray(base_family = ""))

ggplot(data = data_sido, 
       aes(map_id = code, 
           fill = 총인구_명)) +
  geom_map(map = kormap1,
           colour = "black", 
           size = 0.1) +
  expand_limits(x = kormap1$long,
                y = kormap1$lat) +
  scale_fill_gradientn(colours = c('white','orange','red')) +
  ggtitle("2015년도 시도별 인구분포도") +
  coord_map()


#
# 2.2 텍스트 마이닝( word cloud )
# 
# 
install.packages( "memoise" )
install.packages( "KoNLP" )

#-----------------------------------------------------------------------------------
# https://web.facebook.com/notes/r-korea-krugkorean-r-user-group/konlp-%EC%84%A4%EC%B9%98-%EC%9D%B4%EC%8A%88-%EA%B3%B5%EC%9C%A0/1847510068715020/?_rdc=1&_rdr
#
# R 64bit 실행(rstudio 실행도 가능) 

# java, rJava 설치 install.packages("multilinguer")
# 이때 mac 사용자는 데스크탑 비밀번호를 물어봅니다. 입력해줘야 설치가 진행됩니다.
install.packages("multilinguer")

library(multilinguer)
install_jdk()
# 위 함수에서 에러가 발생하면 알려주세요
# https://github.com/mrchypark/multilinguer/issues

# 의존성 패키지 설치( 설치 안함 )
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")

# github 버전 설치
install.packages("remotes")
# 64bit 에서만 동작합니다.
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
#-----------------------------------------------------------------------------------

library( KoNLP )
useNIADic()

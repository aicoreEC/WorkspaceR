# 
# 11일차
#
# 0. Open
# 1. Review
# 2. Topic
#   2.1 단계 구분도
#   2.2 텍스트마이닝( word cloud )
#   2.3 조별 1차 프로젝트
# 3. Q & A
# 4. Next
# 5. Close
#
# 1. Review
#


#
# 2.1 단계 구분도
#
# 단계 구분도( Choropleth Map ) : 지역별 통계치를 색깔의
#      차이로 표현한 지도 시각화 도구
#
install.packages( "ggiraphExtra" ) # 단계구분도 작성을 위한 패키지
install.packages( "maps" ) # R에 내장된 미국 주별 위/경도 데이터
install.packages( "mapproj" ) # 위도( latitude ) / 경도( longitude ) 표시

library( tibble ) # dplyr 패키지 설치시 자동 설치, 행을 변수로 변환 함수 

library( ggiraphExtra )
library( maps )
library( mapproj )

library( ggplot2 )

dim( USArrests )
str( USArrests )
head( USArrests )

# tibble 패키지 rownames_to_column()를 이용하여 data( 행 )를 변수( 열 )로
# 변경
crime <- rownames_to_column( USArrests, var = "state" )
crime$state <- tolower( crime$state )

dim( crime )
str( crime )

# map 패키지의 미국 주별 위/경도를 나타내는 state 데이터를 ggplot2의 
# map_data()를 이용하여 data frame 생성
states_map <- map_data( 'state' )
str( states_map )

# 단계 구분도 작성
ggChoropleth( data = crime,
              aes( fill = Murder,
                   map_id = state ),
              map = states_map,
              interactive = T )
#ggChoropleth( data = crime, # 지도에 표시할 데이터
#               aes( fill = Murder, # 색깔로 표현할 변수
#               map_id = state ), # 지역 지준 변수
#               map = states_map ) # 지도 데이터

# 대한민구 지도 데이터를 이용한 단계 구분도
install.packages( "devtools" ) # R용 패키지 개발용 함수 모음

# R 공식 저장소( CRAN )에 없는 경우 원하는 패키지 설치 방법
devtools::install_github( "cardiomoon/kormaps2014" )

library( kormaps2014 )
library( dplyr )

# kormaps2014 지도 데이터
# kormap1 : 한국 행정 지도( 시도별 )
# kormap2 : 한국 행정 지도( 시군구별 )
# kormap3 : 한국 행정 지도( 읍면동별 )
#
# kormaps2014 내장 dataset
# korpop1 : 2015년 인구 센서스 데이터( 시도별 )
# korpop2 : 2015년 인구 센서스 데이터( 시군구별 )
# korpop3 : 2015년 인구 센서스 데이터( 읍면동별 )

# changeCode() : 인코딩을 cp949로 변환, 원래 korpop1에 UTF-8로 
#                인코딩 되어 있으나, 윈도우에서는 한글 깨짐 발생
str( changeCode( korpop1 ) )

# 한글 변수명 변경
korpop1 <- korpop1 %>% 
  rename( pop = 총인구_명,
          name = 행정구역별_읍면동 )

str( changeCode( kormap1 ) )

korpop1$name <- iconv( korpop1$name, "UTF-8", "CP949" )
ggChoropleth( data = korpop1,
              aes( fill = pop, 
                   map_id = code,
                   tooltip = name ),
              map = kormap1,
              interactive = T )

#ggChoropleth( korpop2, kormap2, fillvar = "남자_명" )

#
# 2.2 텍스트 마이닝( word cloud )
# 
# 
install.packages( "memorise" )
install.packages( "KoNLP" )

# KoNLP : Korean Netural Language Processing( 한글 자연어 처리 )
install.packages( "multilinguer" )

library( multilinguer )
install_jdk()

# KoNLP 패키지 설치
install.packages( "remotes" )
remotes::install_github( "haven-jeon/KoNLP", upgrade = "never",
                        INSTALL_opts = c( "--no-multiarch" ) )

#devtools::install_github('haven-jeon/KoNLP')

library( KoNLP ) # R 용 한글 자연어 처리 패키지
useNIADic()      # 사용자 사전 설정


#
# Data Mining : 대규모로 저장된 데이터안에서 체계적이고 자동적으로
#               통계적 규칙이나 패턴을 찾아내는 것을 말하며
#               KDD( Knowledg-discovery in databases,
#                    데이터베이스 속의 지식 발견 )
#
# Text Mining : 비정형 데이터 마이닝의 유형중 하나
#               비정형 / 반정형 데이터에 대하여 자연어 처리 기술과
#               문서 처리 기술을 적용하여 유용한 정보를 추출, 가공하는
#               목적으로 하는 기술
#
# Word Cloud : 텍스트 데이터를 분석하는 대표기술, 
# (BoW)        대상 데이터에서 단어( 주로 명사 )를 추출하고 단어들의
#  Bag         출현 빈도수를 계산하여 시각화하는 도구
#  of          출현 빈도수가 높은 단어는 그만큼 중요하거나 관심도가
#  Word        높다는 것을 의미
#              Word Cloud에서는 단어의 출현 빈도수가 높을수록 큰 글시로
#              표현
#
# 한글 Word Cloud 절차
# 1. Java 실행 환경 구축( JRE, Java Run-time Environment )
# 2. KoNLP 패키지 설치 / Load
# 3. 사용자 사전 설정
#
# 4. 자료 수집( Text 자료 )
#    4.1 text file 형태로 수집      : 메모장으로 읽을 수 있으면 text file
#    4.2 web scraping을 이용한 수집
#
# 5. 명사 추출
# 6. 추출한 명사의 빈도수 계산
# 7. 빈도수순으로 Sort( Descending )
# 8. 한글 Word Cloud 표현
#
# R에서 한글 WordCloud를 이용하기 위해서는 Java 실행환경( JRE )피료
Sys.setenv( JAVA_HOME = "D:\\Java\\jdk1.8.0_251" )

install.packages( "wordcloud" )     # Word Cloud용 패키지
install.packages( "wordcloud2" )    # Word Cloud용 패키지
install.packages( "RColorBrewer" )  # 색상 팔레트

library( wordcloud )
library( wordcloud2 )
library( RColorBrewer )

library( dplyr )
library( ggplot2 )

# 4. 자료 수집( Text 자료 )
setwd( "D:\\Workspace\\2020BigDataMaestro\\DaejeonWorkR" )
text <- readLines( 'mis_document.txt', encoding = 'UTF-8' )
text

# 사용자 한글 사전 '우리말씀' 로딩
buildDictionary( ext_dic = 'woorimalsam' )
pal2 <- brewer.pal( 8, 'Dark2' ) # 색상 팔레트 생성

# 5. 명사 추출
noun <- sapply( text, extractNoun, USE.NAMES = F ) # 명사 추출
noun

# 6. 추출한 명사의 빈도수 계산
noun2 <- unlist( noun ) # list <- vector 변환
wordcount <- table( noun2 )

# 7. 빈도수순으로 Sort( Descending )
sort.noun <- sort( wordcount, decreasing = T )[ 1:10 ]
sort.noun
sort.noun <- sort.noun[ -1 ]
barplot( sort.noun, names.arg = names( sort.noun ),
         col = 'steelblue', main = '빈도수 높은 단어',
         ylab = '단어 빈도수 ' )

# 8. 한글 Word Cloud 작성
pal3 <- brewer.pal( 9, 'Blues' )[ 5:9 ] # 색상 팔레트 생성
wordcloud( names( wordcount ),
           freq = wordcount,
           main.freq = 3,
           random.order = F,
           rot.per = .1,
           colors = pal3 )
#wordcloud( names( wordcount ), # 단어
#           freq = wordcount,   # 단어 빈도
#           main.freq = 3,      # 단어폰트크기(최대,최소)
#           random.order = F,   # 단어최소빈도
#           rot.per = .1,       # 90도 회전 단어 비율
#           colors = pal3 )     # 단어색

# 한글 Word Cloud를 위한 전처리
# 1. 불필요한 단어 삭제
# 2. 생략된 단어를 사전에 등재
buildDictionary( ext_dic = 'woorimalsam',
                 user_dic = data.fram( '정치', 'ncn' ),
                 replace_usr_dic = T )
noun <- sapply( text, extractNoun, USE.NAMES = F )
noun2 <- unlist( noun2 )

# 1. 불필요한 단어 삭제( 불용어, stop word )
noun2 <- noun2[ nchar( noun2 ) > 1 ] # 빈도수가 2이상
noun2 <- gsub( '하지', '', noun2 )
noun2 <- goub( '때문', '', noun2 )
wordcount <- table( noun2 )
wordcloud( names( wordcount ), freq = wordcount,
           scale = c( 6, 0.7 ), main.freq = 3,
           random.order = F, rot.per = .1, colors = pal3 )

wordcloud2( wordcount, color = 'random-light', 
            backgroundColor = 'black' )

# 모양 변경
wordcloud2( wordcount, fontFamily = '맑은 고딕',
            size = 1.2, color = 'random-lignt',
            backgroundColor = 'black', 
            shape = 'star' )

# 색상변경
wordcloud2( demoFreq, size = 1.6,
            color = rep_len( c( 'red', 'blue' ),
            nrow( wordcount ) ) )
wordcloud2( wordcount, size = 1.6,
            color = rep_len( c( 'red', 'blue' ),
                             nrow( wordcount ) ) )

# 일정 방향 정렬
wordcloud2( wordcount, 
            minRotation = -pi / 6,
            maxRotation = -pi / 6,
            rotateRatio = 1 )













#
# 2.3 조별 1차 프로젝트
#

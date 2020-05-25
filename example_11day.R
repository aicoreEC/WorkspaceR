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
# 2.1 개인 프로젝트 발표
#



#
# 단계구분도( Choropleth Map ) : 지역별 통계치를 색깔의 차이로 표현한 지도 시각화 도구
#
install.packages( "ggiraphExtra" )   # 단계 구분도 작성을 위한 패키지
install.packages( "maps" )           # R에 내장된 미국 주별 위/경도 데이터( state )
install.packages( "mapproj" )        # 위도( latitude ) / 경도( longitude ) 표시 패키지

library( tibble )                    # dplyr 패키지 설치시 자동 설치, 행을 변수로 변경하는 함수 사용

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
install.packages("devtools")             # R용 패키지 개발을 위한 함수 모음, 
                                         # developing statistical software
#install.packages( "GISTools" )

# kormaps2014 패키지를 이용하면 대한민국의 지역 통계 데이터와 지도 데이터 사용
# https://github.com/cardiomoon/kormaps2014
devtools::install_github("cardiomoon/kormaps2014")

library( kormaps2014 )

library( dplyr )

# kormaps2014 패기지 지역별 인구통계 데이터
# korpop1 : 2015년 센서스 데이터( 시도별 )
# korpop2 : 2015년 센서스 데이터( 시군구별 )
# korpop3 : 2015년 센서스 데이터( 읍면동별 )

# changeCode() : 인코딩을 cp949로 변환, 원래 korpop1이 UTF-8이라 윈도우에서 한글 깨짐 발생
str( changeCode( korpop1 ) )             

# 한글 변수명 변경
korpop1 <- korpop1 %>% rename( pop = 총인구_명,
                               name = 행정구역별_읍면동 )

# 대한민국 시도 지도 데이터 준비
#
# kormap1 : 2014년 한국 행정 지도( 시도별 )
# kormap2 : 2014년 한국 행정 지도( 시군구별 )
# kormap3 : 2014년 한국 행정 지도( 읍면동별 )
str( changeCode( kormap1 ) )

# korpop1 데이터의 시도별 인구 변수와 kormap1의 시도별 위경도 데이터를 이용해 단계 구분도
# 지역 기준이 되는 code 변수가 숫자 코드로 되어 있기 때문에 지도에 마우스 커서를 올리면 코드가 표시
# 코드 대신 지역명이 표시되도록 tooltip에 지역명 변수 name을 지정
korpop1$name <- iconv(korpop1$name, "UTF-8","CP949") # 한글 깨짐 해결
ggChoropleth( data = korpop1,           # 지도에 표현할 데이터
              aes( fill = pop,          # 색깔로 표현할 데이터
                   map_id = code,       # 지역 기준 변수
                   tooltip = name ),    # 지도 위에 표시할 지역명
              map = kormap1,            
              interactive = T )         

# 대한민국 시도별 결핵 환자 수 단계 구분도
# kormaps2014 패키지에는 지역별 결핵 환자 수에 대한 정보를 담고 있는 tbc 데이터
#  tbc 데이터의 NewPts(결핵 환자 수) 변수를 이용해 시도별 결핵 환자 수 단계 구분도
str(changeCode(tbc))

tbc$name <- iconv(tbc$name, "UTF-8","CP949")

ggChoropleth(data = tbc,          # 지도에 표현할 데이터
             aes(fill = NewPts,   # 색깔로 표현할 변수
                 map_id = code,   # 지역 기준 변수
                 tooltip = name), # 지도 위에 표시할 지역명
             map = kormap1,       # 지도 데이터
             interactive = T)     # 인터랙티브


# 
#
# 2.2 텍스트 마이닝( word cloud )
# 
# 
# 메모이제이션(memoization)은 컴퓨터 프로그램이 동일한 계산을 반복해야 할 때, 
# 이전에 계산한 값을 메모리에 저장함으로써 동일한 계산의 반복 수행을 제거하여 
# 프로그램 실행 속도를 빠르게 하는 기술
# memoise 패키지에 함수로 구현
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

library( KoNLP ) # R 에서 한글 처리를 위한 패키지
useNIADic()      # 사용자 사전 설정


#
# Data Mining : 대규모로 저장ㄷ된ㄴ 데이터 안에서 체계적이고 자동적으로 통계적 규칙이나
#               패턴을 찾아내는 것을 말하며 KDD( Knowledg-discovery in databases, 
#               데이터베이스 속의 지식 발견 )
#
# Text Mining : 비정형 데이터 마이닝의 유형 중 하나
#               비정형 / 반정형 데이터에 대하여 자연어 처리 기술과 문서 처리 기술을 적용하여
#               유용한 정보를 추출, 가공하는 목적으로 하는 기술
#
# Word Cloud : 텍스트 데이터를 분석하는 대표 기술, 대상 데이터에서 단어( 주로 명사 )를 추출하고
#              단어들의 출현 빈도수를 계산하여 시각화하는 기능
#              출현 빈도수가 높은 단어는 그만큼 중요하거나 관심도가 높다는 것을 의미
#              Word Cloud에서는 단어의 출현 빈도수가 높을수록 큰 글씨로 표현
#
#
# 워드클라우드(Word cloud) <- 텍스트 마이닝의 한 종류
#
# 한글 워드 클라우드 절차
# 1. Java실행 환경 구축
# 2. 자료 수집(Text자료)
#    2.1 text file 형태로 수집                 <- 메모장으로 열어서 파일을 읽으면 텍스트 파일
#    2.2 web scraping을 이용하여 수집
# 3.명사 추출
#

# R에서 한글 Word Cloud를 이용하기 위해서는 Java 실행 환경( Java Runtime Environment ) 필요
Sys.setenv( JAVA_HOME = "D:\\Java\\jdk1.8.0_181" ) # JRE 이용 설정


#필요시 설치
install.packages('wordcloud')       #워드 클라우드
install.packages("wordcloud2")      #워드 클라우드
install.packages("KoNLP")           #한국어 처리
install.packages("RColorBrewer")    #색상 선택

library(wordcloud)
library(wordcloud2)
library(KoNLP)
library(RColorBrewer)

library(dplyr)
library(ggplot2)

setwd('D:\\Workspace\\2020BigDataMaestro\\DaejeonWorkR')
text <- readLines('mis_document.txt')   #텍스트에 마지막 공백줄이 없으면 에러가 뜸
text

#'우리말씀' 한글사전 로딩
buildDictionary(ext_dic = 'woorimalsam')
pal2 <- brewer.pal(8, 'Dark2')   #색상 팔레트 생성
noun <- sapply(text, extractNoun, USE.NAMES = F)  #명사추출 
noun

#4.추출된 단어(주로 명사)에 대한 빈도수 계산 및 시각화
noun2 <- unlist(noun)                         #list <- vector로 변환
wordcount <- table(noun2)
sort.noun <- sort(wordcount, decreasing = T)[1:10]
sort.noun
sort.noun <- sort.noun[-1]
barplot(sort.noun, names.arg= names(sort.noun),
        col='steelblue', main = '빈도수 높은 단어',
        ylab='단어 빈도수')


df <- as.data.frame(sort.noun)
df
ggplot(df, aes(x=df$noun2,y=df$Freq)) +
  geom_bar(stat="identity",
           width=0.7,
           fill='steelblue') +
  ggtitle('빈도수 높은 단어') +
  theme(plot.title = element_text(size = 25,
                                  face='bold',
                                  colour = "steel blue",
                                  hjust=0,
                                  vjust = 1)) + 
  labs(x='명사', y='단어 빈도수') +
  geom_text(aes(label=df$Freq),hjust=-0.3) +           
  coord_flip()                                        

#5.워드 클라우드 작성
# wordcloud(names(wordcount),             #단어 
#           freq = wordcount,             #단어 빈도
#           scale = c(6, 0.7),            #단어폰트크기(최대, 최소)
#           main.freq=3,                  #단어최소빈도
#           random.order = F,             #단어출력위치
#           rot.per = .1,                 #90도 회전 단어 비율
#           colors = pal2)                #단어색

pal3 <- brewer.pal(9, 'Blues')[5:9]   #색상 팔레트 생성

wordcloud(names(wordcount),
          freq = wordcount,
          scale = c(6, 0.7),
          main.freq=3,
          random.order = F,
          rot.per = .1,
          colors = pal3)

#6. 전처리 과정 수행
# 6.1 불필요한 단어 삭제
# 6.2 생략된 단어를 사전에 등재
buildDictionary(ext_dic = 'woorimalsam',
                user_dic = data.frame('정치', 'ncn'),
                replace_usr_dic=T)
noun <- sapply(text, extractNoun, USE.NAMES = F)
noun2 <- unlist(noun)

#6.1불필요한 단어 삭제
noun2 <- noun2[nchar(noun2)>1]
noun2 <- gsub('하지', '',noun2)
noun2 <- gsub('때문', '',noun2)
wordcount <- table(noun2)
wordcloud(names(wordcount),
          freq = wordcount,
          scale = c(6, 0.7),
          main.freq=3,
          random.order = F,
          rot.per = .1,
          colors = pal3)

#애국가 형태소 분석
#
library(KoNLP)
#useSystemDic()
useSejongDic()
useNIADic()

# 애국가 가사:
# https://mois.go.kr/frt/sub/a06/b08/nationalIcon_3/screen.do

#1.사전설정
useSejongDic()

#2.텍스트 데이터 가져오기
setwd('D:\\Workspace\\2020BigDataMaestro\\DaejeonWorkR')
word_data <- readLines( '애국가(가사).txt', encoding="UTF-8" )
word_data

#3.명사추출

worddata2 <- sapply(word_data, extractNoun, USE.NAMES = F)
worddata2

#3.1 제대로 추출되지 않은 단어를 사용자 사전에 등록
add_words <- c('백두산','남산','철갑','가을','하늘','달')
buildDictionary(user_dic = 
                  data.frame(add_words, rep('ncn',length(add_words))),
                replace_usr_dic = T)
get_dictionary('user_dic')

#3.2 단어 추가후 다시 명사 추출
word_data2 <- sapply(word_data, extractNoun, USE.NAMES = F)
word_data2

#4. 행렬을 "벡터"로 변환
undata <- unlist(word_data2)
undata

#5. 사용빈도 확인
word_table <- table(undata)
word_table


#6.필터링 : 두 글자 이상 단어만 선별, 공백이나 한 자리 문자를 걸러냄
undata2 <- undata[nchar(undata)>=2]
undata2
word_table2 <- table(undata2)
word_table2

#7.데이터 정렬
sort(word_table2, decreasing = T)

#애국가 형태 분석 완료
#가장 기본적인 전처리만 수행, 100%정확한 데이터라 볼 수 없음

#8.word cloud 작성 후 분석
library(wordcloud2)
wordcloud2(word_table2)

wordcloud2(word_table2,
           color = 'random-light',
           backgroundColor = 'black')
#8.2 모양 변경
wordcloud2(word_table2,
           fontFamily = '맑은 고딕',
           size=1.2, color='random-light',
           backgroundColor='black',
           shape='star')

#8.3 선택 색상 반복
wordcloud2(word_table2, size = 1.6,
           color = rep_len(c('red','blue'),
                           nrow(word_table2)))

wordcloud2(demoFreq, size = 1.6,
           color = rep_len(c('red','blue'),
                           nrow(word_table2)))

#8.4 일정 방향 정렬
wordcloud2(word_table2,
           minRotation = -pi/6,
           maxRotation = -pi/6,
           rotateRatio = 1)
wordcloud2(demoFreq,
           minRotation = -pi/6,
           maxRotation = -pi/6,
           rotateRatio = 1)

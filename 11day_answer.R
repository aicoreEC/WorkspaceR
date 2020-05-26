#
# 11day_answer.R
#
# 11일차 Word Cloud 실습
#
#문1)
#20대 국회 개원 여·야 3당 대표 국회연설문에 대해 각각 워드클라우드를
#작성하시오.
#예제소스 파일은 ‘ex_10-1.txt’, ‘ex_10-2.txt’, ‘ex_10-3.txt’이다.
Sys.setenv( JAVA_HOME = 'D:\\Java\\jdk1.8.0_181' )

library( wordcloud ) 
library( wordcloud2 )
library( KoNLP ) 
library( RColorBrewer ) 

setwd( 'D:\\Workspace\\2020BigDataMaestro\\DaejeonWorkR' )

# ex_10-1.txt
text <- readLines( "ex_10-1.txt", encoding ="UTF-8" )
text

buildDictionary( ext_dic = "woorimalsam" ) 
noun <- sapply( text, extractNoun, USE.NAMES = F )
noun 

noun2 <- unlist(noun)

# 무의미한 단어 제거
noun2 <- noun2[ nchar(noun2) > 1 ]
noun2 <- gsub( "들이", "", noun2 )
noun2 <- gsub( "10", "", noun2 )
noun2 <- gsub( "이명", "", noun2 )

wordcount <- table( noun2 )
wordcloud2( wordcount, 
            size = 1.2, color = rep_len( c( 'red', 'steelblue'),
                                         nrow( wordcount ) ),
            shape = 'diamond' )


# ex_10-2.txt
useSejongDic()

text <- readLines( "ex_10-2.txt", encoding ="UTF-8" )
text

noun <- sapply( text, extractNoun, USE.NAMES = F )
noun 

# 단어 등록
add_words <- c( '새누리당', '부패', '비정상', '김영란법', 
                '원장님', '북한', '북한당국', '사드', '배치' )
buildDictionary( user_dic = 
                   data.frame( add_words,rep( 'ncn', length( add_words ) ) ),
                 replace_usr_dic = T )
get_dictionary( 'user_dic' )

noun <- sapply( text, extractNoun, USE.NAMES = F )
noun 


noun2 <- unlist(noun)

# 무의미한 단어 제거
noun2 <- noun2[ nchar(noun2) > 1 ]
del_words <- c( "누구", "하지", "이명", "여러분", "동지",
                "발걸음", "시행", "입장", "심각", "상황",
                "해서", "적극", "앞장", "보완", "진행", 
                "불구", "사실상" )
for ( str in del_words ) {
  noun2 <- gsub( str, "", noun2 )
}

wordcount <- table( noun2 )
wordcloud2( wordcount, 
            size = 1.2, color = rep_len( c( 'red', 'steelblue'),
                                         nrow( wordcount ) ),
            shape = 'triangle' )

# ex_10-3.txt
text <- readLines( "ex_10-3.txt", encoding ="UTF-8" )
text

noun <- sapply( text, extractNoun, USE.NAMES = F )
noun 

noun2 <- unlist(noun)

# 무의미한 단어 제거
noun2 <- noun2[ nchar(noun2) > 1 ]
del_words <- c( "누구", "하지", "이명", "여러분", "동지",
                "발걸음", "시행", "입장", "심각", "상황",
                "해서", "적극", "앞장", "보완", "진행", 
                "불구", "사실상" )
for ( str in del_words ) {
  noun2 <- gsub( str, "", noun2 )
}

wordcount <- table( noun2 )
wordcloud2( wordcount, 
            size = 1.2, color = rep_len( c( 'red', 'steelblue'),
                                         nrow( wordcount ) ),
            backgroundColor = 'black',
            shape = 'pentagon' )

#문2)
#스티브 잡스의 스탠포드 대학 졸업식 연설문에 대해 워드클라우드를 작성
#하시오.
#Tip. 예제소스 파일은 ‘ex_10-4.txt’이다.
useNIADic()

text <- readLines( "ex_10-4.txt", encoding = "UTF-8" )
text

noun <- sapply( text, extractNoun, USE.NAMES = F )
noun 
noun2 <- unlist(noun)

noun2 <- noun2[ nchar(noun2) > 1 ]
wordcount <- table( noun2 )

wordcloud2( wordcount,
            color = rep_len( c( 'red', 'blue'),
                             nrow( wordcount ) ),
            minRotation = -pi / 6,
            maxRotation = -pi / 6,
            rotateRatio = 1 )


#문3) 
#오바마 대통령의 데통령 당선 연설문에 대해 워드클라우드를 작성하시오
#Tip. 예제소스 파일은 ‘ex_10-5.txt’이다.
useSystemDic()
pal2 <- brewer.pal( 9, 'Blues' )[ 5:9 ]

text <- readLines( "ex_10-5.txt", encoding ="UTF-8" )
text

noun <- sapply( text, extractNoun, USE.NAMES = F )
noun 

noun2 <- unlist( noun )

noun2 <- noun2[ nchar( noun2 ) > 1 ]
noun2 <- gsub( "들이", "", noun2 )

wordcount <- table( noun2 )

wordcloud( names( wordcount ),
           freq = wordcount,
           scale = c( 6, 0.7 ),
           min.freq = 3,
           random.order = T,
           rot.per = .1,
           colors = pal2 )

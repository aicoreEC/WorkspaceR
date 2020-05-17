# 
# 4일차
#
# 0. Open
# 1. Review
# 2. Topic
#   2.1 Algorithm 이해
#   2.2 선택구조
#   2.3 반복구조
#   2.4 함수
# 3. Q & A
# 4. Next
# 5. Close
#
# 1. Review
#
# 변수명 / 표준 입력 / 표준 출력
#
# 변수명 부여 방식
numberValue <- 1                # camel 표기법
str_value <- "R Language"       # snake 표기법
booleanvalue <- TRUE            # 일반 표기법

# 표준 출력 장치에 출력
#
# print() : 자동 줄바꿈( \n, 자동 개행 )
numberValue; print( numberValue )
str_value; print( str_value )
booleanvalue; print( booleanvalue )


# cat() : 여러 내용을 출력할 수 있고, 자동 줄바꿈이
#         일어나지 않는 표준 출력 장치에 출력하는 함수
#
# 제어 문자 : 화면에 출력되지 않고 기능을 수행하는 문자
#   \n : 개행 문자( 줄바꿈 )
#   \t : tab 문자
#
cat( 'Numeric number : ', numberValue, '\n' )
cat( 'String : ', str_value, '\n' )
cat( 'Boolean value : ', booleanvalue, '\n' )
print( '---------------------------------------' )
cat( 'Numeric number : ', numberValue, '\t',
     'String : ', str_value, '\t',
     'Boolean value : ', booleanvalue, '\n' )
cat( 'test' )

#
# 표준 입력 장치로 부터 입력
#
# scan() : 표준 입력 장치로 부터 입력 받는 함수
# readline() : 표준 입력 장치로 부터 입력 받는 함수
# edit() : 표준 입력 장치로 부터 표형식으로 입력 받는 함수
#
inputData <- scan() # 빈줄이 입력될때 까지 숫자를 입력 받는다.
class( inputData )
inputData

inputData <- scan( what=character() )   # 문자 입려시 사용
class( inputData )
inputData

inputData <- readline( 'Input data -> ' )
class( inputData )
inputData


number1 <- readline( 'Input number1 : ' )
number2 <- readline( 'Input number2 : ' )
result <- as.numeric( number1 ) + as.numeric( number2 )
result

#
# 실습 문제 : 두 수를 입력 받아서 다음과 같이 출력
#      입력
#           Input number1 : [ 10 ] 
#           Input number2 : [ 5 ]
#      출력 결과
#           10 + 5 = 15
#           10 - 5 = 5
#           10 * 5 = 50
#           10 / 5 = 2
#           10 %% 5 = 0
#
number1 <- as.numeric( readline( 'Input number1 : ' ) )
number2 <- as.numeric( readline( 'Input number2 : ' ) )

resultAdd <- number1 + number2
resultSub <- number1 - number2
resultMul <- number1 * number2
resultDiv <- number1 / number2
resultRem <- number1 %% number2

cat( number1, ' + ', number2 , ' = ', resultAdd, '\n',
     number1, ' - ', number2 , ' = ', resultSub, '\n',
     number1, ' * ', number2 , ' = ', resultMul, '\n',
     number1, ' / ', number2 , ' = ', resultDiv, '\n',
     number1, ' %% ', number2 , ' = ', resultRem, '\n' )

#
# 2.1 Algorithm 이해
#
# Algorithm( Logic ) : 문제를 해결하기 위한 일처리 순서
#
# Algorithm 요건 
#  1. 입력 : 반드시 0개 이상의 입력이 있어야한다.
#  2. 출력 : 반드시 1개 이상의 출력이 있어야한다.
#  3. 유한성 : 반드시 끝낼 수 있어야 한다.
#  4. 효과성 : 효과적인 방법으로 정의되어야 한다.
#  5. 명확성 : 명확한 방법으로 정의되어야 한다.
#
# 컴퓨터 프로그램의 구조
#
# - 순차구조 : 시작부터 끝날때 까지 차례대로 수행
# - 선택구조 : 조건에 따라 처리 방향을 바꾸어서 수행
# - 반복구조 : 조건이 만족하는동안 동일한 내용을 반복 수행
#
# 컴퓨터 프로그램은 알고리즘에 기반으로 기억장소 원리와
#                   순차/선택/반복구조를 조합하여 작성한다.
#

#
# 2.2 선택구조
#
# 선택 구조 : 조건에 따라 처리 방향을 결정하는 구조
#
# 선택 구조 종류
#   1. 단순 선택 구조
#   2. 양자 택일 구조
#   3. 다중 선택 구조
#   4. 중첩 선택 구조
#
# 1. 단순 선택 구조
job.type <- 'A'
bonus <- 0
if ( job.type == 'A' ) {    # code block( code 집합 )
    bonus <- 200
}
cat( 'job type : ', job.type, '\t\tbonus : ', bonus )

# 2. 양자 택일 구조
job.type <- 'B'
bonus <- 0
if ( job.type == 'A' ) {    # code block( code 집합 )
    bonus <- 200
} else {
    bonus <- 100
}
cat( 'job type : ', job.type, '\t\tbonus : ', bonus )

# 3. 다중 선택 구조
score <- 50
if ( score >= 90 ) {
    grade <- 'A'
} else if ( score >= 80 ) {
    grade <- 'B'
} else if ( score >= 70 ) {
    grade <- 'C'
} else if ( score >= 60 ) {
    grade <- 'D'
} else {
    grade <- 'F'
}
cat( 'score : ', score, '\t\tgrade : ', grade )

# 4. 중첩 선택 구조
a <- 2; b <- 1; c <- 3
if ( a > b ) {
    if ( a > c ) {
        max <- a
        if ( b > c ) {
            mid  <-  b; min  <-  c
        } else {
            mid <- c; min <- b
        }
    } else if ( c > b ) {
        max <- c; mid <- a; min <- b
    }
} else if ( b > c ) {
    max <- b
    if ( a > c ) {
        mid <- a; min <- c
    } else {
        mid <- c; min <- a
    }
}
cat( 'max : ', max, '\tmid : ', mid, '\tmin : ', min )

number <- 10
remainder <- number %% 2

#if ( remainder == 0 ) {
if ( number %% 2 == 0 ) {
    oddeven <- '짝수'
} else {
    oddeven <- '홀수'
}
cat( 'Number : ', number, '는 ', oddeven, ' 이다.' )


a <- 5
b <- 20

if ( a > 5 & b > 5 ) {
    cat( a, ' > 5 and ', b, ' > 5' )
} else {
    cat ( a, ' <= 5 or ', b, ' <= 5' )
}

a <- 8; b <- 5; c <- 10; max <- a
if ( b > max ) {
    max <- b
}
if ( c > max ) {
    max <- c
}
cat( 'a = ', a, ' b = ', b, ' c = ', c, ' max = ', max )

a <- 8; b <- 5; c <- 10; min <- a
if ( b < min ) {
    min <- b
}
if ( c < min ) {
    min <- c
}
cat( 'a = ', a, ' b = ', b, ' c = ', c, ' min = ', min )

#
# 실습 문제 : 하나의 숫자를 입력받아 짝수인지 홀수인지 출력
#
#   Algorithm : 1. 하나의 숫자 입력
#               2. 짝수이면 짝수, 홀수이면 홀수 출력
#               3. 종료
#
number <- as.numeric( readline( "Input number :  ") )
if ( number %% 2 == 0 ) {
    cat( number, '는 짝수입니다.' )
} else {
    cat( number, '는 홀수입니다.' )
}

#
# 실습 문제 : 하나의 숫자를 입력받아 3의 배수이면 "3의배수"
#             5의 배수이면 "5의 배수" 
#             3, 5의 배수가 아니면 "3, 5의 배수가 이닙니다."
#             출력
#
number <- as.numeric( readline( "Input number : " ) )
if ( number %% 3 == 0 ) {
    cat( number, '는 3의 배수 입니다.' )
} else if ( number %% 5 == 0 ) {
    cat( number, '는 5의 배수 입니다.' )
} else {
    cat( number, '는 3, 5의 배수가 아닙니다.' )
}


number <- as.numeric( readline( "Input number : " ) )
multiple3 = number %% 3 == 0
multiple5 = number %% 5 == 0
if ( multiple3 ) {
    cat( number, '는 3의 배수 입니다.' )
} else if ( multiple5 ) {
    cat( number, '는 5의 배수 입니다.' )
} else {
    cat( number, '는 3, 5의 배수가 아닙니다.' )
}




#
# 2.3 반복구조
#
# 반복구조 : 조건이 만족하는동안 동일한 내용을 반복 수행
#
#   1. 반복 횟수가 정해진 경우 : for
#   2. 반복 횟수가 정해지지 않은 경우 : while
#
#   for문 : for( [반복제어변수] in [반복범위] ) { }
#
for ( i in 1:10 ) {
    print( '*' )
}

for ( i in 1:10 ) {
    cat( i, ' ' )
}

multiple = 2
for ( i in 2:9 ) {
    cat( multiple, ' * ', i , ' = ', multiple * i, '\n' )
}

for ( i in 2:9 ) {
    for ( j in 1:9 ) {
        multiple = i * j
        cat( multiple, '\t' )
    }
}

#
# 반복 제어 변수 초기화
# while ( 반복 제어 변수 조건 검사 ) {
#   반복 제어 변수 값 변경
# }
#
i <- 1  # 반복 제어 변수 초기화
while ( i <= 10 ) { # 반복 제어 변수 조건 검사, 참인동안 반복
    print( '*' )
    i <-  i + 2     # 반복 제어 변수 값 변경, 누적
}

i <- 2
while ( i <= 9 ) {
    j <- 1
    while( j <= 9 ) {
        multiple = i * j
        cat( multiple, '\t' )
        j <- j + 1
    }
    cat( '\n' )
    i <- i + 1
}


#
# 2.4 함수
#
# 함수( Function ) : 단위 기능을 수행하는 코드 집합
#
# 함수 종류 
#   1. 내장 함수 : R에 미리 내장된 함수( 기본 함수 )
#   2. 3rd party 함수 : 제 3 자가 작성하여 배포한 함수
#   3. 사용자 정의 함수 : 사용자가 직접 정의한 함수
#
# 함수 형식
#
# 함수 정의
#   함수이름(명) <- function( 인수 목록 ) {
#       함수 내용에 해당하는 코드 집합
#       return ( return값 )
#   }
#
# 함수 호출
#   함수명( 인수 목록 )
#

# 함수 정의
multiple <- function( x ) {
    for ( i in 1:9 ) {
        multiple <- x * i
        cat( x, ' * ', i, ' = ', multiple, '\n' )
    }
}

# 함수 호출
multiple( 2 )

# return값이 있는 함수
mymax <- function( x, y ) {
    num.max <- x
    if ( y > num.max ) {
        num.max <- y
    }
    return ( num.max )
}

mymax( 5, 6 )

a <- 10; b <- 5; c <- 8
max <- mymax( a, b ); max <- mymax( max, c )
max


# return값이 여러개인 함수
myCalc <- function( x, y ) {
    add <- x + y
    sub <- x - y
    mul <- x * y
    div <- x / y
    rem <- x %% y
    return ( list( ret.add = add, ret.sub = sub, ret.mul = mul,
                   ret.div = div, ret.rem = rem ) )
}
result <- myCalc( 10, 5 )
cat( '10 + 5 = ', result$ret.add, '\n' )
cat( '10 - 5 = ', result$ret.sub, '\n' )
cat( '10 * 5 = ', result$ret.mul, '\n' )
cat( '10 / 5 = ', result$ret.div, '\n' )
cat( '10 %% 5 = ', result$ret.rem, '\n' )

myCalc2 <- function( x, y ) {
    result <- c( x + y, x - y, x * y, x / y, x %% y )
    return ( result )
}

myCalc2( 10, 5 )



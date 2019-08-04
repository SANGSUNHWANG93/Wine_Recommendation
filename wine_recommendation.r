


print('start')
setwd('C://R_temp/wine-reviews')
getwd()
start = Sys.time()
raw_wine = read.csv('winemag-data-130k-v1.csv',stringsAsFactors = F)
end = Sys.time()
print(end-start)
#names
cols = names(raw_wine)#칼럼명을 확인한다.
cols


#getting library :dplyr
library(dplyr)
library(stringr)
library(sqldf)
library(colorspace)
library(lsa)

#-------------------------------------------------------------------------------------
# 01. [목적:처리 시간 감소를 위한 1~200 리뷰 추출,칼럼순서정렬)] / [방법:인덱싱]
#     [생성변수: sample] from [소스:raw_wine]

sample = raw_wine[1:200,]

## [중요] 200개 샘플 작동 확인 후, Line26 활성화 시킬 것.
sample = raw_wine 

cols[c(12,13,14,7,8,2,3,4,5,6)]
#title,variety,winery,province,region_1,country,description,designation,points,price 순으로 정렬.
sample = sample[c(12,13,14,7,8,2,3,4,5,6)]

#-------------------------------------------------------------------------------------

# 02. factor변수를 알맞은 형태로 변경 (다른 변수도 추후 변경해야함)
sample$price = as.character(sample$price)%>%as.numeric()
sample$points = as.character(sample$points)%>%as.numeric()
sample$description = as.character(sample$description)


#-------------------------------------------------------------------------------------
# 03.[목적: 특수문자제거] / [방법 : stringr 패키지 사용]
#    [생성변수:reviews] from [소스:sample]  

reviews = sample$description
str(reviews)
reviews = tolower(reviews)#소문자 변환
#리뷰의 유니코드,특수문자,한글(변환 상 오류)등을 제거한ㄷ
reviews = str_replace_all(reviews,"[^[:alnum:][:blank:]?&/\\-]", "")%>%
          str_replace_all( "[[:punct:]]", "") %>% 
          str_replace_all("\\p{Hangul}","")

#특수문자제거확인
head(sample$description,5);head(reviews,5)
#reviews data type : character
class(reviews)

#-------------------------------------------------------------------------------------
# 04.[목적 : 리뷰별 key_words 단어의 개수를 count] / 
#    [생성변수1:key_words]
#    [생성변수2:empty_list]
#    [소스2:reviews[i]] -> [생성변수3:split_words] -> [파생변수1:counts] / [방법:for문과 strsplit을 활용] 
#    


# reviews는 현재 200길이의 character 모임 c(review1,review2,review3,....review200) 
# 리뷰별 count를 담을 그릇(list)를 만든다.


#reviews의 길이 200 : i는 1~200까지의 인덱스를 뜻한다. 즉 reviews[i]는 i번째 리뷰 전체를 불러오게됨.
word_count = function(keyword){
  empty_list = list()
  temp = NA
  for (i in 1:length(reviews)){
    #cat(i,'/',length(reviews),'\n')
    #strsplit은 문자를 잘라 list형태로 반환하기 때문에 unlist를 덧씌운다.
    #class(strsplit(reviews[1],' ')) : list
    split_words = tolower(unlist(strsplit(reviews[i],' '))) # unlist(list[characters])의 형태(character)
    #  split_words별 key_words의 ii번째 단어의 포함을 체크한다.
    for (ii in 1:length(keyword)){
      #print(keyword[ii])
      temp[ii] = sum(grepl(keyword[ii],split_words))
      #print(temp[ii])
       #1번째 키워드~마지막 키워드까지의 체크값을 더하고, empty_list의[i]번째에 그 합을 기록한다.
    counts = sum(temp)}
    #counts = sum(grepl(keywords,split_words)) #TRUE=1, FALSE=0이기 때문에 SUM(c(TRUE,FALSE))는 결국 TRUE의 개수를 나타낸다.
    #empty_list(그릇)에 i번째 공간(empty_list[i])에 i번째 review의 count를 담는다.[*200번을 반복함] 
    empty_list[i]=counts
  }
  return(unlist(empty_list))
}


R_fruity = c('fruit','blackberr','blackcurrant','blueber','plum','redcurrant','rasber','strawber','cher','prun','berry','berr')
W_fruity = c('fruit','lemon','lim','grape','apple','pear','melon')
floral = c('floral','rosy','rose','violet','iris','jasmin','orange','blossom','lily','honeysuc')
herb_vege = c('herb','vege','menth','mint','eucalyp','liquoric','fennel','dill','rosemar','thyme','grass','capsic')
oak = c('oak','vanill','clove','nutmeg','cinnamo','coco','toast','cedar','butterscotch','ginger')

sample$R_fruity = word_count(R_fruity)
sample$W_fruity = word_count(W_fruity)
sample$floral = word_count(floral)
sample$herb_vege = word_count(herb_vege)
sample$oak = word_count(oak)

#각 지표 min max 정규화(사용자 취향 스케일 0~5 단위로 통일)
sample$R_fruity = 5*(sample$R_fruity-min(sample$R_fruity))/(max(sample$R_fruity)-min(sample$R_fruity))
sample$W_fruity = 5*(sample$W_fruity-min(sample$W_fruity))/(max(sample$W_fruity)-min(sample$W_fruity)) 
sample$floral = 5*(sample$floral-min(sample$floral))/(max(sample$floral)-min(sample$floral))
sample$herb_vege = 5*(sample$herb_vege-min(sample$herb_vege))/(max(sample$herb_vege)-min(sample$herb_vege))
sample$oak = 5*(sample$oak-min(sample$oak))/(max(sample$oak)-min(sample$oak))
sample$sum = (sample$R_fruity+sample$W_fruity+sample$floral+sample$herb_vege+sample$oak)
summary(sample)


#-------------------------------------------------------------------------------------
#05.[목적 : 사용자 취향의 수집과 저장을 위한 함수 입력형 함수 제작]
pr = function(){
  #readline('10개 질문이 있습니다. 계속하시려면 Enter를 누르세요.')
  ID = readline('STUDENT ID: ')
  name = readline('PUT YOUR NAME: ')
  R_fruity = as.numeric(readline('how much do you like fruit flavor(0-5):  '))
  W_fruity = as.numeric(readline('how much do you like white fruit flavor(0-5):  '))
  floral= as.numeric(readline('how muuch do you like floral flavor(0-5): '))
  herb_vege =  as.numeric(readline('how much do you like herbal-vegetal flavor(0-5): '))
  oak = as.numeric(readline('how much do you like oak taste(0-5):  '))
  ANS = data.frame(ID,name,R_fruity,W_fruity,floral,herb_vege,oak)
  return(ANS)
}

ANS = pr()

#2013100385
#SANGSUN HWANG
#5
#0
#0
#0
#0
ANS

#------------------------------------------------------------------------------
# 06.[목적 : 유클리드거리를 사용한 추천]
# 
scores = sample[c(1,11,12,13,14,15)]
euclid_recommend = function(cnt,prc){
  sample = subset(sample,country==cnt)%>%subset(,price<=prc)
  head(sample,10)
  scores = sample[c(1,11,12,13,14,15)]
  A = scores[2:6]
  B = ANS[3:7]
  distance = NA
  for(i in 1:dim(A)[1]){
    cat(i,'/',dim(A)[1],'\n')
    #print(A[i,])
    distance[i] = dist(rbind(A[i,],B,'euclidian'))
  }
  #print(distance)
  scores$rank = min_rank(distance)
  result = sqldf(
    'SELECT * FROM scores WHERE rank <= 10 ORDER BY rank limit 10'
  )
  return(result)
}

r1 = euclid_recommend('US',40)

#------------------------------------------------------------------------------
# 07.[목적 : 코사인유사도를 사용한 추천]
# 

cosine_recommend = function(cnt,prc){
  sample = subset(sample,country==cnt)%>%subset(,price<=prc)
  head(sample,10)
  scores = sample[c(1,11,12,13,14,15)]
  A = scores[2:6]
  B = ANS[3:7]
  similarity = NA
  
  for(i in 1:dim(A)[1]){
    cat(i,'/',dim(A)[1],'\n')
    similarity[i] = cosine(as.vector(as.matrix(A[i,])),as.numeric(as.matrix(B)))
  }
  #print(similarity)
  scores$rank = min_rank(-similarity)
  result = sqldf(
    'SELECT * FROM scores WHERE rank <= 10 ORDER BY rank limit 10'
  )
  return(result)
}
r2 = cosine_recommend('US',40)

r1;r2


#07.[목적 : 오차절대값의 평균을 사용한 추천성능측정함수]
# 성능측정
# 소비자응답(ANS)와 01.유클리드거리, 02.코사인유사도를 사용한 추천 결과의
# R_fruity,W_fruity,floral,herb_vege,oak,rank의 SUM(ABS(오차))/(개수)로 오차평균비교
# 작을수록 정확함을 나타낸다.

score = function(result){
  wines = result$title #추천함수결과에서 와인명 추출
  taste_table = subset(scores,title%in%wines) #맛지표테이블에서 추천된 와인 지표 추출
  matrix_t = as.matrix(taste_table[c(-1)]) #와인명 제외 매트릭스화
  ans = as.matrix(ANS[c(-1,-2)]) #소비자취향 매트릭스화(학번,이름제외)
  matrix_ans = matrix(rep(ans,dim(matrix_t)[1]),dim(matrix_t)[1],5,T) #연산을위해 매트릭스 차원 동일화
  score = sum(abs(matrix_t-matrix_ans))/dim(matrix_t)[1] # SUM(ABS(오차))의 평균
  return(score)
}
ed_score = score(r1);ed_score #1.447619
cs_score = score(r2);cs_score #3.988095


#------------------------------------------------------------------------------
# 더욱 정확한 성능 비교를 위해 US,SPAIN,FRANCE,Italy,Chile를 선택하여 K-fold 비교
e1 = euclid_recommend('US',40); e2 = euclid_recommend('Spain',40);e3 = euclid_recommend('France',40);e4=euclid_recommend('Italy',40);e5=euclid_recommend('Chile',40)
es1=score(e1) ;es1 #1.447619
es2=score(e2) ;es2 #1.678571
es3=score(e3) ;es3 #2.42619
es4=score(e4) ;es4 #1.985714
es5=score(e5) ;es5 #2.25
Euclid_score = mean(c(es1,es2,es3,es4,es5))

c1 = cosine_recommend('US',40); c2 = cosine_recommend('Spain',40);c3 = cosine_recommend('France',40);c4=cosine_recommend('Italy',40);c5=cosine_recommend('Chile',40)
cs1=score(c1);cs1 #3.988095
cs2=score(c2);cs2 #3.571429
cs3=score(c3);cs3 #4.525974
cs4=score(c4);cs4 #4.148352
cs5=score(c5);cs5 #3.636364
Cosine_score = mean(c(cs1,cs2,cs3,cs4,cs5))



Euclid_score;Cosine_score #1.957619 / 3.974043

09.[결론 및 한계]
 1.와인생산국,2.가격(달러기준),3.사용자입력취향을 고려한 와인추천함수를 제작하였다.
 사용자성향의 크기와 방향을 모두 고려하기 위해 추천함수를 1.유클리드거리, 2.코사인 유사도를사용하여 만들었다.
 유클리드거리를 사용한 함수의 정확도가 더 높게 나왔는데 
 이는 코사인유사도는 방향의 유사성을 측정할 뿐, 크기를 고려하지 않기 때문에 차이가 발생한 것으로 예상된다.
 * 한계 : 함수의 성능은 정확도 뿐 아니라 속도도 고려해야 하지만 속도에 대한 부분은 포함되지 않았다.

황상선(rich_flavor@naver.com)

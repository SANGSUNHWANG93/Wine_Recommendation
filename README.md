# Wine_Recommendation
wine recommendation unsing two methond : euclid_distance, cosine_similarity

Kaggle 데이터(https://www.kaggle.com/zynicide/wine-reviews)
의 리뷰를 기반으로 리뷰 → 와인의 맛을 수치화시킴 
와인의 맛은 (https://bettertastingwine.com/wine_tasting_aid.html)  를 기준으로 분류


수치화 된 와인의 맛 table에서
1. 와인생산국, 2. 예산(USD), 3.사용자취향을 고려한 와인 추천함수를 제작함
사용자 취향의 [크기와 방향]을 적절히 고려하기 위해 유클리드거리, 코사인유사도를 활용한 두가지 함수의
정확도를 측정하여 마무리함

목적 : 와인에 대해 익숙하지 않은 사람들을 위해 맛, 국가, 예산을 활용한 쉬운 와인 추천함수를 제작







사용 라이브러리 : dplyr, stringr, sqldf, colorspace, lsa



09.[결론 및 한계]
 1.와인생산국,2.가격(달러기준),3.사용자입력취향을 고려한 와인추천함수를 제작하였다.
 사용자성향의 크기와 방향을 모두 고려하기 위해 추천함수를 1.유클리드거리, 2.코사인 유사도를사용하여 만들었다.
 유클리드거리를 사용한 함수의 정확도가 더 높게 나왔는데 
 이는 코사인유사도는 방향의 유사성을 측정할 뿐, 크기를 고려하지 않기 때문에 차이가 발생한 것으로 예상된다.
 * 한계 : 함수의 성능은 정확도 뿐 아니라 속도도 고려해야 하지만 속도에 대한 부분은 포함되지 않았다.

황상선(rich_flavor@naver.com)

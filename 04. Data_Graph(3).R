#Gender 1개의 인자를 가지고 도수분포표 작성
table(X2023_STB_survey $Gender)

#Gender 1개의 인자를 가지고 상대도수분포표 작성
ECN <- table(X2023_STB_survey $Gender)
prop.table(ECN)

#Gender와 Grade 2개의 인자를 가지고 교차표 작성
table(X2023_STB_survey $Gender, X2023_STB_survey $Grade)

#Nationality 1개의 인자를 가지고 막대그래프 작성
barplot(table(X2023_STB_survey $Nationality), xlab = "Nationality", ylab = "Number of people", ylim = c(0,30), main = "2023 STB survey Nationality ratio", col = rainbow(12))

#residential area 1개 인자를 가지고 (가로) 막대그래프 작성
barplot(table(X2023_STB_survey $`residential area`), xlab = "Number of people", ylab = "Residentiall area", main = "2023 STB survey Residential area ratio", horiz = TRUE, col = rainbow(12))

#Gender와 Grade 2개의 인자를 가지고 막대그래프 작성
entry <- table(X2023_STB_survey $Gender, X2023_STB_survey $Grade)
barplot(entry, legend = TRUE, xlab = "Grade", ylab = "Number of people by gender", col = rainbow(12))

#Grade 1개의 인자를 가지고 파이차트 작성
pie(table(X2023_STB_survey $Grade), main = "2023 STB survey grade ratio", col = rainbow(12))

#Age 인자를 가지고 히스토그램 작성
hist(X2023_STB_survey $Age, main = "AGE Histogram", col = terrain.colors(12))

#Grade별 Age 비교 박스플롯
boxplot(X2023_STB_survey2$'2Age', X2023_STB_survey2$'3Age', X2023_STB_survey2$'4Age', main = "Age comparison by grade box plot", col = "yellow", names = c("2", "3", "4"), xlab = "Grade", ylab = "Age")

#기술통계분석
summary(X2023_STB_survey2)

#Grade를 X값으로 Age를 Y값으로 하는 산점도
plot(x=X2023_STB_survey$Grade, y=X2023_STB_survey$Age, xlab = "Grade", ylab = "Age")
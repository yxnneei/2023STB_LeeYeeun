#실습에 필요한 packages 라이브러리에 등록
library(dplyr)
library(ggplot2)

#CSV형식의 파일 불러와서 subway 객체에 입력하고 구조 확인
str(congestion)

#변수의 이상치와 결측치 확인하고 처리
summary(congestion)

#결측치 개수 확인
is.na(congestion)
sum(is.na(congestion))
colSums(is.na(congestion))

#결측치가 있는 행을 제거한 새로운 데이터 프레임 생성
#6시 출발기차의 결측치를 제거
congestion1 <- congestion[!is.na(congestion$s0600),]
colSums(is.na(congestion1))

#23시 30분 출발기차의 결측치를 제거
congestion1 <- congestion1[!is.na(congestion1$s2330),]
colSums(is.na(congestion1))

#남은 결측치를 0으로 대체
congestion1[is.na(congestion1)]<-0
colSums(is.na(congestion1))

#이상치 확인
ggplot(congestion1, aes(y=s0530))+
  geom_boxplot()

summary(congestion1$s0530)

#1. 지하철역의 하루 평균 혼잡도
congestion1$day_mean <- rowMeans(congestion1[,c('s0530','s0600','s0630','s0700','s0730','s0800', 's0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400', 's1430','s1500','s1530','s1600','s1630','s1700','s1730', 's1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330')])

congestion1%>%
  summarise(on_d=mean(day_mean))

#2. 지하철 호선별 하루 평균 혼잡도
passenger10 <- congestion1%>%
  group_by(line)%>%
  summarise(m=mean(day_mean))%>%
  arrange(desc(m))%>%
  head(20)

head(passenger10, 20)

#3. 지하철 호선별 출근시간(07:00~09:00)대의 평균혼잡도
#3-1-1. 지하철 호선별 출근시간 7:00의 평균혼잡도
gowork70 <- congestion1%>%
  group_by(line)%>%
  summarise(m=mean(s0700))%>%
  arrange(desc(m))%>%
  head(20)

head(gowork70, 3)

#3-1-2. 지하철 호선별 출근시간 7:30의 평균혼잡도
gowork73 <- congestion1%>%
  group_by(line)%>%
  summarise(m=mean(s0730))%>%
  arrange(desc(m))%>%
  head(20)

head(gowork73, 3)

#3-1-3. 지하철 호선별 출근시간 8:00의 평균혼잡도
gowork80 <- congestion1%>%
  group_by(line)%>%
  summarise(m=mean(s0800))%>%
  arrange(desc(m))%>%
  head(20)

head(gowork80, 3)

#3-1-4. 지하철 호선별 출근시간 8:30의 평균혼잡도
gowork83 <- congestion1%>%
  group_by(line)%>%
  summarise(m=mean(s0830))%>%
  arrange(desc(m))%>%
  head(20)

head(gowork83, 3)

#3-1-5. 지하철 호선별 출근시간 9:00의 평균혼잡도
gowork90 <- congestion1%>%
  group_by(line)%>%
  summarise(m=mean(s0900))%>%
  arrange(desc(m))%>%
  head(20)

head(gowork90, 3)

#3-2. 기술통계분석
summary(gowork70)
summary(gowork73)
summary(gowork80)
summary(gowork83)
summary(gowork90)

#3-3. 평균혼잡도 가장 높은 시간대 막대그래프로 그리기
ggplot(data=gowork80, aes(x=reorder(line, m), y=m))+
  geom_col()+
  coord_flip()

#3-4. 평균혼잡도 상위 4개 호선의 역별 기여도
#3-4-1. 7호선
line_pct <- congestion1 %>%
  filter(line==7) %>%
  group_by(station) %>%
  summarise(total=sum(s0800)) %>%
  mutate(all=sum(total), pct=round(total/all*100,2))

line_pct %>%
  arrange(desc(pct)) %>%
  head(3)

#3-4-2. 2호선
line_pct <- congestion1 %>%
  filter(line==2) %>%
  group_by(station) %>%
  summarise(total=sum(s0800)) %>%
  mutate(all=sum(total), pct=round(total/all*100,2))

line_pct %>%
  arrange(desc(pct)) %>%
  head(3)

#3-4-3. 4호선
line_pct <- congestion1 %>%
  filter(line==4) %>%
  group_by(station) %>%
  summarise(total=sum(s0800)) %>%
  mutate(all=sum(total), pct=round(total/all*100,2))

line_pct %>%
  arrange(desc(pct)) %>%
  head(3)

#3-4-4. 8호선
line_pct <- congestion1 %>%
  filter(line==8) %>%
  group_by(station) %>%
  summarise(total=sum(s0800)) %>%
  mutate(all=sum(total), pct=round(total/all*100,2))

line_pct %>%
  arrange(desc(pct)) %>%
  head(3)

#4. 08시 지하철 혼잡도 범주화/범주별 빈도분석
congestion1%>%
  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>%
  group_by(s80_grade)%>%
  summarise(n=n())%>%
  mutate(total=sum(n),pct=round(n/total*100,1))%>%
  select(s80_grade,n,pct)%>%
  arrange(desc(n))

#4-1. 호선별로 08시 지하철 혼잡도 범주화
congestion1%>%
  mutate(s80_grade=ifelse(s0800<=80,"good",ifelse(s0800<=130,"normal",ifelse(s0800<=150,"caution","bad"))))%>%
  group_by(line,s80_grade)%>%
  summarise(n=n())%>%
  mutate(total=sum(n),pct=round(n/total*100,1))%>%
  filter(s80_grade=="caution")%>%
  select(line,s80_grade,n,pct)%>%
  arrange(desc(pct))%>%
  head(5)

#5. 지하철 호선별 퇴근시간(18:00~20:00)대의 평균혼잡도
#5-1-1. 지하철 호선별 출근시간 18:00의 평균혼잡도
gohome18 <- congestion1%>%
  group_by(line)%>%
  summarise(m=mean(s1800))%>%
  arrange(desc(m))%>%
  head(20)

head(gohome18, 3)

#5-1-2. 지하철 호선별 출근시간 18:30의 평균혼잡도
gohome183 <- congestion1%>%
  group_by(line)%>%
  summarise(m=mean(s1830))%>%
  arrange(desc(m))%>%
  head(20)

head(gohome183, 3)

#5-1-3. 지하철 호선별 출근시간 19:00의 평균혼잡도
gohome19 <- congestion1%>%
  group_by(line)%>%
  summarise(m=mean(s1900))%>%
  arrange(desc(m))%>%
  head(20)

head(gohome19, 3)

#5-1-4. 지하철 호선별 출근시간 19:30의 평균혼잡도
gohome193 <- congestion1%>%
  group_by(line)%>%
  summarise(m=mean(s1930))%>%
  arrange(desc(m))%>%
  head(20)

head(gohome193, 3)

#5-1-5. 지하철 호선별 출근시간 20:00의 평균혼잡도
gohome20 <- congestion1%>%
  group_by(line)%>%
  summarise(m=mean(s2000))%>%
  arrange(desc(m))%>%
  head(20)

head(gohome20, 3)

#5-2.기술통계분석
summary(gohome18)
summary(gohome183)
summary(gohome19)
summary(gohome193)
summary(gohome20)

#5-3.평균혼잡도가 가장 높은 시간대 막대그래프로 그리기
ggplot(data=gohome18, aes(x=reorder(line, m), y=m))+
  geom_col()+
  coord_flip()

#5-4. 평균혼잡도 상위 4개 호선의 역별 기여도
#5-4-1. 2호선
line_pct <- congestion1 %>%
  filter(line==2) %>%
  group_by(station) %>%
  summarise(total=sum(s1800)) %>%
  mutate(all=sum(total), pct=round(total/all*100,2))

line_pct %>%
  arrange(desc(pct)) %>%
  head(3)

#5-4-2. 7호선
line_pct <- congestion1 %>%
  filter(line==7) %>%
  group_by(station) %>%
  summarise(total=sum(s1800)) %>%
  mutate(all=sum(total), pct=round(total/all*100,2))

line_pct %>%
  arrange(desc(pct)) %>%
  head(3)

#5-4-3. 4호선
line_pct <- congestion1 %>%
  filter(line==4) %>%
  group_by(station) %>%
  summarise(total=sum(s1800)) %>%
  mutate(all=sum(total), pct=round(total/all*100,2))

line_pct %>%
  arrange(desc(pct)) %>%
  head(3)

#5-4-4. 1호선
line_pct <- congestion1 %>%
  filter(line==1) %>%
  group_by(station) %>%
  summarise(total=sum(s1800)) %>%
  mutate(all=sum(total), pct=round(total/all*100,2))

line_pct %>%
  arrange(desc(pct)) %>%
  head(3)

#6. 출발시간 18시의 지하철 혼잡도 범주화/범주별 빈도분석
congestion1%>%
  mutate(s18_grade=ifelse(s1800<=80, "good", ifelse(s1800<=130, "normal", ifelse(s1800<=150, "caution", "bad"))))%>%
  group_by(s18_grade)%>%
  summarise(n=n())%>%
  mutate(total=sum(n),pct=round(n/total*100,1))%>%
  select(s18_grade,n,pct)%>%
  arrange(desc(n))

#6-1. 호선별 18시의 지하철 혼잡도 범주화
congestion1%>%
  mutate(s18_grade=ifelse(s1800<=80,"good",ifelse(s1800<=130,"normal",ifelse(s1800<=150,"caution","bad"))))%>%
  group_by(line,s18_grade)%>%
  summarise(n=n())%>%
  mutate(total=sum(n),pct=round(n/total*100,1))%>%
  filter(s18_grade=="bad")%>%
  select(line,s18_grade,n,pct)%>%
  arrange(desc(pct))%>%
  head(5)
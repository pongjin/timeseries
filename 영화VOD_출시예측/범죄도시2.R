rm(list=ls())
setwd("/Users/user/Desktop/시계열_프젝/")

library(forecast)
library(fracdiff)
library(lmtest)
library(TTR)
### 2022 holliday and culture day check

# 22년 holiday
holi1 = c(1,rep(0,29),1)
holi2 = c(1,1,rep(0,26))
holi3 = c(1,rep(0,7),1,0,0,1,rep(0,19))
holi5 = c(rep(0,4),1,rep(0,26))
holi6 = c(1,rep(0,4),1,rep(0,24))
holi7 = rep(0,31)
holi8 = c(rep(0,14),1,rep(0,16))
holi9 = c(rep(0,8),1,1,1,1,rep(0,18))
holi10 = c(0,0,1,rep(0,5),1,1,rep(0,21))
holi11 = c(rep(0,30))
# 22년 문화가있는 날 영화 세일
cul1 = c(rep(0,25),1,rep(0,5))
cul2 = c(rep(0,22),1,rep(0,5))
cul3 = c(rep(0,29),1,0)
cul4 = c(rep(0,26),1,0,0,0)
cul5 = c(rep(0,24),1,rep(0,6))
cul6 = c(rep(0,28),1,0)
cul7 = c(rep(0,26),1,0,0,0,0)
cul8 = c(rep(0,30),1)
cul9 = c(rep(0,27),1,0,0)
cul10 = c(rep(0,25),1,rep(0,5))
cul11 = c(rep(0,29),1)

#### load data  ####
crime = read.csv("범죄도시2_0518_0809.csv",encoding='UTF-8')
head(crime)

#좌석판매율
sell=as.numeric(gsub("%","",crime$좌석판매율))
cr0 <- ts(sell, start=c(1,3),frequency = 7);print(cr0,calendar=TRUE)
cr0_35 <- ts(sell[1:35], start=c(1,3),frequency=7)
par(mar=c(4,4,2,1))
ts.plot(cr0, main='Movie1',ylab='sell(%)', xlab=('week'))

# 관객수 데이터
crime_aud = crime$관객수
crime_aud = gsub(",","",crime_aud)
crime_aud = ts(as.numeric(crime_aud), start=c(1,3),frequency=7)
plot(crime_aud)

### 1. n=35 (35days) ###
cr1 = crime_aud[1:35]
cr1 = ts(cr1, start=c(1,3),frequency=7)
print(cr1,calendar=TRUE)
par(mfrow=c(2,1),mar=c(4,4.5,1,1))
ts.plot(cr1, xlab="week", ylab = 'audience')
ts.plot(cr0_35, xlab = 'week', ylab ='sell(%)')
#ts.plot(cr1, main='Movie1',ylab='sell(%)', xlab=('week'))

# 추세 제거
t = 1:35
t2 = t^2
lambda <- BoxCox.lambda(cr1);lambda # lamda=0, 로그변환과 동일.
#> 0.02 ~0 로그변환 진행
autoplot(BoxCox(cr1,lambda))
ind = 
cr1.lm1 = lm(log(cr1)~t ) # sqrt로 하면 이분산이 잡히지않음. 따라서 log변환 진행.
summary(cr1.lm1)
par(mfrow=c(2,2),mar=c(1,1,1,1))
plot(cr1.lm1)

# log 변환
cr1_trend = fitted(cr1.lm1)
cr1_adjtrend1 = log(cr1)-cr1_trend
par(mfrow=c(1,1))
plot(cr1_adjtrend1)

ts.plot(log(cr1),cr1_trend,col=c(1,2))

# 계절 제거
" 공휴일, 문화날에 따라 계절성분이 바뀌는 것이 아니기때문에 +변수로 반영 "
cr1_holi = c(holi5[18:length(holi5)],holi6[1:21]);length(cr1_holi)
cr1_cul = c(cul5[18:length(cul5)],cul6[1:21]);length(cr1_cul)

cr1.lm21 = lm(cr1_adjtrend1~ sin(2*pi*1*t/7) + cos(2*pi*1*t/7)  +
                 cr1_holi +  cr1_cul)
summary(cr1.lm21)
ts.plot(cr1_adjtrend1, fitted(cr1.lm21),col=c(1,2))
AIC(cr1.lm21)
ts.plot(cr1_adjtrend1-fitted(cr1.lm21))

cr1.lm22 = lm(cr1_adjtrend1~ sin(2*pi*1*t/7) + cos(2*pi*1*t/7) + sin(2*pi*2*t/7) + cos(2*pi*2*t/7) +  
                 cr1_cul+cr1_holi)
summary(cr1.lm22)
ts.plot(cr1_adjtrend1, fitted(cr1.lm22),col=c(1,2))
AIC(cr1.lm22)
ts.plot(cr1_adjtrend1-fitted(cr1.lm22))

cr1.lm23 = lm(cr1_adjtrend1~ sin(2*pi*1*t/7) + cos(2*pi*1*t/7) + sin(2*pi*2*t/7) + cos(2*pi*2*t/7) +  
              cr1_holi)
summary(cr1.lm23)
ts.plot(cr1_adjtrend1, fitted(cr1.lm23),col=c(1,2))
ts.plot(cr1_adjtrend1-fitted(cr1.lm23))

ts.plot(cr1_adjtrend1, fitted(cr1.lm22),fitted(cr1.lm23),col=c(1,2,3))
c(AIC(cr1.lm22),AIC(cr1.lm23))

cr1_adjtrend2 = cr1_adjtrend1-fitted(cr1.lm23)
ts.plot(cr1_adjtrend2)

#### 백색잡음 검정

# 1. 등평균
set.seed(1235)
suffle = sample(cr1_adjtrend2,35)

e1 = suffle[1:17];e2 = suffle[18:35]

e1 = cr1_adjtrend2[1:17] ; e2 = cr1_adjtrend2[18:35]

e = c(e1,e2)
grp = c(rep(0,17),rep(1,18))

# 2. anova
data1 = data.frame(cbind(grp,e))
summary(aov(e~grp,data=data1)) 

# 3. levene
d1 = abs(e1-median(e1)) ;d2 = abs(e2-median(e2)) 
d = c(d1,d2)
data2 = data.frame(cbind(grp,d))
t.test(d~grp, data=data2,var.equal=TRUE,conf.level=0.95) 
# 등분산, 등평균만족
# 모든 조건을 만족함을 알 수 있다.

#### Arima
par(mfrow=c(1,2))
acf(cr1_adjtrend2, main="ACF")
pacf(cr1_adjtrend2, main="PACF")
## 검증2
auto.arima(cr1_adjtrend2)
##만두(포트맨토) 검정
Box.test(cr1_adjtrend2,lag = 7,type = "Ljung")

####  최종 모델   ####
final_crime <- lm(log(cr1) ~ t + sin(2*pi*1*t/7) + cos(2*pi*1*t/7) +
                     sin(2*pi*2*t/7) + cos(2*pi*2*t/7) +  
                      cr1_holi) 
summary(final_crime)

# visualization
ts.plot(log(cr1),fitted(final_crime),col=c(1,2))

#### 예측 ####
new_cr1 = data.frame(t = c(1:70),
                      cr1_holi = c(holi5[18:length(holi5)],holi6,holi7[1:26]))
pred = predict(final_crime, new_cr1,interval = "prediction", level=0.95)
predic = exp(pred)
predic
par(mfrow=c(1,1),mar=c(2,2,2,2))
ts.plot(crime_aud[1:70],ts(predic),col=c(1,2,3,3),xlim=c(30,70),ylim = c(0,200000))

crime_aud[1:70]-predic[,2] ##54일부터 시작

######## 계절차분 model #########

#시각화
ts.plot(log(cr1))

# 차분 전 이상치 빼주기
ind_out <- lm(log(cr1) ~ cr1_holi + cr1_cul)
summary(ind_out)
ts.plot(log(cr1))
ts.plot(ind_out$residuals)
clear <- ts(ind_out$residuals,frequency = 7)
ts.plot(clear)
#방법 1. auto arima로 모형 확인해보기 
cr_arima <- auto.arima(clear) 
cr_arima #계절차분 ar

bb <- as_tsibble(clear)
bb %>% 
  model(ARIMA(value)) %>% report() #차분 필요

# 방법 2. acf pacf 및 그래프로 확인해보기
acf(clear,lag.max = 28)
pacf(clear,lag.max = 28)
ts.plot(clear) #차분이 무조건 필요한 상황

# 방법 3. 만두검정
Box.test(clear, lag = 7,type = "Ljung")
Box.test(clear, lag = 10,type = "Ljung") #자기상관 있음


# 방법 4. 함수
ndiffs(clear)
nsdiffs(clear) #이 방법은 이상함

###최종결론
# 계절 차분먼저 해보자
arima1 <- arima(ind_out$residuals,order = c(0,1,0),
                seasonal=list(order = c(0,1,1), period = 7))
ts.plot(arima1$residuals[8:35])
#과대차분 여부
arima1$sigma2
var(ind_out$residuals) #과대차분은 아님

#방법 1. auto arima로 모형 확인해보기 
cr_arima <- auto.arima(arima1$residuals[8:35]) 
cr_arima #계절차분 ar

bb <- as_tsibble(ts(arima1$residuals[8:35]))
bb %>% 
  model(ARIMA(value)) %>% report() #차분 필요

# 방법 2. acf pacf 및 그래프로 확인해보기
acf(arima1$residuals[8:35],lag.max = 28)
pacf(arima1$residuals[8:35],lag.max = 28)

# 방법 3. 만두검정
Box.test(arima1$residuals[8:35], lag = 7,type = "Ljung")
Box.test(arima1$residuals[8:35], lag = 10,type = "Ljung") #자기상관 있음

#따라서 일단 여러가지 후보 아리마 모형을 만들어서 aic, r^2로 비교예정
##arima 적합 
arima2 <- arima(ind_out$residuals, order = c(0,1,3),
                seasonal = list(order = c(0,1,1),period=7))

arima3 <- arima(ind_out$residuals ,order = c(0,1,2),
                seasonal = list(order = c(0,1,1),period=7))

#white noise check arima2
ts.plot(arima2$residuals[8:35])
coeftest(arima2)
#정규성
qqnorm(arima2$residuals);qqline(arima2$residuals,col='red') #정규성 살짝만족?
#acf pacf
acf(arima2$residuals[8:35],lag.max=28)
pacf(arima2$residuals[8:35],lag.max=28) #계절 ar ma 확신
#자기상관
Box.test(arima2$residuals[8:35],lag = 7,type = "Ljung")
Box.test(arima2$residuals[8:35],lag = 10,type = "Ljung") #기각
#오토아리마
auto.arima(arima2$residuals[8:35])
bb <- as_tsibble(ts(arima2$residuals[8:35]))
bb %>% 
  model(ARIMA(value)) %>% report()
# 계절 자기상관이 있는듯함

#white noise check arima3
ts.plot(arima3$residuals[8:35])
coeftest(arima3)
#정규성
qqnorm(arima3$residuals);qqline(arima3$residuals,col='red') #정규성 살짝만족?
#acf pacf
acf(arima3$residuals[8:35],lag.max=28)
pacf(arima3$residuals[8:35],lag.max=28) #계절 ar ma 확신
#자기상관
Box.test(arima3$residuals[8:35],lag = 7,type = "Ljung")
Box.test(arima3$residuals[8:35],lag = 10,type = "Ljung") #기각
#오토아리마
auto.arima(arima3$residuals[8:35])
bb <- as_tsibble(ts(arima3$residuals[8:35]))
bb %>% 
  model(ARIMA(value)) %>% report()

# AIC비교
c(AIC(arima2),AIC(arima3))

# 등평균
suffle = sample(arima3$residuals[8:35],28)
e1 = suffle[1:14];e2 = suffle[15:28]
e = c(e1,e2)
grp = c(rep(0,14),rep(1,14))
data1 = data.frame(cbind(grp,e))
summary(aov(e~grp,data=data1)) 
# levene
d1 = abs(e1-median(e1)) ;d2 = abs(e2-median(e2)) 
d = c(d1,d2)
data2 = data.frame(cbind(grp,d))
t.test(d~grp, data=data2,var.equal=TRUE,conf.level=0.95) 
# AIC비교
# 최종 모형 arima2으로 결정
############################################
####분해법과 sarima 비교
pred_arima <- predict(arima2,35)

ind_out$coefficients
arima_train = exp(12.3925519 + fitted(arima3) + 0.6447423*cr1_holi + 0.3508091*cr1_cul)
arima_test =  exp(12.3925519 + pred_arima$pred)

new_cr1 = data.frame(t = c(1:70),
                     cr1_holi = c(holi5[18:length(holi5)],holi6,holi7[1:26]))
pred = predict(final_crime, new_cr1,interval = "prediction", level=0.95)
predic = exp(pred[,1]);predic

decompose_train = predic[8:35]
decompose_test = predic[35:70]
#RMSE
rbind(rmse_arima_tr = RMSE(arima_train[8:35],cr1[8:35]),
      rmse_arima_test = RMSE(arima_test,cr1[36:70]),
      rmse_dc_tr = RMSE(decompose_train , cr1[8:35]),
      rmse_dc_test = RMSE(decompose_test , cr1[36:70]))
#MAE
rbind(rmse_arima_tr = MAE(arima_train[8:35],cr1[8:35]),
      rmse_arima_test = MAE(arima_test,cr1[36:70]),
      rmse_dc_tr = MAE(decompose_train , cr1[8:35]),
      rmse_dc_test = MAE(decompose_test , cr1[36:70]))
##########################################################################################
#좌석판매율
sell=as.numeric(gsub("%","",crime$좌석판매율))

cr_sc = crime$스크린수
cr_sc = as.numeric(gsub(",","",cr_sc));cr_sc
cr_p <- ts(sell, start=c(1,3),frequency = 7);print(cr_p,calendar=TRUE)
ts.plot(cr_p, main='Movie1',ylab='sell(%)', xlab=('week'))

par(mfrow=c(2,1))
ts.plot(cr_p[1:60], main='Movie1',ylab='sell(%)', xlab=('week'))
plot(crime_aud/cr_sc)
cr0 <- ts(cr_p, start=c(1,3),frequency = 7);print(cr0,calendar=TRUE)
par(mar=c(4,4,2,1))
ts.plot(cr0[1:50], main='Movie1',ylab='sell(%)', xlab=('week'))

cr0_35 = cr0[1:35]
cr0_35 = ts(cr0_35, start=c(1,3),frequency=7)
ts.plot(cr0_35, main='Movie1',ylab='sell(%)', xlab=('week'))
print(cr1,calendar=TRUE)
print(cr0_35,calendar=TRUE)
# 추세 제거
t = 1:35
t2 = t^2

lambda <- BoxCox.lambda(cr0_35);lambda
#> 0.02 ~0 로그변환 진행
autoplot(BoxCox(cr0_35,lambda)) #log변환


#cr0.lm11 = lm(log(cr0_35) ~ ind1 + ind2)
#summary(cr0.lm11)

#cr0_trend1 = fitted(cr0.lm11)
cr0_adjtrend11 = log(cr0_35)
#par(mfrow=c(1,1))
#plot(cr0_adjtrend11)
#ts.plot(log(cr0_35),cr0_trend1,col=c(1,2))

# 계절 제거
" 공휴일, 문화날에 따라 계절성분이 바뀌는 것이 아니기때문에 +변수로 반영 "
cr0_holi = c(holi5[18:length(holi5)],holi6[1:21]);length(cr0_holi)
cr0_cul = c(cul5[18:length(cul5)],cul6[1:21]);length(cr0_cul)
ind1 = c(rep(1,7),rep(0,7),rep(1,7),rep(0,14));length(ind1)
ind2 = c(rep(0,14),1,rep(0,20));length(ind2)

cr0.lm21 = lm(cr0_adjtrend11~ sin(2*pi*1*t/7) + cos(2*pi*1*t/7)  +
                 cr0_holi + cr0_cul + ind1 + ind2)
summary(cr0.lm21)
ts.plot(cr0_adjtrend11, fitted(cr0.lm21),col=c(1,2))
AIC(cr0.lm21)
ts.plot(cr0_adjtrend11-fitted(cr0.lm21))

cr0.lm22 = lm(cr0_adjtrend11~ sin(2*pi*1*t/7) + cos(2*pi*1*t/7) + sin(2*pi*2*t/7) + cos(2*pi*2*t/7) +  
                  + cr0_holi +cr0_cul + ind1 + ind2)
summary(cr0.lm22)
ts.plot(cr0_adjtrend11, fitted(cr0.lm22),col=c(1,2))
AIC(cr0.lm22)
ts.plot(cr0_adjtrend11-fitted(cr0.lm22))

cr0.lm23 = lm(cr0_adjtrend11~ sin(2*pi*1*t/7) + cos(2*pi*1*t/7) + sin(2*pi*2*t/7) + cos(2*pi*2*t/7) +  
                + cr0_holi+ ind1 )
summary(cr0.lm23)
ts.plot(cr0_adjtrend11, fitted(cr0.lm23),col=c(1,2))
AIC(cr0.lm23)
ts.plot(cr0_adjtrend11-fitted(cr0.lm23))
par(mfrow=c(1,1))
ts.plot(cr0_adjtrend11, fitted(cr0.lm22),fitted(cr0.lm23),col=c(1,2,3))

cr0_adjtrend2 = cr0_adjtrend11-fitted(cr0.lm23)
ts.plot(cr0_adjtrend2)

# 백색잡음 검정

# 1. 등평균
set.seed(1235)
suffle = sample(cr0_adjtrend2,35)

e1 = suffle[1:17];e2 = suffle[18:35]

#e1 = cr0_adjtrend2[1:21] ; e2 = cr0_adjtrend2[22:42]

e = c(e1,e2)
grp = c(rep(0,17),rep(1,18))

data1 = data.frame(cbind(grp,e))
summary(aov(e~grp,data=data1)) 

# 3. levene
d1 = abs(e1-median(e1)) ;d2 = abs(e2-median(e2)) 
d = c(d1,d2)
data2 = data.frame(cbind(grp,d))
t.test(d~grp, data=data2,var.equal=TRUE,conf.level=0.95) 
# 등분산, 등평균만족

## Arima
par(mfrow=c(1,2))
acf(cr0_adjtrend2, main="ACF",lag.max = 35)
pacf(cr0_adjtrend2, main="PACF",lag.max = 35)
## 검증2
auto.arima(cr0_adjtrend2)
library(fable)
library(feasts)
bb <- as_tsibble(ts(cr0_adjtrend2))
bb %>% 
  model(ARIMA(value)) %>% report()

##만두검정
Box.test(cr0_adjtrend2,lag = 7,type = "Ljung")
Box.test(cr0_adjtrend2,lag = 12,type = "Ljung")
#최종 모델
final_crime_seat <- lm(log(cr0_35) ~ sin(2*pi*1*t/7) + cos(2*pi*1*t/7) +
                          sin(2*pi*2*t/7) + cos(2*pi*2*t/7) +  
                          cr0_holi+ ind1 )
summary(final_crime_seat)
ts.plot(resid(final_crime_seat))
acf(final_crime_seat$residuals,lag.max = 35)
pacf(final_crime_seat$residuals,lag.max = 35)
auto.arima(final_crime_seat$residuals)
#bb <- as_tsibble(ts(final_crime_seat$residuals))
bb %>% 
  model(ARIMA(value)) %>% report()

Box.test(final_crime_seat$residuals,lag = 7,type = "Ljung")
Box.test(final_crime_seat$residuals,lag = 14,type = "Ljung")
# 예측
new_cr1 = data.frame(t=1:70,
                     ind1 = c(rep(1,7),rep(0,7),rep(1,7),rep(0,14),rep(0,35)),
                     ind2 = c(rep(0,14),1,rep(0,55)),
                     cr0_holi = c(holi5[18:length(holi5)],holi6,holi7[1:26]))
pred = predict(final_crime_seat, new_cr1,interval = "prediction")
predic = exp(pred);predic
par(mfrow=c(1,1),mar=c(2,2,2,2))
ts.plot(cr0[1:70],ts(predic),col=c(1,2,3,3),xlim=c(30,70))
ts.plot(cr0[1:70],ts(predic),col=c(1,2,3,3),xlim=c(0,70))

sell[1:70]-predic[,2] ##64일까지

#### 계절차분
ts.plot(log(cr0_35))

# 차분 전 이상치 빼주기
ind1 = c(rep(1,7),rep(0,7),rep(1,7),rep(0,14));length(ind1)
ind2 = c(rep(0,14),1,rep(0,20));length(ind2)
ind_out <- lm(log(cr0_35) ~ ind1 + ind2)

######## 계절차분
#시각화
ts.plot(log(cr0_35))

#방법 1. auto arima로 모형 확인해보기 
cr_arima <- auto.arima(ind_out$resid) 
cr_arima #계절차분 필요없음

bb <- as_tsibble(ts(ind_out$resid))
bb %>% 
  model(ARIMA(value)) %>% report() #계절차분 필요없음

# 방법 2. acf pacf 및 그래프로 확인해보기
cr_resid <- cr_arima$residuals
acf(cr_resid,lag.max = 28)
pacf(cr_resid,lag.max = 28)
ts.plot(cr_resid) #차분이 무조건 필요한 상황

# 방법 3. 만두검정
Box.test(cr_resid, lag = 7,type = "Ljung")
Box.test(cr_resid, lag = 10,type = "Ljung") #자기상관 있음

# 방법 4. 함수
ndiffs(cr_resid)
#nsdiffs(cr_resid) #이 방법은 이상함

###최종결론
# 오토 아리마는 잘못된것 같고 방법2의 acf를 보면 7주기마다 값이 작아짐
# 이는 계절 주기에서도 ma가 있다고 판단됨. 확인을 위해 7차 차분값을 확인

#diff
ts.plot(ind_out$resid)
aa <- diff(ind_out$resid,7)
ts.plot(aa)
acf(aa,lag.max=28)
pacf(aa,lag.max=28) #정확한 이유는 모르겠지만 여기서도 주기마다 ar(1)이 보임

#따라서 일단 여러가지 후보 아리마 모형을 만들어서 aic, r^2로 비교예정
##arima 적합
arima1 <- arima(ind_out$resid ,seasonal = list(order = c(1,1,1),period=7))

arima2 <- arima(ind_out$resid, seasonal = list(order = c(0,1,0),period=7))

arima3 <- arima(ind_out$resid ,seasonal = list(order = c(0,1,1),period=7))

#white noise check arima1
ts.plot(arima1$residuals[8:35])
#coeftest
coeftest(arima1) #ar은 유의하지 않음
#정규성
qqnorm(arima1$residuals);qqline(arima1$residuals,col='red') #정규성 만족?
#acf pacf
acf(arima1$residuals[8:35])
pacf(arima1$residuals[8:35])
#자기상관
Box.test(arima1$residuals[8:35],lag = 7,type = "Ljung")
Box.test(arima1$residuals[8:35],lag = 10,type = "Ljung")
#오토아리마
auto.arima(arima1$residuals[8:35])
bb <- as_tsibble(ts(arima1$residuals[8:35]))
bb %>% 
  model(ARIMA(value)) %>% report()
# 등평균
suffle = sample(arima1$residuals[8:35],28)
e1 = suffle[1:14];e2 = suffle[15:28]
e = c(e1,e2)
grp = c(rep(0,14),rep(1,14))
data1 = data.frame(cbind(grp,e))
summary(aov(e~grp,data=data1)) 
# levene
d1 = abs(e1-median(e1)) ;d2 = abs(e2-median(e2)) 
d = c(d1,d2)
data2 = data.frame(cbind(grp,d))
t.test(d~grp, data=data2,var.equal=TRUE,conf.level=0.95) 
# 모든 가정 만족. 즉 화이트노이즈

#white noise check arima2
ts.plot(arima2$residuals[8:35])
#coeftest
#coeftest(arima2)
#정규성
qqnorm(arima2$residuals);qqline(arima2$residuals,col='red') #정규성 살짝만족?
#acf pacf
acf(arima2$residuals[8:35],lag.max=28)
pacf(arima2$residuals[8:35],lag.max=28) #계절 ar ma 확신
#자기상관
Box.test(arima2$residuals[8:35],lag = 7,type = "Ljung")
Box.test(arima2$residuals[8:35],lag = 10,type = "Ljung") #기각
#오토아리마
auto.arima(arima2$residuals[8:35])
bb <- as_tsibble(ts(arima2$residuals[8:35]))
bb %>% 
  model(ARIMA(value)) %>% report()
# 등평균
suffle = sample(arima2$residuals[8:35],28)
e1 = suffle[1:14];e2 = suffle[15:28]
e = c(e1,e2)
grp = c(rep(0,14),rep(1,14))
data1 = data.frame(cbind(grp,e))
summary(aov(e~grp,data=data1)) 
# levene
d1 = abs(e1-median(e1)) ;d2 = abs(e2-median(e2)) 
d = c(d1,d2)
data2 = data.frame(cbind(grp,d))
t.test(d~grp, data=data2,var.equal=TRUE,conf.level=0.95) 
# 계절 자기상관이 있는듯함
# AIC비교
c(AIC(arima1),AIC(arima2))

#white noise check arima3
ts.plot(arima3$residuals[8:35])
#coeftest
coeftest(arima3)
#정규성
qqnorm(arima3$residuals);qqline(arima3$residuals,col='red') #정규성만족?
#acf pacf
acf(arima3$residuals[8:35],lag.max=28) 
pacf(arima3$residuals[8:35],lag.max=28) 
#자기상관
Box.test(arima3$residuals[8:35],lag = 7,type = "Ljung")
Box.test(arima3$residuals[8:35],lag = 10,type = "Ljung") #기각
#오토아리마
auto.arima(arima3$residuals[8:35])
bb <- as_tsibble(ts(arima3$residuals[8:35]))
bb %>% 
  model(ARIMA(value)) %>% report()
# 등평균
suffle = sample(arima3$residuals[8:35],28)
e1 = suffle[1:14];e2 = suffle[15:28]
e = c(e1,e2)
grp = c(rep(0,14),rep(1,14))
data1 = data.frame(cbind(grp,e))
summary(aov(e~grp,data=data1)) 
# levene
d1 = abs(e1-median(e1)) ;d2 = abs(e2-median(e2)) 
d = c(d1,d2)
data2 = data.frame(cbind(grp,d))
t.test(d~grp, data=data2,var.equal=TRUE,conf.level=0.95) 
# 계절 자기상관이 있는듯함
# AIC비교
c(AIC(arima1), AIC(arima2), AIC(arima3)) 

# 최종 모형 arima3으로 결정
############################################
####분해법과 sarima 비교
pred_arima <- predict(arima3,35)

ind_out$coefficients
arima_train = exp(2.8633646 + fitted(arima3) + ind1*0.4507 + ind2*0.8861374)
arima_test =  exp(2.8633646 + pred_arima$pred)

pred = predict(final_crime_seat, new_cr1,interval = "prediction")
predic = exp(pred[,1]);predic

decompose_train = predic[8:35]
decompose_test = predic[35:70]
#RMSE
rbind(rmse_arima_tr = RMSE(arima_train[8:35],cr0[8:35]),
      rmse_arima_test = RMSE(arima_test,cr0[36:70]),
      rmse_dc_tr = RMSE(decompose_train , cr0[8:35]),
      rmse_dc_test = RMSE(decompose_test , cr0[36:70]))
#MAE
rbind(rmse_arima_tr = MAE(arima_train[8:35],cr0[8:35]),
      rmse_arima_test = MAE(arima_test,cr0[36:70]),
      rmse_dc_tr = MAE(decompose_train , cr0[8:35]),
      rmse_dc_test = MAE(decompose_test , cr0[36:70]))

rm(list=ls())
setwd("/Users/user/Desktop/시계열_프젝/")

library(forecast)
library(fracdiff)
library(lmtest)
library(TTR)

##### check holiday and culture day ######
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

#### 공조2  ####
cooper = read.csv("공조2_0907_1122.csv",encoding='UTF-8')
head(cooper)

#좌석판매율
sell=as.numeric(gsub("%","",cooper$좌석판매율))
coo0 <- ts(sell, start=c(1,3),frequency = 7);print(coo0,calendar=TRUE)
co_35 <- ts(sell[1:35], start=c(1,3),frequency = 7)
par(mar=c(4,4,2,1))
ts.plot(coo0, main='Movie1',ylab='sell(%)', xlab=('week'))

# 관객수 데이터
cooper_aud = cooper$관객수
cooper_aud = gsub(",","",cooper_aud)
cooper_aud = ts(as.numeric(cooper_aud), start=c(1,3),frequency=7)
plot(cooper_aud)

##### 1. n=35 (35 days after screening) #####
coo1 = cooper_aud[1:35]
coo1 = ts(coo1, start=c(1,3),frequency=7)
print(coo1,calendar=TRUE)

#시각화
par(mfrow=c(2,1),mar=c(4,4.5,1,1))
ts.plot(coo1, xlab="week", ylab = 'audience')
ts.plot(co_35, xlab = 'week', ylab ='sell(%)')

#### 추세 제거 ####
t = 1:35
t2 = t^2

lambda <- BoxCox.lambda(coo1);lambda # lamda=0, 로그변환과 동일.
#> 0.02 ~0 로그변환 진행
autoplot(BoxCox(coo1,lambda))

coo1.lm1 = lm(log(coo1)~t ) # sqrt로 하면 이분산이 잡히지않음. 따라서 log변환 진행.
summary(coo1.lm1)
par(mfrow=c(2,2),mar=c(4,4,2,1))
plot(coo1.lm1)

# log 변환
coo1_trend = fitted(coo1.lm1)
coo1_adjtrend1 = log(coo1)-coo1_trend
par(mfrow=c(1,1))
plot(coo1_adjtrend1)

par(mfrow=c(1,2),mar=c(4,2,1,1))
ts.plot(log(coo1),coo1_trend,col=c(1,2),main = 'with trend')
ts.plot(coo1_adjtrend1,main = 'log - trend')

### 계절 제거

" 공휴일, 문화날에 따라 계절성분이 바뀌는 것이 아니기때문에 +변수로 반영 "
coo1_holi = c(holi9[7:length(holi9)],holi10[1:11]);length(coo1_holi)
coo1_cul = c(cul9[7:length(cul9)],cul10[1:11]);length(coo1_cul)

coo1.lm21 = lm(coo1_adjtrend1~ sin(2*pi*1*t/7) + cos(2*pi*1*t/7)  +
                 coo1_holi +  coo1_cul)
summary(coo1.lm21)
ts.plot(coo1_adjtrend1, fitted(coo1.lm21),col=c(1,2))
AIC(coo1.lm21)
ts.plot(coo1_adjtrend1-fitted(coo1.lm21))

coo1.lm22 = lm(coo1_adjtrend1~ sin(2*pi*1*t/7) + cos(2*pi*1*t/7) + sin(2*pi*2*t/7) + cos(2*pi*2*t/7) +  
                 coo1_cul+coo1_holi)
coo1.lm24 = lm(coo1_adjtrend1~ sin(2*pi*1*t/7) + cos(2*pi*1*t/7) + sin(2*pi*2*t/7) + cos(2*pi*2*t/7) +  
                 coo1_holi)
summary(coo1.lm22)
par(mfrow=c(1,2))
ts.plot(coo1_adjtrend1, fitted(coo1.lm22),col=c(1,2))
ts.plot(coo1_adjtrend1, fitted(coo1.lm24),col=c(1,2))
par(mfrow=c(1,1))
AIC(coo1.lm22)
ts.plot(coo1_adjtrend1-fitted(coo1.lm22))

coo1.lm23 = lm(coo1_adjtrend1~ sin(2*pi*1*t/7) + cos(2*pi*1*t/7) + sin(2*pi*2*t/7) + cos(2*pi*2*t/7) + sin(2*pi*3*t/7) + cos(2*pi*3*t/7) +  coo1_cul+coo1_holi)
summary(coo1.lm23)
ts.plot(coo1_adjtrend1, fitted(coo1.lm23),col=c(1,2))
ts.plot(coo1_adjtrend1-fitted(coo1.lm23))

ts.plot(coo1_adjtrend1, fitted(coo1.lm21),fitted(coo1.lm22),col=c(1,2,3))
legend("topleft",legend=c('raw',"lm1","lm2"),
       fill=c("black","red","green"),cex=0.7)
coo1_adjtrend2 = coo1_adjtrend1-fitted(coo1.lm22)
ts.plot(coo1_adjtrend2,main='irregular')

### 백색잡음 검정 ###

# 1. 등평균
set.seed(1235)
suffle = sample(coo1_adjtrend2,35)

e1 = suffle[1:17];e2 = suffle[18:35]

e1 = coo1_adjtrend2[1:17] ; e2 = coo1_adjtrend2[18:35]

e = c(e1,e2)
grp = c(rep(0,17),rep(1,18))

data1 = data.frame(cbind(grp,e))
summary(aov(e~grp,data=data1)) 

# 2. levene
d1 = abs(e1-median(e1)) ;d2 = abs(e2-median(e2)) 
d = c(d1,d2)
data2 = data.frame(cbind(grp,d))
t.test(d~grp, data=data2,var.equal=TRUE,conf.level=0.95) 
## 등분산, 등평균만족


## Arima ##
par(mfrow=c(1,2))
acf(coo1_adjtrend2, main="ACF")
pacf(coo1_adjtrend2, main="PACF")
## 검증2
auto.arima(ts(coo1_adjtrend2))
bb <- as_tsibble(ts(coo1_adjtrend2))
bb %>% 
  model(ARIMA(value)) %>% report()

##만두(포트맨토)검정
Box.test(coo1_adjtrend2,lag = 7,type = "Ljung")
Box.test(coo1_adjtrend2,lag = 14,type = "Ljung")

####  최종 모델   ####
final_kongjo <- lm(log(coo1) ~ t + sin(2*pi*1*t/7) + cos(2*pi*1*t/7) +
                     sin(2*pi*2*t/7) + cos(2*pi*2*t/7) +  
                     coo1_cul+ coo1_holi)
summary(final_kongjo)
par(mfrow=c(1,1))
ts.plot(log(coo1),fitted(final_kongjo),col=c(1,2))

# 예측 #
new_coo1 = data.frame(t = c(1:70),
                      coo1_cul = c(holi9[7:length(holi9)],holi10,holi11[1:15]),
                      coo1_holi = c(cul9[7:length(cul9)],cul10,cul11[1:15]))
pred = predict(final_kongjo, new_coo1,interval = "prediction", level=0.95)
predic = exp(pred)

par(mfrow=c(1,1))
ts.plot(cooper_aud[1:70],ts(predic),col=c(1,2,3,3))
legend("topright",legend=c('raw',"pred","pred_interval"),
       fill=c("black","red","green"),cex=0.7)
ts.plot(cooper_aud[1:70],ts(predic),col=c(1,2,3,3),
        xlim=c(50,70),ylim = c(0,30000))
legend("topright",legend=c('raw',"pred","pred_interval"),
       fill=c("black","red","green"),cex=0.7)
cooper_aud[1:70]-predic[,2] ##54일부터 시작

#### 계절차분 model ####
ts.plot(log(coo1))

# 차분 전 이상치 빼주기
coo1_cul = c(cul9[7:length(cul9)],cul10[1:11]);length(coo1_cul)
ind_out <- lm(log(coo1) ~ coo1_cul)
ts.plot(log(coo1),ind_out$fitted.values,col=c(1,2))
ts.plot(log(coo1)-0.5932*coo1_cul)
clean <- log(coo1)-0.5932*coo1_cul

######## 계절차분
#시각화
ts.plot(clean)

#방법 1. auto arima로 모형 확인해보기 
cr_arima <- auto.arima(clean)
cr_arima #계절차분 필요없음

bb <- as_tsibble(clean)
bb %>% 
  model(ARIMA(value)) %>% report() #계절차분 필요없음

# 방법 2. acf pacf 및 그래프로 확인해보기
par(mfrow=c(2,1))
acf(clean,lag.max = 15)
pacf(clean,lag.max = 15)

# 방법 3. 만두검정 생략
# 방법 4. 함수
ndiffs(clean)
nsdiffs(clean) #이 방법은 이상함

###최종결론
# 오토 아리마에서 계절차분과 일반차분 둘 다

#따라서 일단 여러가지 후보 아리마 모형을 만들어서 aic, r^2로 비교예정
##arima 적합
arima1 <- arima(ind_out$resid ,seasonal = list(order = c(0,1,0),period=7))

arima2 <- arima(ind_out$resid, order = c(0,0,1),
                seasonal = list(order = c(0,1,0),period=7))

arima3 <- arima(ind_out$resid , order = c(0,1,1),
                seasonal = list(order = c(0,1,0),period=7))

arima4 <- arima(ind_out$resid , order = c(0,1,0),
                seasonal = list(order = c(0,1,0),period=7))
#white noise check arima1
ts.plot(arima1$residuals[8:35])
#coeftest
#정규성
qqnorm(arima1$residuals);qqline(arima1$residuals,col='red') #정규성 만족?
#acf pacf
acf(arima1$residuals[8:35]) #ma(1)도 괜찮고 다 범위내이기도함
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
# 화이트 노이즈

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
# 화이트노이즈

#white noise check arima3
ts.plot(arima4$residuals[8:35])
#정규성
qqnorm(arima4$residuals);qqline(arima4$residuals,col='red') #정규성만족?
#acf pacf
acf(arima4$residuals[8:35],lag.max=28) 
pacf(arima4$residuals[8:35],lag.max=28) 
#자기상관
Box.test(arima4$residuals[8:35],lag = 7,type = "Ljung")
Box.test(arima4$residuals[8:35],lag = 10,type = "Ljung") #기각
#오토아리마
auto.arima(arima4$residuals[8:35])
bb <- as_tsibble(ts(arima4$residuals[8:35]))
bb %>% 
  model(ARIMA(value)) %>% report()
# 등평균
suffle = sample(arima4$residuals[8:35],28)
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
# 등분산 만족 X
# AIC비교
c(AIC(arima1), AIC(arima2), AIC(arima3))

# 최종 모형 arima3으로 결정
############################################
####분해법과 sarima 비교
pred_arima <- predict(arima3,35)

ind_out$coefficients
arima_train = exp(11.69573 + fitted(arima3) - 0.5932416*coo1_cul)
arima_test =  exp(11.69573 + pred_arima$pred)

new_coo1 = data.frame(t = c(1:70),
                      coo1_cul = c(holi9[7:length(holi9)],holi10,holi11[1:15]),
                      coo1_holi = c(cul9[7:length(cul9)],cul10,cul11[1:15]))
pred = predict(final_kongjo, new_coo1,interval = "prediction", level=0.95)
predic = exp(pred[,1]);predic

decompose_train = predic[8:35]
decompose_test = predic[35:70]
#RMSE
rbind(rmse_arima_tr = RMSE(arima_train[8:35],coo1[8:35]),
      rmse_arima_test = RMSE(arima_test,coo1[36:70]),
      rmse_dc_tr = RMSE(decompose_train , coo1[8:35]),
      rmse_dc_test = RMSE(decompose_test , coo1[36:70]))
#MAE
rbind(rmse_arima_tr = MAE(arima_train[8:35],coo1[8:35]),
      rmse_arima_test = MAE(arima_test,coo1[36:70]),
      rmse_dc_tr = MAE(decompose_train , coo1[8:35]),
      rmse_dc_test = MAE(decompose_test , coo1[36:70]))

#분해법이 더 좋음

##########################################################################################
#좌석판매율
par(mfrow=c(1,1))
sell=as.numeric(gsub("%","",cooper$좌석판매율))
ts.plot(sell, main='Movie1',ylab='sell(%)', xlab=('week'))

coo0_42 = sell[1:35]
coo0_42 = ts(coo0_42, start=c(1,3),frequency=7)
ts.plot(coo0_42, main='Movie1',ylab='sell(%)', xlab=('week'))
print(coo0_42,calendar=TRUE)
# 추세 제거
t = 1:35
t2 = t^2

lambda <- BoxCox.lambda(coo0_42);lambda
#> 0.02 ~0 로그변환 진행
autoplot(BoxCox(coo0_42,lambda)) #log변환

coo0.lm1 = lm(log(coo0_42) ~ t ) # sqrt로 하면 이분산이 잡히지않음. 따라서 log변환 진행.
summary(coo0.lm1)
par(mfrow=c(2,2),mar=c(2,2,2,2))
plot(coo0.lm1)

# log 변환
coo0_trend = fitted(coo0.lm1)
coo0_adjtrend1 = log(coo0_42)-coo0_trend
par(mfrow=c(1,1))
plot(coo0_adjtrend1)

ts.plot(log(coo0_42),coo0_trend,col=c(1,2))


coo0.lm11 = lm(log(coo0_42) ~ t ) # sqrt로 하면 이분산이 잡히지않음. 따라서 log변환 진행.
summary(coo0.lm11)
par(mfrow=c(2,2),mar=c(2,2,2,2))
plot(coo0.lm11)
coo0_trend1 = fitted(coo0.lm11)
coo0_adjtrend11 = log(coo0_42)-coo0_trend1
par(mfrow=c(1,1))
plot(coo0_adjtrend11)
ts.plot(log(coo0_42),coo0_trend1,col=c(1,2))

# 계절 제거
" 공휴일, 문화날에 따라 계절성분이 바뀌는 것이 아니기때문에 +변수로 반영 "
chuseok <- c(1,1,1,1,1,1,1,1,rep(0,27))
#ind2 <- c(rep(0,14),1,rep(0,20));ind2
coo1_holi = c(holi9[7:length(holi9)],holi10[1:11]);length(coo1_holi)
coo1_cul = c(cul9[7:length(cul9)],cul10[1:11]);length(coo1_cul)
ind1 <- c(rep(0,21),rep(1,6),rep(0,8));length(ind1)

coo0.lm21 = lm(log(coo0_42)~ sin(2*pi*1*t/7) + cos(2*pi*1*t/7)  +
                   coo0_cul + chuseok + coo1_holi  + ind1)
summary(coo0.lm21)
ts.plot(log(coo0_42), fitted(coo0.lm21),col=c(1,2))
AIC(coo0.lm21)
ts.plot(log(coo0_42)-fitted(coo0.lm21))

coo0.lm22 = lm(log(coo0_42)~ sin(2*pi*1*t/7) + cos(2*pi*1*t/7) + sin(2*pi*2*t/7) + cos(2*pi*2*t/7) +  
                  ind1 + coo0_cul + chuseok + coo1_holi)
summary(coo0.lm22)
ts.plot(log(coo0_42), fitted(coo0.lm22),col=c(1,2))
AIC(coo0.lm22)
ts.plot(log(coo0_42)-fitted(coo0.lm22))

ts.plot(log(coo0_42), fitted(coo0.lm21),fitted(coo0.lm22),col=c(1,2,3))

coo0_adjtrend2 = log(coo0_42)-fitted(coo0.lm22)
ts.plot(coo0_adjtrend2)

# 백색잡음 검정

# 1. 등평균
set.seed(1235)
suffle = sample(coo0_adjtrend2,35)

e1 = suffle[1:17];e2 = suffle[18:35]

#e1 = coo0_adjtrend2[1:21] ; e2 = coo0_adjtrend2[22:42]

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
acf(coo0_adjtrend2, main="ACF")
pacf(coo0_adjtrend2, main="PACF")
## 검증2
auto.arima(coo0_adjtrend2)
##만두검정
Box.test(coo0_adjtrend2,lag = 7,type = "Ljung")
Box.test(coo0_adjtrend2,lag = 14,type = "Ljung")
#최종 모델
final_kongjo_seat <- lm(log(coo0_42) ~ sin(2*pi*1*t/7) + cos(2*pi*1*t/7) +
                          sin(2*pi*2*t/7) + cos(2*pi*2*t/7) +  
                          coo0_cul+chuseok + ind1 + coo1_holi)
summary(final_kongjo_seat)

# 예측
new_coo1 = data.frame(t = c(1:70),
                      ind1 = c(rep(0,21),rep(1,6),rep(0,36),1,rep(0,6)),
                      chuseok = c(1,1,1,1,1,1,1,1,rep(0,62)),
                      coo1_holi = c(holi9[7:length(holi9)],holi10,holi11[1:15]),
                      coo0_cul = c(holi9[7:length(holi9)],holi10,holi11[1:15]))
pred = predict(final_kongjo_seat, new_coo1,interval = "prediction")
predic = exp(pred)

par(mfrow=c(1,1),mar=c(2,2,2,2))
ts.plot(sell[1:70],ts(predic),col=c(1,2,3,3),xlim=c(30,70),ylim=c(0,50))
ts.plot(sell[1:70],ts(predic),col=c(1,2,3,3),xlim=c(40,70),ylim=c(0,50))

sell[1:70] -predic[,2] ##65일부터 시작

##############################################################################
#### 계절차분
ts.plot(log(coo0_42))

# 차분 전 이상치 빼주기
chuseok <- c(1,1,1,1,1,1,1,0,rep(0,27))
coo1_holi = c(holi9[7:length(holi9)],holi10[1:11]);length(coo1_holi)
coo1_cul = c(cul9[7:length(cul9)],cul10[1:11]);length(coo1_cul)
ind1 <- c(rep(0,21),rep(1,6),rep(0,8));length(ind1)

ind_out <- lm(log(coo0_42) ~ coo0_cul + chuseok)
ts.plot(log(coo0_42),ind_out$fitted.values,col=c(1,2))
ts.plot(ind_out$residuals)
clean <- ts(ind_out$residuals,frequency=7)
#방법 1. auto arima로 모형 확인해보기 
cr_arima <- auto.arima(clean) 
cr_arima #계절차분 1번

bb <- as_tsibble(clean)
bb %>% 
  model(ARIMA(value)) %>% report() #계절차분 필요없음

# 방법 2. acf pacf 및 그래프로 확인해보기
acf(clean,lag.max = 28)
pacf(clean,lag.max = 28)

# 방법 3. 만두검정 생략
# 방법 4. 함수
ndiffs(clean)
nsdiffs(clean)

###최종결론
# 오토 아리마에서 계절차분 + ma(1)

#따라서 일단 여러가지 후보 아리마 모형을 만들어서 aic, r^2로 비교예정
##arima 적합
arima1 <- arima(ind_out$resid ,seasonal = list(order = c(0,1,0),period=7))

arima2 <- arima(ind_out$resid, order = c(0,0,1),
                seasonal = list(order = c(0,1,0),period=7))

arima3 <- arima(ind_out$resid , order = c(1,0,1),
                seasonal = list(order = c(0,1,0),period=7))
#white noise check arima1
ts.plot(arima1$residuals[8:35])
#coeftest
#정규성
qqnorm(arima1$residuals);qqline(arima1$residuals,col='red') #정규성 만족?
#acf pacf
acf(arima1$residuals[8:35]) #ma(1)도 괜찮고 다 범위내이기도함
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
# 화이트 노이즈

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
# 화이트노이즈

# AIC비교
c(AIC(arima1), AIC(arima2), AIC(arima3))

# 최종 모형 arima2으로 결정
############################################
####분해법과 sarima 비교
pred_arima <- predict(arima2,35)

ind_out$coefficients
arima_train = exp(2.3472743 + fitted(arima3) + 0.6282553*coo1_cul + 0.8845800*chuseok )
arima_test =  exp(2.3472743 + pred_arima$pred)

new_coo1 = data.frame(t = c(1:70),
                      ind1 = c(rep(0,21),rep(1,6),rep(0,36),1,rep(0,6)),
                      chuseok = c(1,1,1,1,1,1,1,1,rep(0,62)),
                      coo1_holi = c(holi9[7:length(holi9)],holi10,holi11[1:15]),
                      coo0_cul = c(holi9[7:length(holi9)],holi10,holi11[1:15]))
pred = predict(final_kongjo_seat, new_coo1,interval = "prediction")
predic = exp(pred)

decompose_train = predic[8:35]
decompose_test = predic[35:70]
#RMSE
rbind(rmse_arima_tr = RMSE(arima_train[8:35],coo1[8:35]),
      rmse_arima_test = RMSE(arima_test,coo1[36:70]),
      rmse_dc_tr = RMSE(decompose_train , coo1[8:35]),
      rmse_dc_test = RMSE(decompose_test , coo1[36:70]))
#MAE
rbind(rmse_arima_tr = MAE(arima_train[8:35],coo1[8:35]),
      rmse_arima_test = MAE(arima_test,coo1[36:70]),
      rmse_dc_tr = MAE(decompose_train , coo1[8:35]),
      rmse_dc_test = MAE(decompose_test , coo1[36:70]))

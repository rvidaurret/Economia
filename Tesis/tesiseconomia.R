library(zoo)
library(tseries)
library(urca)
library(vars)

#Importar las series
series <- read.table(file = "seriesanuales.txt", header = TRUE, sep = "	", stringsAsFactors = FALSE)
head(series)

#Definir variables en logarítmos
tiempo <- series$tiempo
pib <- log(series$PIB)
ibif <- log(series$IBIF)
m0 <- log(series$M0)
m1 <- log(series$M1)
m2 <- log(series$M2)
m3 <- log(series$M3)
ipc <- log(series$IPC)

#Test AFD para los tres modelos
##Creiterio de selección de Akaike
adfpib <- ur.df(pib, type = "trend", selectlags = "AIC")
summary(adfpib)

#Variables diferenciadas
difpib <- diff(pib)
difibif <- diff(ibif)
difm0 <- diff(m0)
difm1 <- diff(m1)
difm2 <- diff(m2)
difm3 <- diff(m3)
difipc <- diff(ipc)

#Testeo de raíz unitaria para variables diferenciadas
adfdifm2 <- ur.df(difm2, type="trend", selectlag = "AIC")
summary(adfdifm2)

#Segundas diferencias
dif2m0 <- diff(difm0)
dif2m1 <- diff(difm1)
dif2m2 <- diff(difm2)
dif2m3 <- diff(difm3)
dif2ipc <- diff(difipc)

adfdif2m3 <- ur.df(dif2m2, type ="trend", selectlags = "AIC")
summary(adfdif2m2)

#Armo los VAR
pib_m2 <- cbind(dif2m2, difpib[-1])
colnames(pib_m2) <- c("dif2_m2", "dif_pib")
#Selección de lags
lags_var <- VARselect(pib_m2, lag.max = 10, type = "const")
lags_var$select

#Estimación del VAR
varestimado <- VAR(pib_m2, p = 3, type = "const")

#Matriz triangular inferior
amat <- diag(2)
amat[2,1] <- NA

#Estimación del SVAR
svarestimado <- SVAR(varestimado, estmethod = "direct", Amat = amat)

#Funciones de impulso-respuesta
fun_pib_m2 <- irf(svarestimado, impulse = "dif2m2", response = "difpib", ortho = TRUE, cumulative = TRUE, n.ahead = 11 ,ci = 0.90)
plot(fun_pib_m2)




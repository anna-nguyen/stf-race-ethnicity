library(rSymPy)



b0 <- Var("b0")
b1 <- Var("b1")
b2 <- Var("b2")
b3 <- Var("b3")


param <- sympy("(1-(exp(b0+b1+b2+b3)-exp(b0+b1))/(exp(b0+b2)-exp(b0)))")

ss0<-sympy(paste("diff(",param,",b0,1)",sep=""))

ss1<-sympy(paste("diff(",param,",b1,1)",sep=""))

ss2<-sympy(paste("diff(",param,",b2,1)",sep=""))

ss3 <-sympy(paste("diff(",param,",b3,1)",sep=""))




ss <- c(ss0,ss1,ss2,ss3)

#Simulate Data (can ignore - just used your values of betas and VCov)
set.seed(1231) 
library(data.table)
m <- 500
dat.wide <- data.table(id = 1:m, Xi1=0, Xi2=0, Xi3=1, Xi4=1, Ti1=0,Ti2=1,Ti3=0, Ti4=1)
dat.long = melt(dat.wide, measure.vars = list(c(2:5),c(6:9)),  value.name = c("Xij","Tij"),id.vars=1:4)
setorder(dat.long,id,Xij,Tij)


# values in paper
betas<-c(b0=-8.42,b1=.1721,b2=0.72,b3=-0.40)
design.mat <- data.frame(1,Xij=dat.long$Xij,Tij=dat.long$Tij,Inter=dat.long$Xij*dat.long$Tij)
design.mat <- as.matrix(design.mat)

lambdaY = exp(design.mat %*% betas)
Y <- rpois(nrow(design.mat),lambdaY)

dat.long <- data.frame(dat.long,Y=Y)
dat.long <- dat.long %>% mutate(XT = Xij*Tij)

glm.pois <- glm(Y~Xij+Tij+XT,data=dat.long,family=poisson)

betashat <- coefficients(glm.pois)
vbeta <- vcov(glm.pois)

# from Jades Paper
vbeta.jade<-rbind(c(0.005952352,-0.005952352, -0.005952352, 0.005952352),c(-0.005952352, 0.008809490, 0.005952352, -0.008809490),c(-0.005952352, 0.005952352, 0.014801884, -0.014801884),c(0.005952352, -0.008809490, -0.014801884, 0.023988043))


Delta.method <- function(VC,betas,gradients, parameter) {

b0<-betas[1]    
b1<-betas[2]    
b2<-betas[3]
b3<-betas[4]    

grad <- NULL
for(i in 1:4) {
 grad <- c(grad,eval(parse(text=gradients[i]))) 
}

est <- eval(parse(text=parameter))

SE <- sqrt(t(grad) %*% VC %*% grad)
return(list(est=est,SE=SE))
}

Delta.method(vbeta.jade,betas,ss,param)

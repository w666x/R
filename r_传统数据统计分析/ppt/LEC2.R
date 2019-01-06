setwd("E:/Rexercise1")
example("mean")
example("glm")
split.screen(c(2,1))
split.screen(c(1,3), screen = 2)
screen(1)
plot(10:1)
screen(2)
plot(10:1)
screen(3)
plot(10:1)
x=1:10
y=12:21
z=matrix(c(1:100),10)
image(x,y,z)
persp(x,y,z)
sunflowerplot(x,y)
points(x,y)
points(y,x)
text(2,20,"hello")
mtext("hello",side = 2,line = 2)
legend(2,20,"hi")
plot(rep(1,10),ylim=c(-2,1.2),pch=1:10,cex=3,axes=F,xlab="",ylab="")

text(rep(0.6,10),as.character(1:10))
points(rep(0,10),pch=11:20,cex=3)
text(rep(-0.4,10),as.character(11:20))
points(rep(-0.8,5),pch=21:25,cex=3)
text(rep(-1.2,5),as.character(21:25))
points(6:10, rep(-0.8,5),pch=c("*","?","X","x","&"),cex=3)
text(6:10,rep(-1.2,5),c("*","?","X","x","&"))

opar <- par()
par(bg="lightyellow", col.axis="blue", mar=c(4, 4, 2.5, 0.25))
plot(x, y, xlab="Ten random values", ylab="Ten other values",
     xlim=c(-2, 2), ylim=c(-2, 2), pch=22, col="red", bg="yellow",
     bty="l", tcl=-.25, las=1, cex=1.5)
title("How to customize a plot with R (bis)", font.main=3, adj=1)
par(opar)

opar <- par()
par(bg="lightgray", mar=c(2.5, 1.5, 2.5, 0.25))
plot(x, y, type="n", xlab="", ylab="", xlim=c(-2, 2),
     ylim=c(-2, 2), xaxt="n", yaxt="n")
rect(-3, -3, 3, 3, col="cornsilk")
points(x, y, pch=10, col="red", cex=2)
axis(side=1, c(-2, 0, 2), tcl=-0.2, labels=FALSE)
axis(side=2, -1:1, tcl=-0.2, labels=FALSE)
title("How to customize a plot with R (ter)",
      font.main=4, adj=1, cex.main=1)
mtext("Ten random values", side=1, line=1, at=1, cex=0.9, font=3)
mtext("Ten other values", line=0.5, at=-1.8, cex=0.9, font=3)
mtext(c(-2, 0, 2), side=1, las=1, at=c(-2, 0, 2), line=0.3,
      col="blue", cex=0.9)
mtext(-1:1, side=2, las=1, at=-1:1, line=0.2, col="blue", cex=0.9)
par(opar)

data(women)
lm.wm=lm(weight~height,data = women)
summary(lm.wm)
print(lm.wm)
lm.wm
plot(lm.wm)
return
search()
summary("base")
methods("base")

cv=function(x){
  sd(x/mean(x))
}
cv(-1)
traceback()

ft<-function(m){
  if(m==1) rlt<-1
  else rlt<-m*ft(m-1)
  return(rlt)
}

debug(ft)
ft(3)
undebug(ft)

browser(ft)
browser(ft)
n

x=rnorm(100000)
y=rnorm(100000)
z=c()
for (i in 1:100000)
  z=c(z,x[i]+y[i])
z=rep(NA,100000)
##检测程序的运行时间
system.time({
  for (i in 1:100000)
    z=c(z,x[i]+y[i])
  })

for (i in 1:100000)
  z[i]=x[i]+y[i]

Rprof(for (i in 1:100000)
  z[i]=x[i]+y[i])

###R中的调用
void foo(int *nin, double *x)
{
int n = nin[0];
int i;
for (i=0; i<n; i++)
  x[i] = x[i] * x[i];
}

n<-1000
u<-runif(n)
x<-u^(1/3)
hist(x,prob=TRUE,main=expression(f(x)==3*x^2))
y<-seq(0,1,0.01)
lines(y,3*y^2)

x=1:10
as.integer(5<x)

rgeom(10,0.3)
rpois(10,2)

sample(letters)
sort(sample(letters))

ls()
rm(y,u,k)


length(y)
u=runif(1)

p<-seq(.1,.9,.1)
p
Qhat<-quantile(y,p)
Q<-qbeta(p,2,2)
se<-sqrt(p*(1-p)/(n*dbeta(Q,2,2)))
round(rbind(Qhat,Q,se),3)

n<-1000
u<-runif(n)
x<-u^(1/3)
hist(x,prob=TRUE,main=expression(f(x)==3*x^2))
y<-seq(0,1,0.01)
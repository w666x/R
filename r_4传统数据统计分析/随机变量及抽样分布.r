##�����������ֲ�
#����ֲ�
n=20;p=0.5;x=1:5;
p=choose(n,x)*p^x*(1-p)^(n-x);
plot(x,p,'h')
#�����ηֲ�
N=100;M=5;n=10;x=1:5;
P=choose(N-M,n-x)*choose(M,x)/choose(N,n);
plot(x,P,'h')
#
m=10;n=7;k=8;
x=0:(k+1);
px=phyper(x,m,n,k)
dx=dhyper(x,m,n,k)
cbind(px,dx)
#���ɷֲ�
lamda=6;x=0:5;
P=lamda^x*exp(-lamda)/factorial(x)
plot(x,P,'h')
##�������������
#���ȷֲ�
x=0:1;
y=c(1,1)
plot(x,y,ylim=c(0,1.5),type='l')
#��̬�ֲ�
x=seq(-4,4,0.1)
y=1/sqrt(2*pi)*exp(-x^2/2)
plot(x,y,type='l')
P=pnorm(2)  #��̬�ֲ����ۻ�����
alpha=qnorm(0.95) #��̬���ʵķ�λ��
#ָ���ֲ�
x=0:50;
y=0.3*exp(-0.3*x)
plot(x,y,type='l')
P=pexp(2) #ָ���ֲ����ۻ�����
alpha=qexp(0.85) #ָ���ֲ��ķ�λ��
##��������������
#����ֲ������
rbinom(10,1,0.5)
rbinom(10,5,0.35)
n=100;p=0.25
c=rbinom(n,50,p)
hist(c,prob=T,main=paste("n=",n))
xn=0:n
points(xn,dbinom(xn,n,p),type="h",lwd=3)
#����������ֲ���
rhyper(15,10,5,3) 
#���ɷֲ������
rpois(10,lambda=4)
##�������������
runif(5,0,1)
runif(3,1,3)
runif(5)
#����10000�����ȷֲ�����������������ֱ��ͼ
x=runif(100000,0,1)
hist(x,prob=T,ylim=c(0,1.5),main='uniform(0,1)')
curve(dunif(x,0,1),add=T)  #��Ӿ��ȷֲ����ܶȺ�����
#��̬�ֲ������
rnorm(5,10,5)
rnorm(5)
#����1000����׼��̬�ֲ�����������������ֱ��ͼ
x=rnorm(1000)
hist(x,prob=T,ylim=c(0,0.5),main='N(0,1)')
curve(dnorm(x),add=T)
#ָ���ֲ������
rexp(10,1)
#����1000����ֵΪ1/10��ָ���ֲ������
x=rexp(1000,1/10)
hist(x,prob=T,ylim=c(0,0.1),main="exp(1/10)")
curve(dexp(x,1/10),add=T)
#�����ֲ�
sample(c("H","T"),10,rep=T)
sample(100,10,rep=T)
##�����ֲ��Ļ�������
#�����ֲ�
x=seq(0,20,0.1)
curve(dchisq(x,2),0,20,ylab="p(x)")  #���������ɶ�Ϊ2�Ŀ����ֲ���Y��Ϊp(x)ֵ
curve(dchisq(x,4),add=T,lty=2) #�����ɶ�Ϊ4�Ŀ����ֲ�����������Ϊ2����ͬ
curve(dchisq(x,6),add=T,lty=3)
curve(dchisq(x,8),add=T,lty=4)
curve(dchisq(x,10),add=T,lty=5)
legend(13,0.4,c("n=2","n=4","n=6","n=8","n=10"),lty=1:5,bty="n")
#t�ֲ�
x=seq(-4,4,0.01)
plot(x,dnorm(x),type="l",lty=1)
for (i in c(1,5,10))
points(x,dt(x,df=i),type="l",lty=i+1)
legend(2,0.3,c("N(0,1)","t(10)","t(5)","t(1)"),lty=1:4,bty="n")
#F�ֲ�
x=seq(0,6,0.1)
plot(x,df(x,3,3),type="l",ylim=c(0,1.2),ylab="p(x)")
curve(df(x,5,5),add=T,lty=3)
curve(df(x,10,10),0.6,add=T,lty=4)
curve(df(x,20,20),add=T,lty=5)
curve(df(x,30,30),add=T,lty=6)
legend(4,1,c("n=3","n=5","n=10","n=20","n=30"),lty=2:6,bty="n")  #�˴���4��1����ʾͼ����λ�õ�
#���ļ��޶����Ӧ��
m=100 #m��ģ�����
n=10;p=0.25;
z=rbinom(m,n,p)  #����100�����������
x=(z-n*p)/sqrt(n*(1-p)*p)  #��100�������������׼��
hist(x,prob=T,main=paste("n=",n))
curve(dnorm(x),add=T) #������̬����
##�����ֲ����ٽ�ֵ�ı��
#��׼��̬�ֲ����ʱ�
u0=seq(0,3,by=0.1) #�ٽ�����ֵ
u.0=seq(0,0.1,by=0.01)  #�ٽ�����ֵ
u=u0+matrix(u.0,31,11,byrow=T) #�ٽ��λ��
p=pnorm(u)  #�ٽ�����ֵ
colnames(p)<-u.0  #�ٽ���б��
rownames(p)<-u0  #�ٽ���б��
p  #�γ���̬�ֲ��ٽ��
#t�ֲ����ٽ�ֵ�ı��
a=c(0.01,0.025,0.05,0.1,0.5,0.95,0.975,0.99)  #β������
n=2:20  #������
cbind(n,"0.01"=qt(0.01,n),"0.025"=qt(0.025,n))

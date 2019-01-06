###求解PI的估计值
#Buffon掷针问题R模拟
buffon=function(n,l=0.8,a=1)
{
  k=0
  #产生随机数
  theta=runif(n,0,pi);
  x=runif(n,0,a/2)
  #模拟实验，检验如果不等式成立，则计数
  for (i in 1:n){
    if (x[i] <= l/2*sin(theta[i]))
      k=k+1
  }
  #由计数所得的概率值，求得pi的估计值
  2*l*n/(k*a)
}
buffon(n=10000000,l=2,a=3)

MC1 <- function(n){
  #产生0,1内的随机数x,y
  k <- 0; x <- runif(n); y <- runif(n)
  #统计随机数（x,y）落到1/4扇形区域的个数
  for (i in 1:n){
    if (x[i]^2+y[i]^2 < 1)
      k <- k+1
  }
  4*k/n
}
MC1(10000)
MC1(1000000)

###模特卡罗方法的精度分析（确定到达预期精度所需要的实验次数）
#例10.5
num=function(p=0.9,alpha=0.05,h=0.05)
{
  N=qnorm(1-alpha/2)
  n=p*(1-p)/h^2*N
} 
Num=matrix(rep(0,25),5)
p=seq(0.5,0.9,length=5)
h=seq(0.001,0.05,len=5)
for (i in 1:5)
  for (j in 1:5)
    Num[i,j]=num(p[i],h=h[j])
Num

#例10.6
#用平均值法估计圆周率Pi
MC1_2=function(n)
{
  X=runif(n)
  4*sum(sqrt(1-X^2))/n
}
MC1_2(10000)
MC1_2(100000000)

#随机数的产生
help("mod")
runif(5,0,1)
rnorm(5)
rpois(5,4)

###连续系统模拟
####画出A, B, C, D 和 O 点五个点的位置，再作标记
plot(c(0,1,1,0), c(0,0,1,1), xlab =" ", ylab = " ")
text(0, 1, labels="A", adj=c( 0.3, 1.3))
text(1, 1, labels="B", adj=c( 1.5, 0.5))
text(1, 0, labels="C", adj=c( 0.3, -0.8))
text(0, 0, labels="D", adj=c(-0.5, 0.1))
points(0.5,0.5); text(0.5,0.5,labels="O",adj=c(-1.0,0.3))
####将计算出的各点位置存入矩阵X,Y中
####X是ABCD四点的X值，Y是ABCD四点的Y值
delta_t<-0.01; n=110
x<-matrix(0, nrow=5, ncol=n); x[,1]<-c(0,1,1,0,0)
y<-matrix(0, nrow=5, ncol=n); y[,1]<-c(1,1,0,0,1)
#定义储存距离的一向量
d<-c(0,0,0,0)
for (j in 1:(n-1)){
  for (i in 1:4){
    d[i]<-sqrt((x[i+1, j]-x[i,j])^2+(y[i+1, j]-y[i,j])^2)
    ##坐标（X,Y）的变化就是他们实际运动的距离乘以偏移角度的三角函数
    x[i,j+1]<-x[i,j]+delta_t*(x[i+1,j]-x[i,j])/d[i]
    y[i,j+1]<-y[i,j]+delta_t*(y[i+1,j]-y[i,j])/d[i]
  }
  ##因为第四个点需要参照第一个点运动，故如此赋值
  x[5,j+1]<-x[1, j+1]; y[5, j+1]<-y[1, j+1]
}
####画出相应的曲线
for (i in 1:4) lines(x[i,], y[i,])


###离散型系统模拟
MC2=function(n)
{
  ###产生随机数以及定义t1,t3的存储位置
  r1=runif(n);r2=runif(n);t2=rnorm(n,30,2)
  t1=array(0,dim = c(1,n));t3=t1;
  #按照所给的分布，产生符合分布的随机数列  
  for (i in 1:n)
  {
    if (r1[i]<=0.7)
      t1[i]=0
    else if (r1[i]<=9)
      t1[i]=5
    else
      t1[i]=10
  }
  for (i in 1:n)
  {if (r2[i]<=0.3)
    t3[i]=28
  else if (r2[i]<=0.7)
    t3[i]=30
  else if (r2[i]<=0.9)
    t3[i]=32
  else
    t3[i]=34
  }
  ###比较当这一情况出现时的概率
  k=0
  for (i in 1:n)
    if (t1[i]+t2[i]>t3[i])
      k=k+1
  k/n
}
##作十万次试验
MC2(100000)
MC2(1000000)


###核反应堆屏蔽层设计问题
MC3=function(n)
{
  ##定义隔离板的距离，以及中子返回，吸收，穿透的计数向量
  D=3;pi=3.1416;back=0;absorb=0;pierce=0
  ##重复n次试验
  for (k in 1:n)
  {
    ###根据D=3，我们可以得到d=1,此处是得到初次时，垂直进入的距离
    ###经过十次碰撞而非运动十段，故按每碰撞一次计数
    x=-log(runif(1))
    ##最多可以进行10次碰撞
    for (i in 1:10)
    {
      index=1
      r=runif(2);
      ###产生两列随机数，且随之确定运动距离以及反射角
      R=-log(r[1])
      t=2*pi*r[2]
      x=x+R*cos(t)
      ###判断在i0次碰撞中，所能发生的所有情况
      if (x<0)
      {
        back=back+1;index=0;break
      }
      else if (x>D)
      {
        pierce=pierce+1;index=0;break
      }
      else
        next
    }
    ##当十次结束时，也没有发生返回或者穿透时，则表示将被吸收
    if (index==1)
      absorb=absorb+1
  }
  data.frame(Pierce=pierce/n*100,Absorb=absorb/n*100,Back=back/n*100)
}
###
MC3(1000)
MC3(100000)

##确定欲达到预期的精度需要的试验次数
(1-10^-6)*1.96^2

###模拟方法在排队论中的应用
###lambda为顾客前来所属参数为lambda的指数分布，T为总服务时间，mu为平均服务人数
Queue1=function(lambda,mu,T)
{
  ###k=0即为初始状态，wt发生事件的时间，wn系统中的顾客数，ws记录上一事件到下一事件的间隔时间
  ###tp为一控制符，等于一时结束。nA为t时刻到达系统的顾客总数，n为当前时刻到达系统的顾客数
  ###t为时间变量，r为产生的一随机数，tA为顾客的到达时间（模拟泊松过程到达），tD为顾客离开时间
  k=0;wt=0;wn=0;ws=0;
  tp=0;nA=0;n=0;t=0;
  r=runif(1);tA=-1/lambda*log(r);tD=Inf
  ###开始循环
  repeat
  {
    ###k是计数用的，顾客初始到达时间t即为0，系统中的顾客数也记为0
    k=k+1;wt[k]=t;wn[k]=n
    if (tA<T)
    {
      ws[k]=min(tA,tD)-t
      if (tA<tD)
      {
        t=tA;n=n+1;nA=nA+1
        r=runif(1);tA=t-1/lambda*log(r)
        if (n==1)
        {
          r=runif(1);tD=t-1/mu*log(r)
        }
      }
      else
      {
        t=tD;n=n-1
        if (n==0)
          tD=Inf
        else
        {
          r=runif(1);tD=t-1/mu*log(r)
        }
      }
    }
    else
    {
      ws[k]=if (tD==Inf) 0 else tD-t
      if (n>0)
      {
        t=tD;n=n-1
        if (n>0)
        {
          r=runif(1);tD=t-1/mu*log(r)
        }
      }
      else tp=1
    }
    if  (tp==1) break
  }
  data.frame(Ls=sum(ws*wn)/t,Ws=sum(ws*wn)/nA,Pwait=sum(ws[wn>=1]/t))
}
###
Queue1(lambda = 4,mu=10,T=1000)
Queue1(lambda = 0.6,mu=0.8,T=10000)

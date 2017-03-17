library("RMySQL")

# 去掉负值
f<-function(x) {
 for (i in 1:length(x)) {
    x[i] = max(0,x[i])
 }
 return(x)
}
	
# 取出一天的数据，计算差量，最小为0

ana <- function(con,qname='%23_lv2toapp%',d1='2014-01-30') {

	d2 = as.Date(d1)+1

	#dbGetQuery(con,"select * from histories where queue_name like '%23_lv2toapp%' ");

    str = paste("select deque,created_at from histories where queue_name like '",qname,"' and created_at BETWEEN '",d1, "' AND '", d2,"'",sep='')

	x <- data<-dbGetQuery(con, str);

	print(x)
	x <- x$deque

	len <- length(x)
	
	x1<-x[2:len]
	x2<-x[1:len-1]
	y = x1 - x2  #计算差量
	#plot(y,type='l',col='red')
	#hist(y)
	#barplot(f(y),col='blue')

	z = f(y) #最小为0

	for(i in len:24) {
		z[i]=0
	}
	print(z)
	return(z)
}

test=function(len=10,qname='%23_lv2toapp%'){
	x=c()
	y=c()
	stopifnot(len >= 0)

	start=Sys.Date()
	drv = dbDriver("MySQL")
	con = dbConnect(drv,dbname='db',user='user', password='pwd',host='ip',port=3306)

	for(i in len:0) {
		t = ana(con,qname,d1=start-i)
		x=c(x,t)  #拼接起来
		y[len-i+1]=sum(t)  #累计
	}
	#print(x)
	print("y:")
	print(y)

	y=prettyNum(y,big.mark = ",")

	str =""
	for(i in 1:(len+1)) {
		str = paste(str,y[i],"  ")
	}

	#颜色交错，xlab打印累计值
	barplot(x, col = c(rep("blue",24) , rep("red", 24)),legend.text =qname,xlab=str) 

	dbDisconnect(con)
}
# source("E:\\c\\userful\\cylyze\\ex.r")
#debug(ana)
#ana()

testall=function(){
	pdf("./public/1.pdf")

	N <- 6
	
	test(N,"%23_lv2toapp%")
	test(N,"%shop_add%")

	
	dev.off()	
}

testall()
q()


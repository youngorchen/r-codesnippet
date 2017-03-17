library("RMySQL")


drv = dbDriver("MySQL")

con = dbConnect(drv,dbname='db',user='user', password='pwd',host= 'ip',port = 3306)

print(con)


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

getdata <- function(len=10,qname='%23_lv2toapp%',mydata){
	stopifnot(len >= 0)

	mydata<- list(x=c(),y=c(),str="")
	
	start=Sys.Date()	

	for(i in len:0) {
		t = ana(con,qname,d1=start-i)
		mydata$x=c(mydata$x,t)  #拼接起来
		mydata$y[len-i+1]=sum(t)  #累计
	}
	
	mydata$y=prettyNum(mydata$y,big.mark = ",")
	print("y:")
	print(mydata$y)

	mydata$str =""
	for(i in 1:(len+1)) {
		mydata$str = paste(mydata$str,mydata$y[i],"  ")
	}
	return(mydata)
}

test1 <- function(len=10,qname='%23_lv2toapp%'){
	mydata <- getdata(len,qname,mydata)
	#print(mydata)
	#颜色交错，xlab打印累计值
	barplot(mydata$x, col = c(rep("blue",24) , rep("red", 24)),legend.text =qname,xlab=mydata$str) 	
}

test <- function(len=10,qname='16_add'){
	qname1=paste("%/queue/",qname,"%",sep='')
	qname2=paste("%zapp_",qname,"%",sep='')
	qname3=paste("%zo_",qname,"%",sep='')

	mydata1 <- getdata(len,qname1,mydata1)
	barplot(mydata1$x, col = c(rep("blue",24) , rep("red", 24)),legend.text =qname1,xlab=mydata1$str) 

	mydata2 <- getdata(len,qname2,mydata2)
	barplot(mydata2$x, col = c(rep("blue",24) , rep("red", 24)),legend.text =qname2,xlab=mydata2$str) 

	mydata3 <- getdata(len,qname3,mydata3)
	barplot(mydata3$x, col = c(rep("blue",24) , rep("red", 24)),legend.text =qname3,xlab=mydata3$str) 

	z=c()

	#只取一天
	M = 2
	for(i in ((len + 1 - M)*24):((len+1)*24)) {
		z = c(z,mydata1$x[i],mydata2$x[i],mydata3$x[i])
	}

	str1 = ""
	for(i in (len + 2 -M):(len+1)) {
		str1 = paste(str1,mydata1$y[i],"  ")
	}
	str2 = ""
	for(i in (len + 2 -M):(len+1)) {
		str2 = paste(str2,mydata2$y[i],"  ")
	}
	str3 = ""
	for(i in (len + 2 -M):(len+1)) {
		str3 = paste(str3,mydata3$y[i],"  ")
	}
	barplot(z, col = c(rep(c("#FF0000","#ff8888","#222222"),24) , rep(c("#0000FF","#8888FF","#222222"),24)),legend.text =qname,xlab=paste(str1,str2,str3,sep="\n"))

}

# source("E:\\c\\userful\\crnaylyze\\ex.r")
#debug(ana)
#ana()

testall=function(name,N=6){
	pdf(name)

	test1(N,"%23_lv2toapp%")
	test1(N,"%shop_add%")

	x <- c("16","51")
	y <- c("_add","_update")

	for(i in x) {
		for(j in y){
			#test1(N,paste("%/queue/",i,j,"%",sep=''))
			#test1(N,paste("%zapp_",i,j,"%",sep=''))
			#test1(N,paste("%zo_",i,j,"%",sep=''))
			test(N,paste(i,j,sep=''))
		}
	}

	dev.off()	
	dbDisconnect(con)
}


testall("./public/1.pdf",6)
#testall("E:\\c\\userful\\1.pdf",6)
q()

#pdf("E:\\c\\userful\\1.pdf")
#test(6,"16_add")
#dev.off()


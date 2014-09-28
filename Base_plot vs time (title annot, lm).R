y<-c(1,4,5)
x<-c("2014/06/01","2014/06/02","2014/06/05")
class(x)
x<-strptime(x,"%Y/%m/%d")
class(x)

par(mfrow=c(1,2),mar=c(4,4,1,1))
plot(x,y,main="mean")
abline(h=mean(y))
text(x=mean(x),
     y=min(y)+1,
     label=round(mean(y),3))

x<-as.POSIXct(x)
class(x)
coef<-lm(y~x)
s1<-paste(signif(coef$coefficients[1],5))
s2<-as.character(signif(coef$coefficients[2],4))

plot(x,y,main="linear")
abline(lm(y~x))
mtext(paste(s1,s2,sep=" "),side=1,line=4,outer=F)




par(mfrow=c(1,1),mar=c(4,4,5,1))

n <- 5
plot(x,y)
title(main=
        bquote(
          textstyle(
            atop(
              paste("linear regress ",.(n)), # n is numeric
                 bgroup("(",
                        atop(coef[1]~~ .(s1)<0, # ~ for a space
                             c^2~~ .(s2)~area), # ~~ for larger space
                        ")"
                        )
                 )
            )
          ), 
      cex.main=2
)
abline(lm(y~x))



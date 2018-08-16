load(file.choose())
attach(gss)
library(car) #used for recoding and regression diagnostics

summary(CHILDS) #IV
summary(as.numeric(CHILDS)) #IV

summary(HRS2)#DV
summary(as.numeric(HRS2)) #DV

#setting up IV 
cbind(table(CHILDS))#freq table in a single column
cbind(table(unclass(CHILDS))) #freq table of coded values in a single column; no children is valued at 1, 1 child is valued at 2, etc

IV.childs<-NA
IV.childs <- recode(as.numeric(CHILDS), "1=0; 2=1; 3=2; 4=3; 5=4; 6=5; 7=6; 8=7; 9=8;", as.factor.result = F) #sets no children(1) as 0, 1 child(2) as 1, 2(3) children as 2, etc

cbind(table(IV.childs))

#DV
summary(HRS2)
cbind(table(HRS2)) #freq table
cbind(table(unclass(HRS2)))

#recoding
hrsrecode <- recode(as.numeric(HRS2), 

                   "8=10;        
                    10=15;         
                    15=16;        
                    16=20;       
                    20=30;      
                    30=32;     
                    32=36;    
                    36=40;  
                    40=45;
                    45=48; 
                    48=50; 
                    50=60;
                    60=65;
                    65=8"
                      , as.factor.result=F)

#regression diagnostics

summary(lm(hrsrecode~IV.childs))
diagnostics <- lm(hrsrecode ~ IV.childs)
par (mfrow= c(2,2))
plot(diagnostics)

#regression
summary(lm(hrsrecode~IV.childs))

#plot
plot(IV.childs, hrsnumeric,  
     pch = 16, 
     cex = 1.3, 
     col = "blue", 
     main = "Number of children and Hours spent at Work Weekly",   
     xlab = "Number of Children", 
     ylab = "Average Number of hours spent at Work Weekly") 
abline(lm(hrsnumeric ~ IV.childs, data = gss))  


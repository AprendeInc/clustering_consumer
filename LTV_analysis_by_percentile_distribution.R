dev.off()
table(df$country_class, df$has_bogo)

df <-merge(x = LTV_homologado_2020, y = student_uniqueinvoices_classes_by_mode, by = "salesforce_contact_id", all.x = TRUE)
nrow(df)
library(dplyr)

#################### payment mode
library(readr)
student_uniqueinvoices_classes_by_mode <- read_csv("C:/Users/luluo/Documents/SurvivalAnalysisLTV/student_uniqueinvoices_classes_by_mode.csv")
View(student_uniqueinvoices_classes_by_mode)
head(student_uniqueinvoices_classes_by_mode)

library(dplyr)
library(epiDisplay)

t<-barplot(table(student_uniqueinvoices_classes_by_mode$billing_invoice_type), xlab = 'Common payment terms', ylab = 'Number Of Users')

t<-barplot(table(student_uniqueinvoices_classes_by_mode$billing_payment_status), xlab = 'Common bulling status', ylab = 'Number Of Users')

t<-barplot(table(student_uniqueinvoices_classes_by_mode$billing_invoice_payment_terms), xlab = 'Common billing status', ylab = 'Number Of Users')
text(x = t, y = dat$freqs, label = dat$freqs, pos = 3, cex = 0.8, col = "red")



################################################################################################################
# el siguiente conjunto toma el data set por estudiantes en el periodo ene-dic 2020
library(readr)
LTV_homologado <- read_csv("C:/Users/luluo/Documents/SurvivalAnalysisLTV/files_version/LTV_homologado.csv")
View(LTV_homologado)

table(LTV_homologado$gender_class, LTV_homologado$country_class)
LTV_homologado_2020<-LTV_homologado[LTV_homologado$Fecha_de_creacion >= "2020-01-01" 
                                    & LTV_homologado$Fecha_de_creacion <= "2020-12-01",]
summary(LTV_homologado_2020$invoice_amount_accumulative)
LTV_homologado_2020$Month_Yr <- format(as.Date(LTV_homologado_2020$Fecha_de_creacion), "%m_%Y")

LTV_homologado_2020$amount_med <- ifelse(LTV_homologado_2020$invoice_amount_accumulative <=213, 
                                         "less than avg (213 USD)", "more than avg (213 USD)")
general_med<-median(LTV_homologado_2020$invoice_amount_accumulative)
colnames(LTV_homologado_2020)
table(LTV_homologado_2020$gender_class)

table(LTV_homologado_2020$country_class,LTV_homologado_2020$gender_class, LTV_homologado_2020$amount_med)
total<-length(unique(LTV_homologado_2020$salesforce_contact_id))
prcg <- table(LTV_homologado_2020$amount_med,LTV_homologado_2020$gender_class)/total * 100

mybar_grouped_product_invoices <-barplot(prcg, 
          main="Género vs LTV Class",
          xlab="LTV",ylab='% students', col=c("red","blue"),
          legend = rownames(prcg),
          args.legend = list(x = "top", bty="n", 
          ncol = 1,inset = -0.1), beside=TRUE,ylim = c(0 , 40))

text(mybar_grouped_product_invoices, 0, round(prcg, 1), cex=1, pos=3,col = "white")


table(LTV_homologado_2020$country_class,LTV_homologado_2020$gender_class, LTV_homologado_2020$amount_med)

colnames(df)
library(sjPlot)
library(sjmisc)
library(ggplot2)
data(efc)
theme_set(theme_sjplot())

fit <- lm(total_invoice_amount_per_student ~ gender_class *country_class, data = df)

fit <- lm(total_invoice_amount_per_student ~ country_class+has_bogo *popular_products, 
          data = df)

# select only levels 30, 50 and 70 from continuous variable Barthel-Index
plot_model(fit, type = "pred", terms = c("total_invoice_amount_per_student [200,250,300]","country_class", "popular_products"))
ggplot(data = df, aes(x = country_class, y = total_invoice_amount_per_student, color = country_class)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  theme_bw() +
  theme(legend.position = "null")


modelo_lineal <- glm(total_invoice_amount_per_student ~ has_bogo, data = df,family = "binomial")
ggplot(data = df, aes(x = total_invoice_amount_per_student, y = country_class)) +
  geom_point(aes(color = as.factor(total_invoice_amount_per_student)), shape = 1) + 
  geom_smooth(method = "lm", color = "gray20", se = FALSE) +
  theme_bw()  +
  labs(title = "Regresión lineal por mínimos cuadrados",
       y = "Probabilidad default") +
  theme(legend.position = "none")


plot_model(fit, type = "pred", terms = c("gender_class","country_class"))
############## bars country class and avg total invoice
total<-length(unique(LTV_homologado_2020$salesforce_contact_id))
prcg <- table(LTV_homologado_2020$amount_med,LTV_homologado_2020$country_class)/total * 100

mybar_grouped_product_invoices<-barplot(prcg, main="Country vs LTV Class",xlab="Country class",ylab='% students', 
                                        col=c("red","blue"),
                                        legend = rownames(prcg), args.legend = list(x = "top", bty="n", 
                                                                                    cex = 0.7, ncol = 1), 
                                        beside=TRUE,ylim = c(0 , 40))

text(mybar_grouped_product_invoices, prcg, round(prcg, 1), cex=0,cey=2, pos=3,col = "black",srt=0)

############## bars gender class and avg total invoice
total<-length(unique(LTV_homologado_2020$salesforce_contact_id))
prcg <- table(LTV_homologado_2020$amount_med,LTV_homologado_2020$gender_class)/total * 100

mybar_grouped_product_invoices<-barplot(prcg, main="Gender vs LTV Class",xlab="Gender",ylab='% students', 
                                        col=c("red","blue"),
                                        legend = rownames(prcg), args.legend = list(x = "top", bty="n", 
                                                                                    cex = 0.7, ncol = 1), 
                                        beside=TRUE,ylim = c(0 , 40))

text(mybar_grouped_product_invoices, prcg, round(prcg, 1), cex=0,cey=2, pos=3,col = "black",srt=0)

############## bars month and LTV class by gender
unique(df$country_class)
aggregate(df$invoice_amount_accumulative, list(df$country_class), mean)
total<-length(unique(LTV_homologado_2020$salesforce_contact_id))
prcg <- table(LTV_homologado_2020$amount_med,LTV_homologado_2020$gender_class)/total * 100

mybar_grouped_product_invoices<-barplot(prcg, main="Gender class vs LTV class",xlab="Gender class",ylab='% students', 
                                        col=c("red","blue"),
                                        legend = rownames(prcg), args.legend = list(x = "top", bty="n", 
                                                                                    cex = 0.7, ncol = 2), 
                                        beside=TRUE,ylim = c(0 , 40))

text(mybar_grouped_product_invoices, prcg, round(prcg, 1), cex=0,cey=2, pos=3,col = "black",srt=0)

############## bars month and LTV class by gender in high and low amount
fm = aov(lm(df$total_invoice_amount_per_student ~ df$country_class))

summary(fm)

unique(df$country_class)
aggregate(df$invoice_amount_accumulative, list(df$country_class), mean)
big_amount<-df[df$invoice_amount_accumulative > 213,]
small_amount<-df[df$invoice_amount_accumulative <= 213,]


total<-length(unique(small_amount$salesforce_contact_id))
prcg <- table(small_amount$gender_class,small_amount$country_class)/total * 100

mybar_grouped_product_invoices<-barplot(prcg, main="Gender and country in low LTV",xlab="Country class",ylab='% students', 
                                        col=c("red","blue"),
                                        legend = rownames(prcg), args.legend = list(x = "top", bty="n", 
                                                                                    cex = 0.7, ncol = 2), 
                                        beside=TRUE,ylim = c(0 , 40))

text(mybar_grouped_product_invoices, prcg, round(prcg, 1), cex=0,cey=2, pos=3,col = "black",srt=0)

############## bars all countries and avg total invoice per student
total<-length(unique(LTV_homologado_2020$salesforce_contact_id))
prcg <- table(LTV_homologado_2020$amount_med,LTV_homologado_2020$country)/total * 100

mybar_grouped_product_invoices<-barplot(prcg, main="Country vs LTV Class",xlab="LTV",ylab='% students', 
                                        col=c("red","blue"),
                                        legend = rownames(prcg), args.legend = list(x = "top", bty="n", 
                                                                                    cex = 0.7, ncol = 1), 
                                        beside=TRUE,ylim = c(0 , 40))

text(mybar_grouped_product_invoices, prcg, round(prcg, 1), cex=0,cey=2, pos=3,col = "black",srt=0)

library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
table(df$billing_invoice_payment_terms)

######################################################################################
####################### Densidad acumulada y sobrepuesta  ############################

ggplot(df, aes(x = total_invoice_amount_per_student)) +
  geom_histogram(aes(color = has_bogo, fill = has_bogo), 
                 position = "identity", bins = 30, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))



ggplot(df, aes(x = total_invoice_amount_per_student,group=country_class, fill=country_class)) +
  geom_histogram(aes(color = country_class, fill = country_class), 
                 position = "identity", bins = 30, alpha = 0.4) 
    +ggtitle("Total students by country") +
  scale_color_manual(values = c("yellow", "red", "blue", "pink")) +
  scale_fill_manual(values = c("yellow", "red", "blue", "pink"))

table(df$popular_products)
total<-length(df_us_mx$salesforce_contact_id)
mytable <- table(df_us_mx_popular$popular_products)/total*100
lbls <- paste(round(mytable,2), "% \n", names(mytable), sep="")
pie(mytable, labels = lbls,
    main="Popular products \n un US and MX")

df_us_mx<-df[!(df$country_class %in% c('OTHERS','CO')),]

df_popular<-df[!(df_us_mx$popular_products %in% c('Others')),]
df_us_mx_popular<-df_us_mx[!(df_us_mx$popular_products %in% c('Others')),]
##################################################################################
############# CLEANING AGE
df_age<-df
df_age$hom_age <- mutate(df_age, if (df$clean_age==1 & df$clean_age!=4){df$hom_age = 1}
                 else if (df$clean_age==4 & df$clean_age!=1){df$hom_age = 2}
                 else {df$hom_age = "Unknown"})

colnames(df_age)
unique(df_age$hom_age)

table(df_us_mx$hom_age,df_us_mx$gender_class, df_us_mx$buscando_empleo, df_us_mx$size)
buscando_empleo
servicio_telefonico
colnames(df)

################################################################################
######## POPULAR PRODUCTS
library(dplyr) 
res <- df %>% group_by(df$salesforce_product_name) %>% summarise(Freq=n())
colnames(res)
names(res)[names(res) == "df$salesforce_product_name"] <- "salesforce_product_name"
type(res)
upp<-res[res$Freq > 1000,]
b<-c(upp$salesforce_product_name)
df$popular_products <- ifelse(df$salesforce_product_name %in% b, 
                              df$salesforce_product_name, "Others")

df$popular_products<-df[df$salesforce_product_name %in% upp$salesforce_product_name,]
unique(df$popular_products)

##################################################################################
unique(df_us_mx$country_class)
unique(df_us_mx$country_class)

library(gridExtra)


png("df_us_mx_popular.png", width=480,height=480,bg = "white")
grid.table(df_us_mx_popular)
dev.off()

colors<-c("darkgreen","blue","green","yellow","pink","orange","purple","black","red","darkblue",'gray')
library(ggplot2)
library(reshape2)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
colnames(df_us_mx_popular)
ggplot(data=df_us_mx_popular, aes(x=invoice_amount_accumulative, 
                                  group=billing_invoice_payment_terms, 
                                  fill=billing_invoice_payment_terms)) +
  geom_histogram(aes(color = billing_invoice_payment_terms, fill = billing_invoice_payment_terms), 
                 position = "identity", bins = 30, alpha = 0.4)+ 
  scale_fill_manual(values = c("red","green","pink","grey","blue")) 
+ggtitle("Has bogo in MX and US by popular products") +
  theme_ipsum() +facet_wrap(~country_class,ncol=4)
+theme(strip.text = element_text(size=5))+
  abline(v = mean(df_us_mx_popular$invoice_amount_accumulative, na.rm = T),
                                                col = "black",lwd = 2)

library(viridis)
length(unique(df$country))
colores <- viridis(256)
tot<-length(df$salesforce_contact_id)
prcg <- table(df$country)/tot*100

lbls <- paste(round(prcg, 1), "% ", names(prcg), sep=" ")
pie(prcg, labels = lbls,
    main="Market share by all countries", col=viridis(33))



df_us_mx<-df[df$country_class %in% c('US','MX'),]
df_us_mx<-df[!(df$popular_products %in% c('Others')),]
nrows(df_us_mx_popular)

table(df_us_mx_popular$popular_products,df_us_mx_popular$country_class)
total<-length(unique(df_us_mx_popular$salesforce_contact_id))
prcg <- table(df_us_mx_popular$popular_products)/total * 100

mybar_grouped_product_invoices<-barplot(prcg, main="Popular products",
                                        xlab="Product name",ylab='% students', 
                                        col=c("red"), beside=TRUE,
                                        ylim = c(0 , 20))
text(mybar_grouped_product_invoices, prcg, round(prcg, 1), cex=0,cey=2, pos=3,col = "black",srt=0)

total<-length(unique(df_us_mx_popular$salesforce_contact_id))
meds_absolute_cc <- table(df_us_mx_popular$popular_products,df_us_mx_popular$country_class)
meds_cc <- table(df_us_mx_popular$popular_products,df_us_mx_popular$country_class)/total * 100
mybar_grouped_product_invoices<-barplot(meds_cc, main="Country and popular products",xlab="Popular products",ylab='% students involved', col=c("red","blue","red","blue","red","blue","red","blue","red","blue",'pink'),
                                        legend = rownames(meds_cc))

text(mybar_grouped_product_invoices, 0, round(meds_cc, 1), cex=1, pos=3,col = "white")

##############################################################################
df_us_mx_popular_paymterms<-df_us_mx_popular[df_us_mx_popular$billing_invoice_payment_terms %in% c('Financed','Upfront','Upfront Split'),]
length(df_us_mx_popular_paymterms$salesforce_contact_id)
colnames(df_us_mx_popular_paymterms)
unique(df_us_mx_popular_paymterms$billing_invoice_type)
table(df_us_mx$country_class, df_us_mx$gender_class)

colnames(df_us_mx)
table(df_us_mx$size, df_us_mx$gender_class)
summary(df$invoice_amount_accumulative)
decile(df$invoice_amount_accumulative, decreasing = FALSE)
print(chisq.test(df_us_mx$billing_invoice_payment_terms, df_us_mx$amount_med, correct=TRUE,simulate.p.value = TRUE))
print(chisq.test(df_us_mx$billing_invoice_payment_terms, df_us_mx$gender_class, correct=TRUE,simulate.p.value = TRUE))
print(chisq.test(df_us_mx$billing_invoice_payment_terms, df_us_mx$country_class, correct=TRUE,simulate.p.value = TRUE))
print(chisq.test(df_us_mx$billing_invoice_payment_terms, df_us_mx$has_bogo, correct=TRUE,simulate.p.value = TRUE))
print(chisq.test(df_us_mx$billing_invoice_payment_terms, df_us_mx$popular_products, correct=TRUE,simulate.p.value = TRUE))
print(chisq.test(df_us_mx$amount_med, df_us_mx$servicio_telefonico, correct=TRUE,simulate.p.value = TRUE))

table(df_us_mx$billing_invoice_payment_terms, df_us_mx$servicio_telefonico)
print(chisq.test(df_us_mx$billing_invoice_payment_terms, df_us_mx$servicio_telefonico, correct=TRUE,simulate.p.value = TRUE))
interaction.plot(df_us_mx$invoice_amount_accumulative,df_us_mx$country_class,df_us_mx$gender_class)

modelo2 <- aov( total_invoice_amount_per_student ~ country_class * gender_class, 
                data = df )

summary( modelo2 ) 
colnames(df)

interaction.plot( df$country_class,
                  df$gender_class, 
                  df$total_invoice_amount_per_student, 
                  ylim = c( 80, 300 ), # eje y
                  col = c("red", "#27ae60", "#5499c7","gray"), # color de líneas
                  lty = c( 5, 1, 12 ), # tipo de líneas
                  lwd = 2, # grosor de línea
                  ylab = "Media de montos totales pagados por estudiante",
                  xlab = "País", 
                  trace.label = "Género" )

unique(df_us_mx$servicio_telefonico)

print(chisq.test(df_us_mx$size, df_us_mx$hom_age, correct=TRUE))
print(chisq.test(df_us_mx$size, df_us_mx$gender_class, correct=FALSE))
print(chisq.test(df_us_mx$size, df_us_mx$country_class, correct=FALSE))
print(chisq.test(df_us_mx$size, df_us_mx$popular_products, correct=FALSE))
print(chisq.test(df_us_mx$size, df_us_mx$has_bogo, correct=FALSE))

print(chisq.test(table(df_us_mx$country_class, df_us_mx$popular_products)))
print(chisq.test(table(df_us_mx$country_class, df_us_mx$gender_class)))
print(chisq.test(table(df_us_mx$country_class, df_us_mx$has_bogo)))


print(chisq.test(table(df_us_mx$gender_class, df_us_mx$popular_products)))
print(chisq.test(table(df_us_mx$gender_class, df_us_mx$has_bogo)))


print(chisq.test(table(df_us_mx$popular_products, df_us_mx$has_bogo)))

print(chisq.test(df_us_mx$size, df_us_mx$buscando_empleo, correct=FALSE))
print(chisq.test(df_us_mx$size, df_us_mx$billing_payment_status, correct=FALSE))
print(chisq.test(df_us_mx$size, df_us_mx$servicio_telefonico, correct=FALSE))
############################################################## THE PLOT
############################################################## THE PLOT
############################################################## THE PLOT

table(df$billing_invoice_payment_terms,df_us_mx_popular$billing_invoice_payment_terms, df_us_mx_popular$popular_products)
ggplot(data=df_us_mx, aes(x=total_invoice_amount_per_student, group=gender_class, 
                    fill=gender_class)) +
  geom_histogram(aes(color = gender_class, 
                     fill = gender_class), 
                 position = "identity", bins = 20, alpha = 0.1) +
  ggtitle("Gender ") +
  theme_ipsum() +facet_wrap(~has_bogo) +theme_ipsum()
##############################################################################
colnames(df_us_mx)
colnames(df)
quantile(df_co$total_invoice_amount_per_student, probs = seq(0, 1, 1/20))
df_p95<-df[df$total_invoice_amount_per_student <436,]

df_co<-df[df$country %in% c('CO'),]

ggplot(data=df_p95, aes(x=total_invoice_amount_per_student, group=has_bogo, 
                    fill=has_bogo)) +
  geom_histogram(aes(color = has_bogo, 
                     fill = has_bogo), 
                 position = "identity", bins = 20, alpha = 0.1) +
  ggtitle("Has bogo by country") +
  theme_ipsum() +facet_wrap(~country_class) +theme_ipsum()


ggplot(df_us_mx, aes(x = total_invoice_amount_per_student,group=popular_products, 
               fill=popular_products)) +
  geom_histogram(aes(color = popular_products, 
                     fill = popular_products), 
                 position = "identity", bins = 30, alpha = 0.4) 
+ggtitle("Total students by payment terms") +theme_ipsum() 
+facet_wrap(~gender_class)
  scale_color_manual(values = c("yellow", "red")) +
  scale_fill_manual(values = c("yellow", "red"))




############### segments big amount and small amount by median
df <-merge(x = LTV_homologado_2020, y = student_uniqueinvoices_classes_by_mode, by = "salesforce_contact_id", all.x = TRUE)
names(df)[20] <- "total_invoice_amount_per_student"
nrow(df)
colnames(df)
big_amount<-df[df$total_invoice_amount_per_student > 200,]
small_amount<-df[df$total_invoice_amount_per_student <= 200,]






p1 <- ggplot(data=df, aes(x=total_invoice_amount_per_student, group=gender_class, fill=gender_class)) +
  geom_density(adjust=1.5, alpha=.4) +ggtitle("Density by age") +
  theme_ipsum()

ggplot(data=df, aes(x=total_invoice_amount_per_student, group=gender_class, fill=gender_class)) +
  geom_density(adjust=1.5) +ggtitle("Bogos density by age") +
  theme_ipsum() +
  facet_wrap(~has_bogo) +theme_ipsum()



p1 <- ggplot(data=df, aes(x=invoice_amount_accumulative, group=billing_invoice_payment_terms, fill=billing_invoice_payment_terms)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()


ggplot(data=small_amount, aes(x=invoice_amount_accumulative, group=billing_invoice_payment_terms, fill=billing_invoice_payment_terms)) +
  geom_density(adjust=1.5) +
  theme_ipsum() +
  facet_wrap(~billing_invoice_payment_terms) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()
  )

colnames(df)
library(dplyr)
rename(count(df, df$salesforce_product_name), Freq = n)

################################ age bars
table(df$country_class,df$clean_age)
total<-length(unique(LTV_homologado_2020$salesforce_contact_id))
prcg <- table(df$clean_age)/total * 100

mybar_grouped_product_invoices<-barplot(prcg, main="Age bars",xlab="Age",ylab='No students', col=c("red"),
                                        legend = rownames(prcg), args.legend = list(x = "top", bty="n", 
                                          cex = 0.7, ncol = 5), beside=TRUE,ylim = c(0 , 40))

text(mybar_grouped_product_invoices, prcg, round(prcg, 1), cex=0,cey=2, pos=3,col = "black",srt=0)

########################################
##########################################################



p1 <- ggplot(data=LTV_homologado_2020, aes(x=invoice_amount_accumulative, group=Month_Yr, fill=Month_Yr)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()

ggplot(data=LTV_homologado_2020, aes(x=invoice_amount_accumulative, group=gender_class, fill=gender_class)) +
  geom_density(adjust=1.5) +
  theme_ipsum() +
  facet_wrap(~gender_class) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()
  )

pie(df$value, labels = df$group, radius = 1,
    col = c("#999999", "#E69F00", "#56B4E9"))
tot<-length(LTV_homologado_2020$salesforce_contact_id)
prcg <- table(LTV_homologado_2020$country_class)/tot*100

lbls <- paste(round(prcg, 1), "% \n ", names(prcg), sep=" ")
pie(prcg, labels = lbls,
    main="Market share by country", col=viridis(4))


mytable <- table(iris$Species)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls,
    main="Pie Chart of Species\n (with sample sizes)")


library(dplyr)
library(ggplot2)
library(ggrepel)
library(forcats)
library(scales)
pie <- ggplot(prcg, aes(x = "", y = lbls, fill = fct_inorder(lbls))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_label_repel(aes(label = prop), size=5, show.legend = F, nudge_x = 1) +
  guides(fill = guide_legend(title = "Group"))







students <- table(LTV_homologado_2020$country_class)
mybar_grouped_product_invoices<-barplot(prcg, main="Percentage market share by country",xlab="Country",ylab='% students', col=c("blue"),
                                        , beside=TRUE,ylim = c(0 , 70))

text(mybar_grouped_product_invoices, prcg, round(prcg, 1), cex=0, pos=3,col = "black",srt=0)


tot<-length(small_amount$salesforce_contact_id)
prcg <- table(small_amount$gender_class)/tot*100

students <- table(big_amount$gender_class)
mybar_grouped_product_invoices<-barplot(prcg, main="Gender class down to 200USD",xlab="Gender",ylab='% students', col=c("blue"),
                                        legend = rownames(prcg), beside=TRUE,ylim = c(0 , 100))

text(mybar_grouped_product_invoices, prcg, round(prcg, 1), cex=0, pos=3,col = "black",srt=0)




p1 <- ggplot(data=LTV_homologado_2020, aes(x=invoice_amount_accumulative, group=has_bogo, fill=has_bogo)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()

ggplot(data=big_amount, aes(x=invoice_amount_accumulative, group=has_bogo, fill=has_bogo)) +
  geom_density(adjust=1.5) +
  theme_ipsum() +
  facet_wrap(~has_bogo) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()
  )

small_amount<-LTV_homologado_2020[LTV_homologado_2020$invoice_amount_accumulative <= 200,]
ggplot(data=small_amount, aes(x=invoice_amount_accumulative, group=gender_class, fill=gender_class)) +
  geom_density(adjust=1.5) +
  theme_ipsum() +
  facet_wrap(~gender_class) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()
  )


p1 <- ggplot(data=small_amount, aes(x=invoice_amount_accumulative, group=gender_class, fill=gender_class)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()
colnames(small_amount)
ggplot(data=LTV_homologado_2020, aes(x=invoice_amount_accumulative, group=has_bogo, fill=has_bogo)) +
  geom_density(adjust=1.5) +
  theme_ipsum() +
  facet_wrap(~has_bogo) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()
  )

###gender
tot<-lenght(small_amount$salesforce_contact_id)
prcg <- table(small_amount$country_class)/tot*100

students <- table(small_amount$country_class)
mybar_grouped_product_invoices<-barplot(prcg, main="Country class (down 200USD)",xlab="Country",ylab='% students', col=c("blue"),
                                        beside=TRUE,ylim = c(0 , 50))

text(mybar_grouped_product_invoices, prcg, round(prcg, 2), cex=0, pos=3,col = "black",srt=0)


###and_country
tol<-length(big_amount$salesforce_contact_id)
prcg <- table(small_amount$gender_class, small_amount$country_class)/tot*100

students <- table(small_amount$gender_class, small_amount$country_class)
mybar_grouped_product_invoices<-barplot(prcg, main="Gender and country in small amount class",xlab="Country",ylab='% students', col=c("blue","red"),
                                        legend = rownames(prcg), beside=TRUE,ylim = c(0 , 55))

text(mybar_grouped_product_invoices, prcg, round(prcg, 1), cex=1, pos=3,col = "black",srt=0)


########################################3
#########################################################





library(ggplot2)
ggplot(LTV_homologado_2020, aes(x = Fecha_de_creacion, 
                                y = invoice_amount_accumulative)) +
  geom_line(color = "indianred3", 
            size=1 ) +
  geom_smooth() +
  scale_x_date(date_breaks = '1 month') +
  labs(title = "Personal Savings Rate",
       subtitle = "2020 year",
       x = "",
       y = "Personal Savings Rate") +
  theme_minimal()


#Clasifying ltv amount by med
median(LTV_homologado_2020$invoice_amount_accumulative)
LTV_homologado_2020$median_bound <- ifelse(LTV_homologado_2020$invoice_amount_accumulative <200, 
                                   "less than median (200USD)", "more than median (200USD)")
total<-count(LTV_homologado_2020$salesforce_contact_id)
#genero vs ltv class high and low
count(LTV_homologado_2020$salesforce_contact_id)
meds_absolute <- table(LTV_homologado_2020$gender_class, LTV_homologado_2020$median_bound)
meds <- table(LTV_homologado_2020$median_bound, LTV_homologado_2020$gender_class)/total * 100
mybar_grouped_product_invoices<-barplot(meds, main="Gender vs LTV Class",xlab="Gender",ylab='% students', col=c("red","blue"),
                                        legend = rownames(meds), beside=TRUE,ylim = c(0 , 50))

text(mybar_grouped_product_invoices, 0, round(meds, 1), cex=1, pos=3,col = "black")

###country vs high amount and low amount
meds_absolute_cc <- table(LTV_homologado_2020$country_class, LTV_homologado_2020$gender_class)
meds_cc <- table(LTV_homologado_2020$gender_class, LTV_homologado_2020$country_class)/sum(table(LTV_homologado_2020$country_class)) * 100
mybar_grouped_product_invoices<-barplot(meds_cc, main="Country and LTV classes",xlab="Country",ylab='% students', col=c("red","blue"),
                                        legend = rownames(meds_cc), beside=TRUE,ylim = c(0 , 35))

text(mybar_grouped_product_invoices, 0, round(meds_cc, 1), cex=1, pos=3,col = "white")

##buscando empleo
meds_absolute_cc <- table(LTV_homologado_2020$edad_sf, LTV_homologado_2020$median_bound)
meds_cc <- table(LTV_homologado_2020$median_bound, LTV_homologado_2020$edad_sf)/sum(table(LTV_homologado_2020$edad_sf)) * 100
mybar_grouped_product_invoices<-barplot(meds_cc, main="Looking for a job and LTV classes",xlab="Edad",ylab='% students', col=c("red","blue"),
                                        legend = rownames(meds_cc), beside=TRUE,ylim = c(0 , 50))

text(mybar_grouped_product_invoices, 0, round(meds_cc, 1), cex=1, pos=3,col = "black")

##buscando empleo
meds_absolute_cc <- table(LTV_homologado_2020$buscando_empleo, LTV_homologado_2020$median_bound)
meds_cc <- table(LTV_homologado_2020$median_bound, LTV_homologado_2020$buscando_empleo)/sum(table(LTV_homologado_2020$buscando_empleo)) * 100
mybar_grouped_product_invoices<-barplot(meds_cc, main="Looking for a job and LTV classes",xlab="Buscando empleo",ylab='% students', col=c("red","blue"),
                                        legend = rownames(meds_cc), beside=TRUE,ylim = c(0 , 100))

text(mybar_grouped_product_invoices, 0, round(meds_cc, 1), cex=1, pos=3,col = "black")

##has bogo
meds_absolute_cc <- table(LTV_homologado_2020$has_bogo, LTV_homologado_2020$median_bound)
meds_cc <- table(LTV_homologado_2020$median_bound, LTV_homologado_2020$has_bogo)/sum(table(LTV_homologado_2020$has_bogo)) * 100
mybar_grouped_product_invoices<-barplot(meds_cc, main="Has bogo and LTV classes",xlab="Has bogo",ylab='% students', col=c("red","blue"),
                                        legend = rownames(meds_cc), beside=TRUE,ylim = c(0 , 100))

text(mybar_grouped_product_invoices, 0, round(meds_cc, 1), cex=1, pos=3,col = "black")


##días desde el útlimo login (?)
meds_absolute_cc <- table(LTV_homologado_2020$median_bound, LTV_homologado_2020$days_since_login)
meds_cc <- table(LTV_homologado_2020$days_since_login, LTV_homologado_2020$median_bound)/sum(table(LTV_homologado_2020$medium_source)) * 100
mybar_grouped_product_invoices<-barplot(meds_cc, main="Días desde último login and LTV",xlab="Días desde ultimo login",ylab='% students', col=c("red","blue"),
                                        legend = rownames(meds_cc), beside=TRUE,ylim = c(0 , 10))

text(mybar_grouped_product_invoices, 0, round(meds_cc, 1), cex=1, pos=3,col = "black")


#########################################################################################
### Join homologado 2020 con sources y con paymets terms frequency
#########################################################################################
##--------------------- terms frequency
library(readr)
student_uniqueinvoices_classes_by_mode <- read_csv("C:/Users/luluo/Documents/SurvivalAnalysisLTV/student_uniqueinvoices_classes_by_mode.csv")
View(student_uniqueinvoices_classes_by_mode)

nrow(LTV_homologado_2020)
unique(LTV_homologado_2020$median_bound)

#--- merge terms and 2020 students
df <-merge(x = LTV_homologado_2020, y = student_uniqueinvoices_classes_by_mode, by = "salesforce_contact_id", all.x = TRUE)
nrow(df)
library(dplyr)
res <- df %>% group_by(salesforce_product_name) %>% summarise(Freq=n())
colnames(res)
type(res)
upp<-res[res$Freq > 1000,]
b<-list(upp$salesforce_product_name)
colnames(df)






library(ggplot2)
library(reshape2)
gg <- melt(res,id="salesforce_product_name",value.name="Freq", variable.name="salesforce_product_name")
ggplot(gg, aes(x=salesforce_product_name, y=Freq, fill=salesforce_product_name))+
  geom_bar(stat="identity")+
  facet_grid(Type~.)

#---------------merge sources and prior students
library(readr)
Channel_category_rows_by_lead <- read_csv("C:/Users/luluo/Documents/SurvivalAnalysisLTV/files_version/Channel_category_rows_by_lead.csv")
View(Channel_category_rows_by_lead)

unique_by_sources<-Channel_category_rows_by_lead[!duplicated(Channel_category_rows_by_lead$salesforce_contact_id), ]

homologado2020_sources_payment_terms_sociodemographic <-merge(x = df, y = unique_by_sources, by = "salesforce_contact_id", all.x = TRUE)


#---------------merge sources and prior students
#---------------merge sources and prior students
#---------------decision tree
df<-homologado2020_sources_payment_terms_sociodemographic
attach(df)
sesmeans <- tapply(median_bound, gender_class,length)


tapply(df$invoice_amount_accumulative, df$gender_class, mean)
tapply(df$invoice_amount_accumulative, df$country_class, mean)
tapply(df$invoice_amount_accumulative, df$salesforce_product_name, mean)
mean(df$invoice_amount_accumulative)
table(median_bound)
table(gender_class)
table(country_class)
table(clean_age)
table(salesforce_product_name)
grouped_product_invoices_tab <- table(df$median_bound, df$gender_class)

mybar_grouped_product_invoices<-barplot(grouped_product_invoices_tab, 
                                        main="Género vs Invoice Class",xlab="LTV",
                                        ylab='Number of students', col=c("darkblue","red"),
                                        legend = rownames(grouped_product_invoices_tab), beside=TRUE)

text(mybar_grouped_product_invoices, 0, round(medtimerx, 1), cex=1, pos=3)


########## Example
hsb2 <- read.table('https://stats.idre.ucla.edu/stat/r/faq/hsb2.csv', header=T, sep=",")
colnames(hsb2)
attach(hsb2)
sesmeans <- tapply(math, ses, mean)
sesmeans 
femaleses = tapply(math, list(as.factor(ses), as.factor(female)), mean)
femaleses
bp <- barplot(femaleses, beside = TRUE, main = "Math by SES and gender", 
              col = c("lightblue", "mistyrose", "lavender"),
              xlab = "Gender", names = c("Male", "Female"), 
              ylab = "Mean Math Score", legend = c("Low", "Medium", "High"), 
              args.legend = list(title = "SES", x = "topright", cex = .7), ylim = c(0, 90))
text(bp, 0, round(femaleses, 1),cex=1,pos=3) 
###############################################end of the example
df_test<-homologado2020_sources_payment_terms_sociodemographic
colnames(df)

myvars <- c("country_class", "gender_class","median_bound", "billing_invoice_type",
            "billing_payment_status", "salesforce_product_name","has_bogo","clean_age")
newdata <- df[myvars]
newdata <- na.omit(newdata)

unique(newdata$median_bound)
library(tree)
# Selección muestra entrenamiento
train=sample(seq(length(newdata$median_bound)),length(newdata$median_bound)*0.70,replace=FALSE)
length(train)
newdata.tree = tree(newdata$median_bound~.,newdata,subset=train)
summary(newdata.tree)
plot(newdata.tree);text(newdata.tree,pretty=0)

library(tree)
# Selección muestra entrenamiento
train=sample(seq(length(iris$Species)),length(iris$Species)*0.70,replace=FALSE)
iris.tree = tree(iris$Species~.,iris,subset=train)
summary(iris.tree)
plot(iris.tree);text(iris.tree,pretty=0)


df.tree = tree(df$median_bound~.,df,subset=train)
summary(iris.tree)






















meds_absolute_cc <- table(homologado2020_sources_payment_terms_sociodemographic$billing_invoice_type, homologado2020_sources_payment_terms_sociodemographic$median_bound)
meds_cc <- table(homologado2020_sources_payment_terms_sociodemographic$median_bound, homologado2020_sources_payment_terms_sociodemographic$billing_invoice_type)/sum(table(homologado2020_sources_payment_terms_sociodemographic$billing_invoice_type)) * 100
mybar_grouped_product_invoices<-barplot(meds_cc, main="Billing invoice frequent type and LTV classes",xlab="Common billing invoice type",ylab='% students', col=c("red","blue"),
                                        legend = rownames(meds_cc), beside=TRUE,ylim = c(0 , 60))

text(mybar_grouped_product_invoices, 0, round(meds_cc, 1), cex=1, pos=3,col = "black")



require(tree)
homologado2020_sources_payment_terms_sociodemographic = tree(homologado2020_sources_payment_terms_sociodemographic$median_bound, data=homologado2020_sources_payment_terms_sociodemographic)
plot(tree.homologado2020_sources_payment_terms_sociodemographic)
text(tree.homologado2020_sources_payment_terms_sociodemographic, pretty = 0)

write.csv(homologado2020_sources_payment_terms_sociodemographic,"C:/Users/luluo/Documents/SurvivalAnalysisLTV/files_version/homologado2020_sources_payment_terms_sociodemographic.csv", row.names = TRUE)



##billing_invoice_type
meds_absolute_cc <- table(homologado2020_sources_payment_terms_sociodemographic$billing_invoice_type, homologado2020_sources_payment_terms_sociodemographic$median_bound)
meds_cc <- table(homologado2020_sources_payment_terms_sociodemographic$median_bound, homologado2020_sources_payment_terms_sociodemographic$billing_invoice_type)/sum(table(homologado2020_sources_payment_terms_sociodemographic$billing_invoice_type)) * 100
mybar_grouped_product_invoices<-barplot(meds_cc, main="Billing invoice frequent type and LTV classes",xlab="Common billing invoice type",ylab='% students', col=c("red","blue"),
                                        legend = rownames(meds_cc), beside=TRUE,ylim = c(0 , 60))

text(mybar_grouped_product_invoices, 0, round(meds_cc, 1), cex=1, pos=3,col = "black")

##billing_payment_status
meds_absolute_cc <- table(homologado2020_sources_payment_terms_sociodemographic$billing_payment_status, homologado2020_sources_payment_terms_sociodemographic$median_bound)
meds_cc <- table(homologado2020_sources_payment_terms_sociodemographic$median_bound, homologado2020_sources_payment_terms_sociodemographic$billing_payment_status)/sum(table(homologado2020_sources_payment_terms_sociodemographic$billing_payment_status)) * 100
mybar_grouped_product_invoices<-barplot(meds_cc, main="Billing invoice frequent status and LTV classes",xlab="Common billing invoice status",ylab='% students', col=c("red","blue"),
                                        legend = rownames(meds_cc), beside=TRUE,ylim = c(0 , 60))

text(mybar_grouped_product_invoices, 0, round(meds_cc, 1), cex=1, pos=3,col = "black")

##product name
meds_absolute_cc <- table(homologado2020_sources_payment_terms_sociodemographic$median_bound, homologado2020_sources_payment_terms_sociodemographic$salesforce_product_name)
meds_cc <- table(homologado2020_sources_payment_terms_sociodemographic$salesforce_product_name, homologado2020_sources_payment_terms_sociodemographic$median_bound)/sum(table(homologado2020_sources_payment_terms_sociodemographic$median_bound)) * 100

row.names(meds_absolute_cc)
mybar_grouped_product_invoices<-barplot(meds_absolute_cc, main="Billing invoice frequent status and LTV classes",xlab="Common billing invoice status",ylab='% students', col=c("red","blue"),
                                        legend = rownames(meds_cc), beside=TRUE,ylim = c(0 , 60))

ggplot(tally(group_by(mybar_grouped_product_invoices, median_bound, salesforce_product_name)),
       aes(x = median_bound, y = n, fill = salesforce_product_name)) +
  geom_bar(stat="identity") + labs(fill="Employment")

text(mybar_grouped_product_invoices, 0, round(meds_cc, 1), cex=1, pos=3,col = "black")


nrow(homologado2020_sources_payment_terms_sociodemographic)
names(homologado2020_sources_payment_terms_sociodemographic)

##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################

##----- sources analysis and pay terms analysis
###channel_category vs high amount and low amount
meds_absolute_cc <- table(homologado2020_sources_payment_terms_sociodemographic$median_bound, homologado2020_sources_payment_terms_sociodemographic$channel_type)
meds_cc <- table(homologado2020_sources_payment_terms_sociodemographic$median_bound, 
                 homologado2020_sources_payment_terms_sociodemographic$channel_type)/sum(table(LTV_homologado_2020$channel_type)) * 100
mybar_grouped_product_invoices<-barplot(meds_cc, main="Channel type and LTV classes",xlab="Channel type",ylab='% students', col=c("red","blue"),
                                        legend = rownames(meds_cc), beside=TRUE,ylim = c(0 , 35))

text(mybar_grouped_product_invoices, 0, round(meds_cc, 1), cex=1, pos=3,col = "white")

###country vs high amount and low amount

##billing_invoice_type
meds_absolute_cc <- table(homologado2020_sources_payment_terms_sociodemographic$billing_invoice_type, homologado2020_sources_payment_terms_sociodemographic$median_bound)
meds_cc <- table(homologado2020_sources_payment_terms_sociodemographic$median_bound, homologado2020_sources_payment_terms_sociodemographic$billing_invoice_type)/sum(table(homologado2020_sources_payment_terms_sociodemographic$billing_invoice_type)) * 100
mybar_grouped_product_invoices<-barplot(meds_cc, main="Billing invoice frequent type and LTV classes",xlab="Common billing invoice type",ylab='% students', col=c("red","blue"),
                                        legend = rownames(meds_cc), beside=TRUE,ylim = c(0 , 60))

text(mybar_grouped_product_invoices, 0, round(meds_cc, 1), cex=1, pos=3,col = "black")
colnames(homologado2020_sources_payment_terms_sociodemographic)
########################################################################################################################################################

#billing invoice and ltv
df<-homologado2020_sources_payment_terms_sociodemographic
meds_absolute_cc <- table(df$salesforce_product_name, df$median_bound)
meds_cc <- table(df$median_bound, df$salesforce_product_name)/sum(table(df$salesforce_product_name)) * 100
mybar_grouped_product_invoices<-barplot(meds_cc, main="Billing invoice frequent type and LTV classes",xlab="Common billing invoice type",ylab='% students', col=c("red","blue"),
                                        legend = rownames(meds_cc), beside=TRUE,ylim = c(0 , 60))

text(mybar_grouped_product_invoices, 0, round(meds_cc, 1), cex=1, pos=3,col = "black")


######### channel category
colnames(df)
head(df)
meds_absolute_cc <- table(df$channel_type, df$median_bound)
meds_cc <- table(df$channel_category, df$median_bound)/sum(table(df$median_bound)) * 100
mybar_grouped_product_invoices<-barplot(meds_cc, main="Billing invoice frequent type and LTV classes",xlab="Common billing invoice type",ylab='% students', col=c("red","blue"),
                                        legend = rownames(meds_cc), beside=TRUE,ylim = c(0 , 60))

text(mybar_grouped_product_invoices, 0, round(meds_cc, 1), cex=1, pos=3,col = "black")


colnames(homologado2020_sources_payment_terms_sociodemographic)




########################################################################################################################################################
########################################################################################################################################################







########################################################################################################################################################
########################################################################################################################################################


#######################################################################################







###only high ltv amount analysis
big_amount<-LTV_homologado_2020[LTV_homologado_2020$median_bound > 194,]
text(mybar_grouped_product_invoices, 0, round(meds, 1), cex=1, pos=3,col = "white")

meds_absolute_cc <- table(LTV_homologado_2020$country_class, LTV_homologado_2020$gender_class)
meds_cc <- table(LTV_homologado_2020$gender_class, LTV_homologado_2020$country_class)/sum(table(LTV_homologado_2020$country_class)) * 100
mybar_grouped_product_invoices<-barplot(meds_cc, main="Género vs country in high amount",xlab="country",ylab='% students', col=c("red","blue"),
                                        legend = rownames(meds_cc), beside=TRUE,ylim = c(0 , 35))

text(mybar_grouped_product_invoices, 0, round(meds_cc, 1), cex=1, pos=3,col = "white")




#small ltv amount
small_amount<-big_amount<-LTV_homologado_2020[LTV_homologado_2020$median_bound < 194,]


high_class <- LTV_homologado_2020[LTV_homologado_2020$invoice_amount_accumulative >196,]


boxplot(high_class$invoice_amount_accumulative ~ high_class$country_class, col = c("yellow", "blue", "white","green"), ylab = "Pago acumulado USD")
tapply(high_class$invoice_amount_accumulative, high_class$country_class, mean)

boxplot(high_class$invoice_amount_accumulative ~ high_class$gender_class, col = c("white","green"), ylab = "Pago acumulado USD")
tapply(high_class$invoice_amount_accumulative, high_class$gender_class, mean)
####################################################################################
meds <- c(by(high_class$invoice_amount_accumulative, high_class$country_class, median))
library("ggplot2")

ggplot(high_class, aes(x = high_class$bogo_items, y = high_class$invoice_amount_accumulative)) +
  geom_point(aes(color = high_class$country_class)) +
  scale_color_viridis_d() +
  theme_minimal()





ggplot(high_class, aes(factor(invoice_amount_accumulative), country_class)) +
  geom_boxplot() + 
  geom_text(data=high_class, aes(x=names(invoice_amount_accumulative), 
                                 y=country_class, label=1:3), col='red', size=10)






grouped_product_invoices_tab <- table(LTV_homologado_2020$size, LTV_homologado_2020$gender_class)

mybar_grouped_product_invoices<-barplot(grouped_product_invoices_tab, main="Género vs Invoice Class",xlab="LTV",ylab='Number of students', col=c("darkblue","red"),
                                        legend = rownames(grouped_product_invoices_tab), beside=TRUE)

legend("topright", legend=c("LOW", "HIGH"), bty="n", fill=c("azure3", "azure"))
text(mybar_grouped_product_invoices, 0, round(medtimerx, 1), cex=1, pos=3)



table(LTV_homologado_2020$size, LTV_homologado_2020$gender_class)
chisq.test(LTV_homologado_2020$size, LTV_homologado_2020$gender_class, correct=FALSE)


#p value 0.125
table(LTV_homologado$size, LTV_homologado$country_class)
chisq.test(LTV_homologado$size, LTV_homologado$country_class, correct=FALSE)
#país y class payment amount están relacionados, p valor 2.2e-16


table(LTV_homologado$size, LTV_homologado$has_bogo)
chisq.test(LTV_homologado$size, LTV_homologado$has_bogo, correct=FALSE)
#has bogo y class payment amount están relacionados, p valor 2.2e-16


table(LTV_homologado$medium_source, LTV_homologado$size)
chisq.test(LTV_homologado$size, LTV_homologado$medium_source, correct=FALSE)
#has bogo y class payment amount están relacionados, p valor 2.2e-16

#####################################################################
#####################################################################
quantile(LTV_homologado$invoice_amount_accumulative)

table(LTV_homologado$country_class, LTV_homologado$size)
names(LTV_homologado)

ba<-subset(LTV_homologado, size == "big_amount")

library(ggplot2)
ggplot(LTV_homologado) +
  aes(x = country_class, fill = size) +
  geom_bar() +
  scale_fill_hue() +
  theme_minimal()



boxplot(ba$invoice_amount_accumulative ~ ba$country_class, col = c("yellow", "blue", "white","green"), ylab = "Pago acumulado USD")
tapply(ba$invoice_amount_accumulative, ba$country_class, mean)

boxplot(ba$invoice_amount_accumulative ~ ba$gender_class, col = c("yellow", "blue"), ylab = "Pago acumulado USD")
tapply(ba$invoice_amount_accumulative, ba$gender_class, mean)



chisq
fm = aov(lm(LTV_homologado$invoice_amount_accumulative ~ LTV_homologado$gender_class))

summary(fm)


chisq.test(LTV_homologado$size ~ LTV_homologado$country_clas, correct=FALSE)

chisq.test(LTV_homologado$size ~ LTV_homologado$medium_source, correct=FALSE)

chisq.test(LTV_homologado$size, LTV_homologado$medium_source, correct=FALSE) 
hist(ba$medium_source)

#############################################################
############################################################
####### decision tree
require(tree)


LTV_homologado$size <- ifelse(LTV_homologado$invoice_amount_accumulative <194, 
                              "No", "Yes")

LTV_homologado$buscando_empleo[LTV_homologado$buscando_empleo<10]  <- "unknown" 
LTV_homologado$edad_sf[LTV_homologado$edad_sf<10]  <- "unknown" 
LTV_homologado$servicio_telefonico[LTV_homologado$servicio_telefonico<10]  <- "unknown"
LTV_homologado$edad[LTV_homologado$edad<10]  <- "unknown"
LTV_homologado$clean_age[LTV_homologado$clean_age<10]  <- "unknown"
is.na(LTV_homologado)
names(LTV_homologado)


myvars <- c("status", "medium_source", "edad","bogo_items","country_class","tenure",
            "gender_class","has_bogo","servicio_telefonico", "payment_terms_concat","size")
newdata <- LTV_homologado[myvars]



#### K MEANS
LTV_homologado_ss <- subset(LTV_homologado, LTV_homologado$invoice_amount_accumulative >=194)
LTV_homologado_ss <- replace(LTV_homologado_ss, is.na(LTV_homologado_ss), 0)
library(factoextra)
centers<-4
kmeans(LTV_homologado_ss, 4, iter.max = 10, nstart = 1)


# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(LTV_homologado_ss, 4, nstart = 25)




is.na(newdata)
kmeans(x, centers, iter.max = 10, nstart = 1)
High = ifelse(carseats$Sales<=8, "No", "Yes")
highp <- data.frame(newdata, newdata$size)
carseats = data.frame(carseats, High)


tree.highp <- tree(newdata$size, data=newdata)
summary(tree.highp)
plot(tree.highp)
text(tree.highp, pretty = 0)

plot(tree.carseats)
text(tree.carseats, pretty = 0)

tree.carseats
tree.carseats = tree(High~.-Sales, data=carseats)


library(tibble)
LTV_homologado$size <- ifelse(LTV_homologado$invoice_amount_accumulative <194, "less than 194USD", "big LTV")




library(readr)
LTV_homologado <- read_csv("C:/Users/luluo/Documents/SurvivalAnalysisLTV/LTV_homologado.csv")
View(LTV_homologado)
head(LTV_homologado)
LTV_homologado$buscando_empleo[LTV_homologado$buscando_empleo<10]  <- "unknown" 
LTV_homologado$edad_sf[LTV_homologado$edad_sf<10]  <- "unknown" 
LTV_homologado$servicio_telefonico[LTV_homologado$servicio_telefonico<10]  <- "unknown"
LTV_homologado$edad[LTV_homologado$edad<10]  <- "unknown"
LTV_homologado$clean_age[LTV_homologado$clean_age<10]  <- "unknown"
LTV_homologado$gender_class[LTV_homologado$gender=='unknow']  <- "unknown"
is.na(LTV_homologado)
names(LTV_homologado)


table(LTV_homologado$gender_class, LTV_homologado$country_class)

LTV_homologado$size <- ifelse(LTV_homologado$invoice_amount_accumulative <271, 
                              "menos de 270USD", "Más de 270USD")
LTV_homologado_tab <- table(LTV_homologado$size, LTV_homologado$has_bogo)
mybar<-barplot(LTV_homologado_tab, main="Distrbución LTV por bogos",xlab="Adquirio Bogo", col=c("darkblue","red"),
        legend = rownames(LTV_homologado_tab), beside=TRUE)


LTV_homologado_tab <- table(LTV_homologado$size, LTV_homologado$country_class)
barplot(LTV_homologado_tab, main="Distrbución LTV por país",xlab="Países", col=c("darkblue","red"),
        legend = rownames(LTV_homologado_tab), beside=TRUE)

LTV_homologado_tab <- table(LTV_homologado$size, LTV_homologado$gender)
barplot(LTV_homologado_tab, main="Distrbución LTV por género",xlab="Género", col=c("darkblue","red"),
        legend = rownames(LTV_homologado_tab), beside=TRUE)

LTV_homologado_tab <- table(LTV_homologado$size, LTV_homologado$edad)
barplot(LTV_homologado_tab, main="Distrbución LTV por edad",xlab="Edad", col=c("darkblue","red"),
        legend = rownames(LTV_homologado_tab), beside=TRUE)


LTV_homologado_tab <- table(LTV_homologado$size, LTV_homologado$servicio_telefonico)
barplot(LTV_homologado_tab, main="Distrbución LTV por servicio telefonico",xlab="Servicio telefonico", col=c("darkblue","red"),
        legend = rownames(LTV_homologado_tab), beside=TRUE)

LTV_homologado_tab <- table(LTV_homologado$size, LTV_homologado$buscando_empleo)
barplot(LTV_homologado_tab, main="Distrbución LTV para la variable buscando empleo",xlab="Buscando empleo", col=c("darkblue","red"),
        legend = rownames(LTV_homologado_tab), beside=TRUE)



####
library(readr)
grouped_product_invoices <- read_csv("C:/Users/luluo/Documents/SurvivalAnalysisLTV/grouped_product_invoices.csv")
View(grouped_product_invoices)


library(data.table)


merged<-merge(x = grouped_product_invoices, y = LTV_homologado, by = "salesforce_contact_id")

LTV_homologado_pt <- table(merged$salesforce_contact_id,merged$size, merged$billing_invoice_payment_terms)
barplot(LTV_homologado_pt, main="Distrbución LTV para la variable términos de pago",xlab="Términos de pago", col=c("darkblue","red"),
        legend = rownames(LTV_homologado_pt))

LTV_homologado_pt <- table(merged$size, merged$salesforce_product_name)
barplot(LTV_homologado_pt, main="Distrbución LTV para la variable producto",xlab="Productos", col=c("darkblue","red"),
        legend = rownames(LTV_homologado_pt))


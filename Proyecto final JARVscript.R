#PROYECTO: Análisis morfométrico de especies mexicanas 
#de Forestiera a partir de caracteres anatómicos de la madera

#Cargar datos
maderas=read.csv("D:vars_rf.csv",header=TRUE)
View(maderas)

#Separar variables cualitativas
cualitativas=maderas[,15:21]
View(cualitativas)
#Guardar cualitativas en un nuevo archivo
write.csv(cualitativas,"vars_cualitativas.csv")

#Generar variables dummies para usar en el modelo
library(fastDummies)
cualitativas_2=fastDummies::dummy_cols(cualitativas)
View(cualitativas_2)
cualitativas_3=cualitativas_2[,8:23]
View(cualitativas_3)
#Guardar variables dummies
write.csv(cualitativas_3,"vars_dummies.csv")

#Unión variables cuantitativas y dummies
maderas2=data.frame(maderas[,1:14],cualitativas_3)
View(maderas2)
#Guardar la nueva matriz incluyendo variables dummies
write.csv(maderas2,"matriz_cuant_plus_dummies.csv")

#Análisis exploratorio no gráfico por especie
library(psych)#llamar a la libreria
resumen=describeBy(maderas2[,2:14],maderas2$Especie)#describe por categor?as
resumen
str(resumen)
resumen$F.angustifolia

#Guardar resumen de cada especie
write.csv(resumen$F.angustifolia,"resumen_angustifolia.csv")
write.csv(resumen$F.durangensis,"resumen_durangensis.csv")
write.csv(resumen$F.phillyreoides,"resumen_phillyreoides.csv")
write.csv(resumen$F.racemosa,"resumen_racemosa.csv")
write.csv(resumen$F.reticulata,"resumen_reticulata.csv")
write.csv(resumen$F.rhamnifolia,"resumen_rhamnifolia.csv")
write.csv(resumen$F.rotundifolia,"resumen_rotundifolia.csv")
write.csv(resumen$F.tomentosa,"resumen_tomentosa.csv")

#Generar bases de datos con algunos descriptores para cada especie
ang=as.data.frame(t(resumen$F.angustifolia))
View(ang)
ang=ang[c(3,4,5,8,9),]
View(ang)
Especie=rep("F. angustifolia",5)
Especie
ang2=data.frame(ang,Especie)
View(ang2)

dur=as.data.frame(t(resumen$F.durangensis))
View(dur)
dur=dur[c(3,4,5,8,9),]
View(dur)
Especie=rep("F. durangensis",5)
Especie
dur2=data.frame(dur,Especie)
View(dur2)

phill=as.data.frame(t(resumen$F.phillyreoides))
View(phill)
phill=phill[c(3,4,5,8,9),]
View(dur)
Especie=rep("F. phillyreoides",5)
Especie
phill2=data.frame(phill,Especie)
View(phill2)

rac=as.data.frame(t(resumen$F.racemosa))
View(rac)
rac=rac[c(3,4,5,8,9),]
View(rac)
Especie=rep("F. racemosa",5)
Especie
rac2=data.frame(rac,Especie)
View(rac2)

ret=as.data.frame(t(resumen$F.reticulata))
View(ret)
ret=ret[c(3,4,5,8,9),]
View(ret)
Especie=rep("F. reticulata",5)
Especie
ret2=data.frame(ret,Especie)
View(ret2)

rham=as.data.frame(t(resumen$F.rhamnifolia))
View(rham)
rham=rham[c(3,4,5,8,9),]
View(rham)
Especie=rep("F. rhamnifolia",5)
Especie
rham2=data.frame(rham,Especie)
View(rham2)

rot=as.data.frame(t(resumen$F.rotundifolia))
View(rot)
rot=rot[c(3,4,5,8,9),]
View(rot)
Especie=rep("F. rotundifolia",5)
Especie
rot2=data.frame(rot,Especie)
View(rot2)

tom=as.data.frame(t(resumen$F.tomentosa))
View(tom)
tom=tom[c(3,4,5,8,9),]
View(tom)
Especie=rep("F. tomentosa",5)
Especie
tom2=data.frame(tom,Especie)
View(tom2)
str(dur2)

#Unir análisis exploratorios de las especies
resumen2=rbind(ang2,dur2,phill2,rac2,ret2,rham2,rot2,tom2)
View(resumen2)
#Guardar nuevo resumen
write.csv(resumen2,"resumen_2".csv)

#Análisis exploratorio gráfico
library(multiDimBio)
var_clas=as.matrix(scale(maderas[,2:14]))
cat=as.vector(maderas[,1])
cat
boxWhisker(data = var_clas, groups =cat, palette = 'Set1')

#Prueba de normalidad multivariada
library(MVN)
mvn(maderas2[,2:14], mvnTest ="mardia")

#Análsis de correlación, al no pasar la prueba de normalidad,
#se ejecuta con el método spearman
m=cor(maderas2[,2:14],method="spearman")
View(m)
#Guardar matriz de correlación
write.csv(m,"matriz_correlacion.csv")

#Generar pheatmap de la matriz de correlación
library("pheatmap")
pheatmap(m, display_numbers = TRUE)


#Modelo de clasificación Random Forest
library(randomForest)
library(caret)
#Definir especie como factor
maderas2$Especie <- as.factor(maderas2$Especie)
#cuantificar cuántas observaciones hay por especie
table(maderas2$Especie)

#Establecer semilla para hacer replicable el análisis
set.seed(123)
#generar indice 
indice <- sample(2, nrow(maderas2), replace = TRUE, prob = c(0.7, 0.3))
indice=as.vector(indice)

#datos de entrenamiento
train <- maderas2[indice==1,]
#datos de validación
test <- maderas2[indice==2,]
View(train)
View(test)

#Configurar modelo con datos de entrenamiento
rf <- randomForest(Especie~., data=train, proximity=TRUE) 
#salida del modelo
print(rf)

#predicciones para los datos de entrenamiento
p1 <- predict(rf, train)
#matriz de confusión
confusionMatrix(p1, train$Especie)

#predicciones para los datos de validación
p2 <- predict(rf, test)
#matriz de confusión 
confusionMatrix(p2, test$Especie)

#gráfica del RF. 
plot(rf)

#búsqueda del número de árboles
t <- tuneRF(train[,-1], train[,1],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 150,
            trace = TRUE,
            improve = 0.05)

#tamaño de árboles
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

#gráfica de la importancia de variables
varImpPlot(rf,
           sort = T,
           n.var = 20,
           main = "Top 20 - Variable Importance")

#importancia de variables
importance(rf)

#Gráfica de los grupos
MDSplot(rf, train$Especie,k=2)

#Se repite análisis con muestras dirigidas
table(maderas2$Especie)
#separando especies
angustifolia=subset(maderas2,Especie=="F.angustifolia")
View(angustifolia)
durangensis=subset(maderas2,Especie=="F.durangensis")
View(durangensis)
phillyreoides=subset(maderas2,Especie=="F.phillyreoides")
View(phillyreoides)
racemosa=subset(maderas2,Especie=="F.racemosa")
View(racemosa)
reticulata=subset(maderas2,Especie=="F.reticulata")
View(reticulata)
rhamnifolia=subset(maderas2,Especie=="F.rhamnifolia")
View(rhamnifolia)
rotundifolia=subset(maderas2,Especie=="F.rotundifolia")
View(rotundifolia)
tomentosa=subset(maderas2,Especie=="F.tomentosa")
View(tomentosa)

#indice y datos de entrenamiento y validación por especie
set.seed(123)
indice1 <- sample(2, nrow(angustifolia), replace = TRUE, prob = c(0.7, 0.3))
indice1=as.vector(indice1)
train1 <- angustifolia[indice1==1,]
test1 <- angustifolia[indice1==2,]
View(train1)
View(test1)

set.seed(123)
indice2 <- sample(2, nrow(durangensis), replace = TRUE, prob = c(0.7, 0.3))
indice2=as.vector(indice2)
train2 <- durangensis[indice2==1,]
test2 <- durangensis[indice2==2,]
View(train2)
View(test2)

set.seed(123)
indice3 <- sample(2, nrow(phillyreoides), replace = TRUE, prob = c(0.7, 0.3))
indice3=as.vector(indice3)
train3 <- phillyreoides[indice3==1,]
test3 <- phillyreoides[indice3==2,]
View(train3)
View(test3)

set.seed(123)
indice4 <- sample(2, nrow(racemosa), replace = TRUE, prob = c(0.7, 0.3))
indice4=as.vector(indice4)
train4 <- racemosa[indice4==1,]
test4 <- racemosa[indice4==2,]
View(train4)
View(test4)

set.seed(123)
indice5 <- sample(2, nrow(reticulata), replace = TRUE, prob = c(0.7, 0.3))
indice5=as.vector(indice5)
train5 <- reticulata[indice5==1,]
test5<- reticulata[indice5==2,]
View(train5)
View(test5)

set.seed(123)
indice6 <- sample(2, nrow(rhamnifolia), replace = TRUE, prob = c(0.7, 0.3))
indice6=as.vector(indice6)
train6 <- rhamnifolia[indice6==1,]
test6<- rhamnifolia[indice6==2,]
View(train6)
View(test6)

set.seed(123)
indice7 <- sample(2, nrow(rotundifolia), replace = TRUE, prob = c(0.7, 0.3))
indice7=as.vector(indice7)
train7 <- rotundifolia[indice7==1,]
test7<- rotundifolia[indice7==2,]
View(train7)
View(test7)

set.seed(123)
indice8 <- sample(2, nrow(tomentosa), replace = TRUE, prob = c(0.7, 0.3))
indice8=as.vector(indice8)
train8 <- tomentosa[indice8==1,]
test8<- tomentosa[indice8==2,]
View(train8)
View(test8)

#unión de los datos de entrenamiento por especie
train_union=rbind(train1,train2,train3,train4,train5,train6,train7,train8)
View(train_union)
#unión de los datos de validación por especie
test_union=rbind(test1,test2,test3,test4,test5,test6,test7,test8)
View(test_union)

#definir especie como factor
train_union$Especie <- as.factor(train_union$Especie)
#tabla de observaciones por especie
table(train_union$Especie)

#Nuevo modelo RF
rf1 <- randomForest(Especie~., data=train_union, proximity=TRUE) 
print(rf1)

#predicciones para datos de entrenamiento
p1_1 <- predict(rf1, train_union)
#matriz de confusión 
confusionMatrix(p1_1, train_union$Especie)

#definir especie de datos de validación como factor
test_union$Especie <- as.factor(test_union$Especie)
table(test_union$Especie)

#predicciones para datos de validación
p2_1 <- predict(rf1, test_union)

#matriz de confusión del segundo modelo
confusionMatrix(p2_1, test_union$Especie)
#matriz de confusión del primer modelo
confusionMatrix(p2, test$Especie)

#gráfica del segundo modelo
plot(rf1)

#búsqueda del número de árboles

t <- tuneRF(train_union[,-1], train_union[,1],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 150,
            trace = TRUE,
            improve = 0.05)

#tamaño de árboles
hist(treesize(rf1),
     main = "No. of Nodes for the Trees",
     col = "green")

#gráfica de importancia de las variables
varImpPlot(rf1,
           sort = T,
           n.var = 20,
           main = "Top 20 - Variable Importance")

#importancia de las variables
importance(rf1)

#gráfica de los grupos
MDSplot(rf1, train_union$Especie,k=3)


# curva ROC
library(ROCR)

prediction_for_roc_curve <- predict(rf1,
           test_union[,-1],type="prob")
# definir colores
pretty_colours <- c("black","red","green","gray","blue","orange",
                    "yellow","purple")
# especificidad 
classes <- levels(test_union$Especie)
# uso de For 
for (i in 1:8)
{
    true_values <- ifelse(test_union[,1]==classes[i],1,0)
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
    auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}

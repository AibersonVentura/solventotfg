# Librerías
library(dplyr)
library(FactoMineR)
library(knitr)
library(summarytools)
library(ROCR)
library(AUC)
library(car)
library(dplyr)
library(factoextra)
library(fpc)


setwd("E:/Universidad/Stats2021/tfg/datos")


dc<- openxlsx::read.xlsx("clientes3.xlsx", detectDates = TRUE, skipEmptyRows=T)


dp<- openxlsx::read.xlsx("polizas2.xlsx", detectDates = TRUE, skipEmptyRows=T)



#dp[,1]<- as.character(dp[,1])

dr<- openxlsx::read.xlsx("recibos3.xlsx", detectDates = TRUE, skipEmptyRows=T)


# UNIR BASES DE DATOS

names(dc)[1]<- "NºCliente"
names(dp)[1]<- "Cód..Poliza"


db<-inner_join(dc,dr,by="NºCliente") 

db<-inner_join(db,dp,by="Cód..Poliza") 

db<- as.data.frame(db)



 
# ELIMINAR VARIABLES CON +90% missing

db.na<-(colSums(apply(db,2,is.na))/dim(db)[1])>0.90  ## cutoff 90%

db<- db[,-which(db.na==TRUE)]

dim(db)


# DEPURACIÓN VARIABLES NUMÉRICAS

# Resumen de las variables numéricas

dbn<-select_if(db, is.numeric)
summary(dbn)

# Observar % Error de las variables

## Liquid.Cía  

100*sum(db$Liquid.Cía<0,na.rm=T)/dim(db)[1] #1.51% Error

#Pr.Anterior
100*sum(db$Pr.Anterior<0,na.rm=T)/dim(db)[1] #0.04%Error

#Com Anterior

100*sum(db$Com.Anterior<0,na.rm=T)/dim(db)[1]  #0.05% Error

#Pr Tot

100*sum(db$Pr.Tot<0,na.rm=T)/dim(db)[1]  # 0.9% Error

# Comisiones

100*sum(db$Comisiones<0,na.rm=T)/dim(db)[1] #0%

# Comisiones Brutas

100*sum(db$Com.Bruta<0,na.rm=T)/dim(db)[1] # 0.91 son errores

# Comisiones Cedidas

100*sum(db$C.Ced<0,na.rm=T)/dim(db)[1]  #0.49% son errores

#Comissiones Líquidas

100*sum(db$C.liq<0,na.rm=T)/dim(db)[1] #9.8% errores

#Pr.Cartera  
100*sum(db$PR..Cartera<0,na.rm=T)/dim(db)[1] #0% errores

# Pr.Media
100*sum(db$PR..Media<0,na.rm=T)/dim(db)[1] #0% errores

# Prima Anual

100*sum(db$Prima.anual<0,na.rm=T)/dim(db)[1] #0% errores

#PR.Neta

100*sum(db$Pr.Net<0,na.rm=T)/dim(db)[1] #0.92% Error

#Recargos

100*sum(db$Recargos<0,na.rm=T)/dim(db)[1] # 0.16%

##Desajuste 

100*sum(db$Desajuste!=0,na.rm=T)/dim(db)[1] # solo un 0.4347 de las veces no es 0.

# corregir variables


db<-db[db$Pr.Tot>0 & db$C.liq>0 & db$Com.Bruta>0,]
table(db$Tipo.Cliente)

bolt<- db$Pr.Net>0 & db$Recargos>=0 & db$Pr.Anterior>=0 & db$Com.Anterior>=0 & db$Liquid.Cía>0 & db$PR..Media>0

bolt[is.na(bolt)]<-TRUE
db<- db[ bolt,]



# Eliminar Variables no útiles

db$NºCía.x<-db$NºCía.y<-NULL 
db$Impuestos  <- NULL
db$C.Tal <- db$C.Adicional<-NULL
db$Var.Abs.<-db$`Var.Abs.(%Com)`<-db$`Var.Abs.(Com)`<-NULL
db$Var.Por.<-db$`Var.Por.(%Com)`<-db$`Var.Por.(Com)`<-NULL
db$`%Com.Actual`<- db$`%Com.Anterior`<-NULL
db$Uso.Auto<-NULL
db$P.Cartera<-NULL
db$Código<-NULL
db[,"Cod. Tomador"]<-NULL 
db$Cod.Pro<-NULL
db[,which(names(db)=="Núm.Siniestros")]<-NULL
db$Pr.Tot<-NULL
db$Matrícula<-NULL
db$Primas.Totales<-db$Primas.Netas<-NULL
db$HSP.cedido<-db$B.I..HSP<-db$Cuota.IVA.HSP<-NULL
db$H.S.P.<-db$`Cuota IVA HSP`<-NULL
db$Plazas.auto<-NULL
db$Importe.total<-NULL
db[,"Desajuste"]<-NULL
db$HSP<-NULL
db$Producto.x<-NULL
db$Ramo.Niv.1.x<-db$Ramo.Niv.2.x<-NULL
db[, c("Valor.Auto","Peso.Auto")]<-NULL
db$Cód..Poliza<-NULL


# DEPURACIÓN VARIABLES CATEGÓRICAS

#Depuración Variables Categóricas

# niveles de las categóricas
dbc<- select_if(db, is.character)
unc<- apply(dbc,2,function(x){length(unique(x))})
sort(unc, decreasing = T)


#Recategorización de las variables

## Tipo Cliente

###{Niveles diferente a Seguros generales} -> OTROS
# Esta variable no puede tener NAs porque será necesaria más adelante para la condición de la correduría

db<- db[!is.na(db$Tipo.Cliente),]

db$Tipo.Cliente[db$Tipo.Cliente!="SEGUROS GENERALES"]<-"OTROS"


## Motivo Baja

### {COMPETENCIA,DECISION TECNICA,DEFUNCION}->"OTROS"


bol<-  db$Motivo.Baja=="COMPETENCIA" |db$Motivo.Baja=="DECISION TECNICA" | db$Motivo.Baja=="DEFUNCION"

bol[is.na(bol)]<-F
db$Motivo.Baja[bol]<-"OTROS"


## Forma Pago

### {Niveles diferente a ANUAL}-> NO ANUAL
bol<-db$Forma.Pago!="ANUAL"

bol[is.na(bol)]<-F

db$Forma.Pago[bol]<-"NO ANUAL"




## Estado.y

### {Todas las categorias diferentes a anulados} ->"EN VIGOR"


db$Estado.y[db$Estado.y!="ANULADA"]<- "VIGOR"


## Provincia

### {GERONA,BARCELONA,LERIDA Y TARRAGONA}
### {CASTELLON, VALENCIA Y ALICANTE}
### {HUESCA, ZARAGOZA Y TERUEL}
### {Resto de niveles} -> OTROS

bol3<-db$Provincia=="GERONA"| db$Provincia=="BARCELONA"  | db$Provincia=="LERIDA"| db$Provincia=="TARRAGONA" 

bol3[is.na(bol3)]<-F
db$Provincia[bol3]<-"Cataluña"

bol4<- db$Provincia=="CASTELLON"| db$Provincia=="VALENCIA"| db$Provincia=="ALICANTE" 
bol4[is.na(bol4)]<-F
db$Provincia[bol4]<-"Valencia"

bol5<- db$Provincia=="HUESCA"| db$Provincia=="ZARAGOZA"| db$Provincia=="TERUEL" 
bol5[is.na(bol5)]<-F
db$Provincia[bol5]<-"Aragon"



bol6<-bol3+bol4+bol5

bol6[is.na(db$Provincia)]<-1
db$Provincia[bol6==0]<-"Otros"


## Abrev.Cía
### {Niveles diferentes a ZURICH,MAPFRE,ALLIANZ,HELVETIA,AXA,GENERALI Y PLUS ULTRA}-> OTRAS


bol7<-  db$Abrev.Cía.x=="MAPFRE" | db$Abrev.Cía.x=="ALLIANZ" | db$Abrev.Cía.x=="HELVETIA" | db$Abrev.Cía.x=="AXA" |db$Abrev.Cía.x=="PLUS ULTRA"  # 87.57% 

db$Abrev.Cía.x[ bol7==0]<-"OTRAS"


##Segmento
### {ORO,VIP,PLATA}-> ORO-PLATA-VIP

db$Segmento<-db$Segmento.y
bol<- db$Segmento=="ORO" |db$Segmento=="VIP" | db$Segmento=="PLATA" 
db$Segmento[bol]<-"ORO-PLATA-VIP"

table(db$Segmento)
# Eliminación de algunos niveles

##Est.Civ

### Eliminar todo los niveles diferentes a C,S y O


bol<- db$Est.Civ=="C"|db$Est.Civ=="O"| db$Est.Civ=="S"
bol[is.na(bol)]<-T

db<-db[bol,]


## Grupo cliente

### {Solo tener en cuenta a los niveles SIN GRUPO y SEGUROS}

bol<- db$Grupo.Cliente.x=="SEGUROS"|db$Grupo.Cliente.x=="SIN GRUPO"
bol[is.na(bol)]<-T
db<-db[bol,]


# Medio Cobro

bol<-db$Medio.Cobro=="OFICINA EFECTIVO"| db$Medio.Cobro=="COMPAÑIA EFECTIVO"

bol[is.na(bol)]<-F

db$Medio.Cobro[bol]<-"EFECTIVO"

bol<-db$Medio.Cobro=="COMPAÑIA BANCO"| db$Medio.Cobro=="BANCO" |db$Medio.Cobro=="BCO.TRANSFERENCIA" 
bol[is.na(bol)]<-F
db$Medio.Cobro[bol]<-"NO EFECTIVO"


# Eliminar Variable no útiles 

db$Irpf <- NULL
db$Tipo.x<-NULL
db$Segmento.y<-NULL
db$Of.Cobradora.x<-db$Of.Cobradora.y<-db$Of.Gestora<-db$Of.Productora.y<-NULL
db$Of.Gestora.x<-db$Of.Gestora.y<-db$Of.Productora<-db$Of.Productora.x<-NULL
db$Pago<-NULL
db$Tipo.y<-NULL
db$Pago<-NULL
db$Mot.Anulación<- db$Nombre.Correduría.x<-db$Nombre.Correduría.y<-NULL
db$Comentarios.x<- NULL
db$Tipo.x<-NULL
db$Segmento.y<-NULL
db$Pago<-NULL
db$Estado.x<- NULL
db$Plan.Com..y<-NULL
db$Duración.y<-NULL
db$Riesgo.Poliza<-db$Ramo.Niv.1.y<-db$Ramo.Niv.2.y<-NULL
db$Riesgo.y<-NULL 
db$Grupo.Cliente.y<-db$Compañía<-db$Nombre.Cía<-db$Abrev.Cía.y <-NULL
db$Nº.Orden<-db$Prov.Postal<-NULL
db$Segmento.x<-NULL
db$Producto.y<-NULL
db$Municipio.Principal<-db$Población.Principal<-db$Pob.Postal<-db$`Marca/Modelo`<-NULL
db$Comentarios.y<-NULL
db$Nat.Riesgo<-NULL
db$Modelo<-NULL
db$Riesgo.x<-db$Riesgo.y<-NULL
db$Sect..Laboral<-NULL
db$CP.Principal<-db$Cod..Tomador<-db$Prima.anual<-NULL
db$CP<-NULL
db[, c("Fe.Efecto.Car.","Prox.Vto.Rec.", "Marca","Pol.Reemplaz", "Pol..que.Ree",
       "P.Produccion", "Garantia1", "Garantia2", "Garantia3", "Garantia4","Garantia5",           
       "Garantia6", "Garantia7", "Garantia8", "Garantia9", "Garantia10","Cia.que.Ree","Cia.Reemplaz","F.Operacion" )]<-NULL



## Condición Correduría: 

# C.ced=C.liq cuando ceden el 100%
# C.liq superior a 9% y 5% (de la com.bruta) Para seguros generales y agrarios respectivamente

db$C.liq[db$C.Ced==db$Com.Bruta]<-db$C.Ced[db$C.Ced==db$Com.Bruta]

db[db$C.liq<0.09*db$Com.Bruta,] # En Estas 40 pólizas también ponemos C.liq=C.ced  + Todas son de Seguros generales

db$C.liq[db$C.liq<0.09*db$Com.Bruta ]<-db$C.Ced[db$C.liq<0.09*db$Com.Bruta ]


# Eliminación variable ya no son necesarias

db$Tipo.Cliente<-db$C.Ced<-NULL

#Eliminación de observaciones que no permiten crear alguna variable respuesta

db<-db[!is.na(db$Estado.y),]
db<-db[!is.na(db$C.liq),]


# CONSTRUCCIÓN DE VARIABLES RESPUESTA Y VARIABLES ACTIVAS CLUSTERING

#yc - 1:Ha cancelado alguna poliza, 0 no ha cancelado ninguna.

#Yimp C.LIQ medio del asegurado

m<-aggregate(db$C.liq, list(db$NºCliente), mean)

db$Estado.y<- as.numeric(db$Estado.y=="ANULADA")

my<-aggregate(db$Estado.y, list(db$NºCliente), mean) 

my2<-aggregate(db$Pr.Actual, list(db$NºCliente), sum) 
my3<-aggregate(db$Pr.Net, list(db$NºCliente), sum) 
my4<-aggregate(db$Com.Bruta, list(db$NºCliente), sum) 

db.d<-distinct(db, NºCliente,.keep_all = TRUE)

db.d<- arrange(db.d,NºCliente)

db.d$C.liq<-m[,2]

db.d$Estado.y[my$x!=0]<-1
db.d$Estado.y[my$x==0]<-0

db.d$Tot.Pr.Actual<-my2[,2]

db.d$Tot.Pr.Net<-my3[,2]

db.d$Tot.Com.Bruta<-my4[,2]

db.d$yc<- factor(db.d$Estado.y) 

db.d$yimp<- db.d$C.liq

row.names(db.d)<- db.d$NºCliente

# CONSTRUCCIÓN VARIABLE ANTIGÜEDAD

db.d$F.Baja[is.na(db.d$F.Baja)]<-"2021-02-20 UTC"

db.d$F.Baja <- as.Date(db.d$F.Baja, format='%d/%m/%y')

db.d$anti<-as.numeric(difftime(db.d$F.Baja,db.d$F.Alta, units = "weeks")/52.25) # Fbaja-Falta

db.d<-db.d[!is.na(db.d$anti), ] 

db.d<-db.d[db.d$anti>0, ] 



## agrupar reg.laboral y Cobro

bol<- db.d$Reg..Laboral!="REG.GENERAL"
bol[is.na(bol)]<-F

db.d$Reg..Laboral[bol]<-"OTROS"

db.d$Cobro[db.d$Cobro=="COMPAÑIA EFECTIVO"|db.d$Cobro=="OFICINA EFECTIVO"]<-"EFECTIVO"

db.d$Cobro[db.d$Cobro!="EFECTIVO"]<-"NO EFECTIVO"

table(db.d$Cobro)/sum(table(db.d$Cobro))

# ANÁLISIS DESCRIPTIVO UNIVARIANTE PARA LOS MODELOS LINEALES

# NUMÉRICAS

##yimp

summary(db.d$yimp)

sqrt(var(db.d$yimp, na.rm=T))

hist(db.d$yimp,col="royalblue",main="Histograma Impacto", xlab="Yimp")

boxplot(db.d$yimp,col="royalblue",main="Histograma Impacto", xlab="Yimp")

db.d<- db.d[db.d$yimp<1000,] # 10 valores outliers claros (>1000)

##Pr.media

summary(db.d$PR..Media)

sum(db.d$PR..Media>quantile(db.d$PR..Media, 0.75, na.rm=T) + 3*(quantile(db.d$PR..Media, 0.75, na.rm=T) -quantile(db.d$PR..Media, 0.25, na.rm=T)) , na.rm=T)

sum(db.d$PR..Media>1300, na.rm=T)/ length(db.d$PR..Media)

db.d<-db.d[db.d$PR..Media<30000,]   # 5 observaciones por encima de 30000

boxplot(db.d$PR..Media)

sqrt(var(db.d$PR..Media, na.rm=T))

hist(db.d$PR..Media,col="royalblue",main="Histograma Pr.Media", xlab="Pr.Media", ylab="Frecuencia")

##Num Pólizas
summary(db.d$Num.Póliza)
plot(db.d$Num.Póliza)

# Antigüedad


summary(db.d$anti)
boxplot(db.d$anti, col="royalblue",main="Antigüedad")
db.d<-db.d[db.d$anti<150,] ##♣ 1 obsevación error

# CUALITATIVAS
#yc
table(db.d$yc)
table(db.d$yc)/sum(table(db.d$yc))

#########

variable<- "Abrev.Cía.x"
avg(variable)

avg2(variable)

table(db.d[,variable], db.d$yc)/rowSums(table(db.d[,variable], db.d$yc))

#############


##Fis/Jur
table(db.d$`Fis/Jur`)

table(db.d$`Fis/Jur`)/sum(table(db.d$`Fis/Jur`))




# Est.Civ

table(db.d$Est.Civ)

table(db.d$Est.Civ)/sum(table(db.d$Est.Civ))




#Sexo

table(db.d$Sexo)

table(db.d$Sexo)/sum(table(db.d$Sexo))

## Añadir nivel J a Sexo

bol<-db.d$`Fis/Jur`=="J"
bol[is.na(bol)]<-F

db.d$Sexo[bol]<-"J"


table(db.d$Sexo)

table(db.d$Sexo)/sum(table(db.d$Sexo))




#Provincia
table(db.d$Provincia)
table(db.d$Provincia)/sum(table(db.d$Provincia))



#GRUPO CLIENTE
table(db.d$Grupo.Cliente.x)
table(db.d$Grupo.Cliente.x)/sum(table(db.d$Grupo.Cliente.x))





#PLAN COM
table(db.d$Plan.Com..x)


table(db.d$Plan.Com..x)/sum(table(db.d$Plan.Com..x))



#ABREV CIA
table(db.d$Abrev.Cía.x)
table(db.d$Abrev.Cía.x)/sum(table(db.d$Abrev.Cía.x))





# Medio Cobro 

table(db.d$Medio.Cobro)

table(db.d$Medio.Cobro)/sum(table(db.d$Medio.Cobro))



#FORMA PAGO

table(db.d$Forma.Pago)
table(db.d$Forma.Pago)/sum(table(db.d$Forma.Pago))



#OF COBRADORA
table(db.d$Of.Cobradora)
table(db.d$Of.Cobradora)/sum(table(db.d$Of.Cobradora))


#CLASE

table(db.d$Clase)
table(db.d$Clase)/sum(table(db.d$Clase))


variable<- "Clase"
avg(variable)

avg2(variable)

table(db.d[,variable], db.d$yc)/rowSums(table(db.d[,variable], db.d$yc))



# ANÁLISIS DESCRIPTIVO BIVARIANTE PARA LOS MODELOS LINEALES

# variables explicativas en función de Yc


names(db.d)[1]<-"ind"
dx2<- na.omit(db.d[, c("Provincia", "Grupo.Cliente.x","Plan.Com..x", "Abrev.Cía.x","Of.Cobradora","Num.Pólizas","yimp","Forma.Pago","Clase","Est.Civ","Cobro","ind","anti","Sexo","Reg..Laboral","Segmento")])



dx1<- na.omit(db.d[, c("Est.Civ","Sexo","Grupo.Cliente.x","Cobro","Provincia", "Plan.Com..x", "Abrev.Cía.x","Of.Cobradora","Num.Pólizas","PR..Media","yc","Forma.Pago","Clase","ind","anti","Reg..Laboral","Segmento")])
avg<-function(x){
  
  barplot(table(dx1$yc,dx1[,x]),cex.names = 0.7,col=c("royalblue","darkblue"), xlab="",ylab="Frecuencia", main=paste("Cancelar vs",x))
  legend("topright", legend=c("Cancelar=0", "Cancelar=1"),
         col=c("royalblue","darkblue"), pch=16, cex=1)  
  
  
}

avg2<-function(x){
  
  boxplot(dx2$yimp~dx2[,x],ylim=c(0,200),cex.axis=0.7,col="royalblue",ylab="Impacto",xlab=" ",main=paste("Impacto vs ",x), cex.names=0.5)
}

# Gráficos para memoria

par(mfrow=c(1,2))

for(i in c("Est.Civ","Grupo.Cliente.x", "Cobro","Provincia", "Plan.Com..x", "Abrev.Cía.x","Of.Cobradora","Forma.Pago","Clase", "Sexo")){
  

  avg(x = i)
  avg2(x = i)
  
}



###################

#Est.Civ

avg(x = "Est.Civ")

table(dx1$Est.Civ, dx1$yc)/rowSums(table(dx1$Est.Civ, dx1$yc))


#Sexo
avg(x = "Sexo")

table(dx1$Sexo, dx1$yc)/rowSums(table(dx1$Sexo, dx1$yc)) 

#Grupo Cliente

avg(x = "Grupo.Cliente.x")
table(dx1$Grupo.Cliente.x, dx1$yc)/rowSums(table(dx1$Grupo.Cliente.x, dx1$yc))  


#Cobro
avg(x = "Cobro")
table(dx1$Cobro, dx1$yc)/rowSums(table(dx1$Cobro, dx1$yc))  

#Pr.MEDIA

boxplot(dx1$PR..Media~factor(dx1$yc), col="royalblue",ylab="Pr Media",xlab="Yc",main="Yc vs Pr.Media")

#Plan Com

avg(x = "Plan.Com..x")

#Abrev cia

avg(x = "Abrev.Cía.x")

table(dx1$Abrev.Cía.x, dx1$yc)/rowSums(table(dx1$Abrev.Cía.x, dx1$yc))
#Of.Cobradora

avg(x = "Of.Cobradora")

#Num Pólizas

boxplot(dx1$Num.Pólizas~factor(dx1$yc), ylim=c(0,20) ,col="royalblue",ylab="Núm Polizas",xlab="Yc")

avg(x = "Num.Pólizas")

#Provincia

avg(x = "Provincia")
table(dx1$Provincia, dx1$yc)/rowSums(table(dx1$Provincia, dx1$yc))  

#Forma Pago
avg(x = "Forma.Pago")
table(dx1$Forma.Pago, dx1$yc)/rowSums(table(dx1$Forma.Pago, dx1$yc)) 

#Clase
avg("Clase")
table(dx1$Clase, dx1$yc)/rowSums(table(dx1$Clase, dx1$yc)) 

# Antigüedad

boxplot(dx1$anti~factor(dx1$yc), col="royalblue",ylab="Antigüedad",xlab="Cancelar",main="Cancelar vs Antigüedad")

# Racategorización de variables

## Variable Antigüedad por cuartiles

dx1$fanti<-cut(dx1$anti, c(0,4,9,14,32), include.lowest = F)


## Variable Número de Pólizas

### {4,5,.....70...} -> "+4"

dx1$f.numpol<-dx1$Num.Pólizas
bol<-dx1$f.numpol>=4
bol[is.na(bol)]<-F

dx1$f.numpol[bol]<-"+4"




# Distribución con variables recategorizadas

# Antigüedad

avg(x = "fanti")

table(dx1$fanti, dx1$yc)/rowSums(table(dx1$fanti, dx1$yc)) 



# Número de Pólizas como factor

avg(x = "f.numpol")

table(dx1$f.numpol, dx1$yc)/rowSums(table(dx1$f.numpol, dx1$yc))

 
# variables explicativas en función de Yimp


#Provincia

avg2(x = "Provincia") 

#Plan Com

avg2(x = "Plan.Com..x")

#Abrev Cia

avg2(x = "Abrev.Cía.x")

#Of.Cobradora

avg2(x = "Of.Cobradora")

#Num Pólizas

plot(dx2$yimp~dx2$Num.Pólizas,ylim=c(0,200),col="royalblue") 

table(dx2$Num.Pólizas)  

#Clase
avg2(x = "Clase")
#Forma de Pago
avg2(x = "Forma.Pago")

#Est.Civ

boxplot(db.d$yimp~db.d$Est.Civ,ylim=c(0,200),col="royalblue", main="Yimp vs Est.Civil", ylab="Yimp",xlab = "Est.Civ")

# Antigüedad
plot(dx2$yimp, dx2$anti)

# Recategorización de las variables 

# Viable Antigüedad por cuartiles
quantile(dx2$anti)

dx2$fanti<-cut(dx2$anti, c(0,4,9,14,32), include.lowest = F)



## Variable Num Pólizas



### {4,5,.....70} -> "+4"
###{2,3} -> "2-3" 

dx2$f.numpol<-dx2$Num.Pólizas
bol<-dx2$f.numpol>=4
bol[is.na(bol)]<-F

dx2$f.numpol[bol]<-"+4"

# Distribución de las variables recategorizadas

# Núm de Pólizas como factor

avg(x = "f.numpol")

plot(dx2$yimp, dx2$Num.Pólizas, col="royalblue", main="Imapcto vs Núm Pólizas", xlab="Impacto", ylab="Núm Pólizas")
boxplot(dx2$yimp~dx2[,"f.numpol"],ylim=c(0,200),col="royalblue",ylab="Impacto",xlab="Núm.Pólizas",main="Yimp vs Número de Pólizas")

# Antigüedad

avg2(x = "fanti")




# REGRESÓN LOGÍSTICA
par(mfrow=c(1,1))

# modelo con todas la variables 

lmodcT<-glm(yc~Est.Civ+Sexo+Provincia+Grupo.Cliente.x+Cobro+Plan.Com..x+`Abrev.Cía.x`+Num.Pólizas 
            +Of.Cobradora+PR..Media+Forma.Pago+Clase+anti+Reg..Laboral, data=dx1, maxit=150, family=binomial("logit")) 


residualPlot(lmodcT)


Anova(lmodcT,test.statistic="LR") 


#Pr.Media, Grupo.Cliente.x     

lmodcT2<-glm(yc~Est.Civ+Sexo+Provincia+Cobro+Plan.Com..x+`Abrev.Cía.x`+Num.Pólizas 
            +Of.Cobradora+Forma.Pago+Clase+anti+Reg..Laboral, data=dx1, maxit=150, family=binomial("logit")) 

Anova(lmodcT2,test.statistic="LR") 

# probar con  antigüedad factorizada

lmodc<-glm(yc~Est.Civ+Sexo+Provincia+Cobro+Plan.Com..x+`Abrev.Cía.x`+Num.Pólizas 
           +Of.Cobradora+Forma.Pago+Clase+Reg..Laboral+fanti, data=dx1, family=binomial) 

anova(lmodcT2,lmodc,test="Chisq") #mejor variable Factor


# probar con  Num Pólizas

lmodc2<-glm(yc~Est.Civ+Sexo+Provincia+Cobro+Plan.Com..x+`Abrev.Cía.x`
           +Of.Cobradora+Forma.Pago+Clase+Reg..Laboral+fanti+f.numpol, data=dx1, family=binomial) 

anova(lmodc2,lmodc,test="Chisq") # diferencia y devianza modelo numpol factor menor devianza-> mejor 



# Cambios en los niveles de las variables:

lmodc<- lmodc2
summary(lmodc)

## no se diferencian

# Cy o no diferenciable

dx1$Est.Civ[dx1$Est.Civ!="S"]<-"C o O"


lmodc<-glm(yc~Est.Civ+Sexo+Provincia+Cobro+Plan.Com..x+`Abrev.Cía.x`
            +Of.Cobradora+Forma.Pago+Clase+Reg..Laboral+fanti+f.numpol, data=dx1, family=binomial) 

summary(lmodc)

# Test anova entre modelo completo y simplificado

anova(lmodcT,lmodc,test="Chisq") # devianza modelo simple más peque y más grados libertad

# Percentil 95 de la chi cuadrado para el modelo simplificado

qchisq(0.95,lmodc$df.residual)   # devianza 9777.8 más peque que 13071.38 chi con df=12807

# AIC Y BIC

AIC(lmodcT2, lmodc)

BIC(lmodcT2, lmodc)


# Comprobación puntos influyentes y mulicolinealidad

plot(lmodc)  # Sin puntos influyente 

vif(lmodc)  # no multicolinealidad máx 2.09

round(exp(lmodc$coefficients),4)

## Evaluación capacidad predictiva del modelo (todos los datos)

db.d2<- na.omit(dx1)
dadesroc <- prediction(predict(lmodc,type="response"),db.d2$yc)
par(mfrow=c(1,2))
roc.perf <- performance(dadesroc,"auc",fpr.stop=0.05)
plot(performance(dadesroc,"err"),main="Tasa de error", col="royalblue", ylab="Tasa de error", xlab="Punto de corte")
plot(performance(dadesroc,"tpr","fpr"),main="Curva ROC",col="royalblue", ylab="Tasa de positivos reales",xlab="Tasa de falsos positivos")

pres.est <- ifelse(lmodc$fit<0.5,0,1)

# Matriz Confusión
(t <- table(pres.est,db.d2$yc))

#Área bajo la curva ROC

auc(roc(predict(lmodc,type="response"),factor(db.d2$yc)))  

# Cálculo de estadísticos interensantes

x<-c(Sensibilidad=t[2,2]/sum(t[,2]), Especificidad=t[1,1]/sum(t[,1]), ValorPredPositiu=t[2,2]/sum(t[2,])  , ValorPredNegatiu=t[1,1]/sum(t[1,]))

x

## Evaluación capacidad predictiva del modelo (Training and test)

set.seed(1)
sel<- sample(1:nrow(dx1),size=round(0.8*nrow(dx1)))

lmodctest<-glm(yc~Est.Civ+Sexo+Provincia+Cobro+Plan.Com..x+`Abrev.Cía.x`
               +Of.Cobradora+Forma.Pago+Clase+Reg..Laboral+fanti+f.numpol, data=dx1[sel,], family=binomial) 



p.test<-predict(lmodctest,type="response",dx1[-sel,])

dadesroc <- prediction(p.test,dx1[-sel,"yc"])
par(mfrow=c(1,2))
roc.perf <- performance(dadesroc,"auc",fpr.stop=0.05)
plot(performance(dadesroc,"err"),main="Tasa de error", col="royalblue", ylab="Tasa de error", xlab="Punto de corte")
plot(performance(dadesroc,"tpr","fpr"),main="Curva ROC",col="royalblue", ylab="Tasa de positivos reales",xlab="Tasa de falsos positivos")

## a partir del gráfico error rate parece que un buen cutoff es 0.55. - si y<0.55 - y=0 - no ha cancelado ninguna poliza.
pres.est <- ifelse(p.test<0.5,0,1)
(t <- table(pres.est,dx1[-sel,"yc"]))

auc(roc(p.test,factor(dx1[-sel,"yc"])))

x<-c(Sensibilidad=t[2,2]/sum(t[,2]), Especificidad=t[1,1]/sum(t[,1]), ValorPredPositiu=t[2,2]/sum(t[2,])  , ValorPredNegatiu=t[1,1]/sum(t[1,]))

x


# Exponencial de los coeficientes

round(exp(lmodc$coefficients),4)

# gráfico efecto coeficientes
par(mfrow=c(1,1))
coefs<-lmodc$coefficients

names(coefs)<- c("Intercept","Provincia-Otros", "Grupo.Cliente - SIN GRUPO", 
                 "Plan Com - SI", "Abrev.Cía - AXA", "Abrev.Cía - MAPFRE O HELVETIA", "Abrev.Cía - OTRAS", "Abrev.Cía - PLUS ULTRA",
                 "Num.Pólizas","Of.Cobradora - SEGRESEGUR CENTRAL", "Of.Cobradora - UNIO DE LLAURADORS", 
                 "Forma Pago - NO ANUAL","Clase - PRODUCCION", "Antigüedad")


colores<-c("yellow3","orange", 
           "violetred","red","red","red","red",
           "purple",
           "green", "green",
           "darkgreen","royalblue",
           "darkblue")


dotchart(coefs[-1],color=colores, cex=1, pch=16, main="Efecto de las variables" , xlab=expression(beta))
abline(v=0, col=1)



# REGRESIÓN GAMMA

#Modelo con todas las variables




lmodT<-glm(yimp~Provincia+Grupo.Cliente.x+Plan.Com..x+`Abrev.Cía.x`+Num.Pólizas + 
             Of.Cobradora+Forma.Pago+Clase+Est.Civ+Cobro+anti+Sexo,data=dx2, family=Gamma("log"), maxit=150)


plot(lmodT)

Anova(lmodT, test = "F")

# Eliminar Grupo.Cliente.x 

lmod<-glm(yimp~Provincia+Plan.Com..x+`Abrev.Cía.x`+ Num.Pólizas+
             Of.Cobradora+Forma.Pago+Clase+Est.Civ+Cobro+anti+Sexo,data=dx2, family=Gamma("log"), maxit=150)
Anova(lmod, test = "F")

# Modelo con Núm de Pólizas como factor 


lmod2<-glm(yimp~Provincia+Plan.Com..x+`Abrev.Cía.x`+ 
            Of.Cobradora+Forma.Pago+Clase+Est.Civ+Cobro+anti+Sexo+f.numpol,data=dx2, family=Gamma("log"), maxit=150)



anova(lmod,lmod2,test="F") # devianza menor con factor y significativa.


# Probar variable antigüedad categorizada

lmod3<-glm(yimp~Provincia+Plan.Com..x+`Abrev.Cía.x`+ 
            Of.Cobradora+Forma.Pago+Clase+Est.Civ+Cobro+fanti+Sexo+f.numpol,data=dx2, family=Gamma("log"), maxit=150)

# Test de devianza

anova(lmod2,lmod3, test="F") 

# Observar p-valores de los coeficientes
lmod<-lmod2
summary(lmod)

# Cambios en los niveles de las variables:

# F.num POL 3 == fnumpol +4

dx2$f.numpol[dx2$f.numpol=="3" | dx2$f.numpol=="+4"] <- "+3"

# SEXO Mno diferenciable de F 

dx2$Sexo[dx2$Sexo=="F" |dx2$Sexo=="M" ]<- "M o F"

#♣ EST.cIV 
dx2$Est.Civ[dx2$Est.Civ!="O"]<-"S o C"

#modelo con la recategorización hecha


lmod<-glm(yimp~Provincia+Plan.Com..x+`Abrev.Cía.x`+ 
            Of.Cobradora+Forma.Pago+Clase+Est.Civ+Cobro+anti+Sexo+f.numpol,data=dx2, family=Gamma("log"), maxit=150)


summary(lmod)

# Test de devianza modelo nuevo y el aditivo completo

deviance(lmod)-deviance(lmodT)
anova(lmod,lmodT, test="F")

# AIC Y BIC

AIC(lmod,lmodT)
BIC(lmod,lmodT)

# Ajuste?

cbind(Devianza=deviance(lmod),Chi2=qchisq(0.95, df.residual(lmod)))

# Pseudo-R^2

1-deviance(lmod)/lmod$null.deviance 

# Exponencial coeficientes

round(exp(lmod$coefficients),4)

summary(lmod)

# COMPROBACIÓN CAPACIDAD PREDICTIVA (Datos entrenamiento y test)

### 10-fold Cross validation

(EQM<-boot::cv.glm(dx2, lmod, K=10)$delta[1])
sqrt(EQM) 



# Modelización con los datos de entrenamiento

set.seed(1)
sel<- sample(1:nrow(dx2),size=round(0.8*nrow(dx2)))

lmodtest<-glm(yimp~Provincia+Plan.Com..x+`Abrev.Cía.x`+ 
                Of.Cobradora+Forma.Pago+Clase+Est.Civ+Cobro+anti+Sexo+f.numpol,data=dx2[sel,], family=Gamma("log"), maxit=150)

# Predicciones
p.test2<-predict(lmodtest,type="response",dx2[-sel,c("Provincia", "Plan.Com..x","Abrev.Cía.x", 
                                                       "Of.Cobradora","Forma.Pago","Clase","Est.Civ","Cobro","anti","Sexo","f.numpol")])


# Comparación distribuciones empíricas

plot(ecdf(dx2[-sel,"yimp"]), col="blue",pch=16, main="Distribuciones de probabilidad acumulada")
lines(ecdf(p.test2), col="darkblue")

legend("topright", legend=c("Mostral", "Predicciones"),
       col=c("royalblue","darkblue"),pch=15)





# CONTRUCCIÓN MAPAS DE RIESGO 

# Predicciones a nivel respuesta

dx1$pc<-100*predict(lmodc, type="response")

dx2$pimp<- predict(lmod, type="response")

# Contrucción mapas de riesgo en función de los niveles de los factores

x<-inner_join(dx1,dx2,by="ind") 

bol<-db.d$F.Baja=="2021-02-20"
bol[is.na(bol)]<-F
sel_noB<-data.frame(ind=db.d[bol,1])

x<- inner_join(x,sel_noB,by="ind") 

xr<-x[, c("yimp","pimp","yc","pc","ind")]

xe<- x[,c(1:8,12:13,17:18)]

xe$fanti.x<-as.character(xe$fanti.x)

## escala REAL
palet<-c("royalblue","darkblue","turquoise4","skyblue", "darkturquoise","aquamarine1","blue")
for (i in 1:(ncol(xe)-1)){ 
  
  k<- xr
  
  xr$q <- rep(0, length(xr$pimp))
  
  myimp<- mean(mean(xr$pimp))
  
  xr[xr$pc<50 & xr$pimp>myimp, "q"]<-1
  
  xr[xr$pc<50 & xr$pimp<myimp, "q"]<-3
  
  xr[xr$pc>50 & xr$pimp>myimp, "q"]<-2
  
  xr[xr$pc>50 & xr$pimp<myimp, "q"]<-4

  v<- unique(xe[,i])
  col<- xe[,i]
  for(j in 1:length(v)){
    
    col[xe[,i]==v[j]]<- palet[j]
  }
  
  cua<- factor(xr$q)
  ggplot(xr, aes(y = pimp, x = pc,shape = factor(cua), colour = col)) + 
    geom_point(aes(group = interaction(cua,col)))
  
  
  plot(xr$pc,xr$pimp, ylab="Percentil Impacto(Yimp)",xlab="Percentil Probabilidad Cancelación (Yc)", col=col,pch=16,main=names(xe)[i])
  
  legend("topleft",title="Niveles  Cuadrante", legend=c(v, paste("C",unique(xr$q))),
         col=unique(col), pch=xr$q, cex=0.8) 
}



plot(xr$pc,xr$pimp, col="blue", pch=19)



# Por Cuadrantes

myimp<- 30

xe$cuadrantes<- rep("C1", nrow(xe))

xe$cuadrantes[xr$pc>50 & xr$pimp>myimp]<- "C2"

xe$cuadrantes[xr$pc<50 & xr$pimp<myimp]<- "C3"

xe$cuadrantes[xr$pc>50 & xr$pimp<myimp]<- "C4"

v<- unique(xe$cuadrantes)

col<- xe$cuadrantes

for(j in 1:4){
  
  col[xe$cuadrantes==v[j]]<- palet[j]
}



plot(xr$pc,xr$pimp, ylab="Impacto",xlab="Probabilidad de Cancelación ", col=col,pch=16,main="Mapa de riesgo por cuadrantes")
legend("topleft", legend=v,col=unique(col), pch=16, cex=0.8) 



# CLUSTERING

# Datos Para Relizar el análisis

ve<-c("Tot.Pr.Actual","Tot.Com.Bruta","Tot.Pr.Net", "yimp","PR..Media")


dx3<- na.omit(db.d[,c("Est.Civ","Sexo","Grupo.Cliente.x","Segmento","Cobro",
                      "Provincia", "Plan.Com..x", "Abrev.Cía.x","Of.Cobradora","yc",
                      "Forma.Pago","Clase","anti","Num.Pólizas",ve )])

## Análsis descriptivo variables activas

apply(dx3[,ve],2,sd)

apply(dx3[,ve],2,summary)

boxplot(dx3[,"Tot.Pr.Actual"], main="Tot.Pr.Actual(Antes)", col="royalblue")

boxplot(dx3[,"Tot.Com.Bruta"], main="Tot.Com.Bruta(Antes)", col="royalblue")

boxplot(dx3[,"Tot.Pr.Net"], main="Tot.Pr.Net(Antes)", col="royalblue")

boxplot(dx3[,"yimp"], main="Yimp(Antes)", col="royalblue")

boxplot(dx3[,"PR..Media"], main="PR..Media(Antes)", col="royalblue")


# Calcular los el límite superior de las variables activas + Eliminar outliers


l1<-quantile(dx3[,"Tot.Pr.Actual"],probs=0.75)+3*IQR(dx3[,"Tot.Pr.Actual"])

sum(dx3[,"Tot.Pr.Actual"]>3185)

dx3<- dx3[dx3[,"Tot.Pr.Actual"]<round(l1), ] 


l2<-quantile(dx3[,"Tot.Com.Bruta"],probs=0.75)+3*IQR(dx3[,"Tot.Com.Bruta"])
sum(dx3[,"Tot.Com.Bruta"]>l2)

dx3<- dx3[dx3[,"Tot.Com.Bruta"]<round(l2), ]




l3<-quantile(dx3[,"Tot.Pr.Net"],probs=0.75)+3*IQR(dx3[,"Tot.Pr.Net"])
sum(dx3[,"Tot.Pr.Net"]>l3)

dx3<- dx3[dx3[,"Tot.Pr.Net"]<round(l3), ] 


l4<-quantile(dx3[,"yimp"],probs=0.75)+3*IQR(dx3[,"yimp"])

sum(dx3[,"yimp"]>l4)

dx3<- dx3[dx3[,"yimp"]<round(l4), ]  


l5<-quantile(dx3[,"PR..Media"],probs=0.75)+3*IQR(dx3[,"PR..Media"])
sum(dx3[,"PR..Media"]>l5)

dx3<- dx3[dx3[,"PR..Media"]<round(l5), ] 


# Análsis descriptivo variables activas (después de eliminar outliers)

apply(dx3[,ve],2,sd)

apply(dx3[,ve],2,summary)

boxplot(dx3[,"Tot.Pr.Actual"], main="Tot.Pr.Actual(Después)", col="royalblue")

boxplot(dx3[,"Tot.Com.Bruta"], main="Tot.Com.Bruta(Después)", col="royalblue")

boxplot(dx3[,"Tot.Pr.Net"], main="Tot.Pr.Net(Después)", col="royalblue")

boxplot(dx3[,"yimp"], main="Yimp(Después)", col="royalblue")

boxplot(dx3[,"PR..Media"], main="PR..Media(Después)", col="royalblue")


# Yc numércia y cálculo de correlaciones de las variables activas

dx3[,"yc"]<- as.numeric(dx3[,"yc"])-1

cor(dx3[,ve])


# Seleccionar numéro de componentes ACP

dx3.2<-dx3

pc1 <- prcomp(dx3.2[,ve], scale=TRUE)

eigcr_val<-pc1$sdev^2

par(mfrow=c(1,1))

plot(cumsum(eigcr_val/sum(eigcr_val)),type="b",xlab="Nºcomponent",ylab="Varianza explicada acumulada",main="Criterio PCA",col="royalblue")
abline(h=0.80)

# Escalar variables activas

dx3.2[,ve]<-scale(dx3.2[,ve]) 


names(dx3.2)
# PCA

res.pca <- PCA(dx3.2,quali.sup=c(1:9,11:12), quanti.sup = c(10,13,14), ncp = 2) 

Psi = res.pca$ind$coord

# K means con k=3
set.seed(123)
k3<-kmeans(Psi,3,nstart = 25,iter.max=150)

catdes(cbind(as.factor(k3$cluster),dx3.2),1)

# Número de observaciones por clúster y gráfico

table(k3$cluster) 

fviz_cluster(k3, geom = c("point") , data = dx3.2[,ve])+ ggtitle("k = 3")


# Cálculo estadísticos 

dd <- dist(dx3.2[,ve], method ="euclidean")

km_stats <- cluster.stats(dd,  k3$cluster)

km_stats$within.cluster.ss 

km_stats$clus.avg.silwidths

km_stats$dunn


# comparación con variable segmento

table(dx3$Segmento, k3$cluster)  # Oro-Plata-Vip=1, bronce=2, anulado=3

cseg<-dx3$Segmento
cseg[cseg=="ANULADO"]<-"green"
cseg[cseg=="BRONCE"]<-"red"
cseg[cseg=="ORO-PLATA-VIP"]<-"black"

plot(Psi,type="n",main="Variable Segmento", xlim=c(-2.5,10))
text(Psi, col=cseg,labels=names((k3$cluster)))
abline(h=0,v=0,col="gray")
legend("bottomright",unique(dx3$Segmento),pch=20,col=unique(cseg))


# Descriptiva de los cluesters por numéricas y categóricas

des<-catdes(cbind(as.factor(k3$cluster),dx3.2),1)

des

# Seleccionar una mejor K.

# scree Plot

ratio_ss <- rep(0, 10)

for (k in 1:10) {
  set.seed(123)
  school_km <- kmeans(Psi, k, nstart = 20)
  
  ratio_ss[k] <- school_km$tot.withinss / school_km$totss
  
}


plot(ratio_ss, type = "b", xlab = "k", col="royalblue", main="Scree Plot")


# Plot de los diferentes K means
par(mfrow=c(2,2))

fviz_cluster(kmeans(Psi,4,nstart = 25,iter.max=150), geom = c("point") , data = dx3.2[,ve])+ ggtitle(paste("k=",4))  

fviz_cluster(kmeans(Psi,5,nstart = 25,iter.max=150), geom = c("point") , data = dx3.2[,ve])+ ggtitle(paste("k=",5))

fviz_cluster(kmeans(Psi,6,nstart = 25,iter.max=150), geom = c("point") , data = dx3.2[,ve])+ ggtitle(paste("k=",6))

fviz_cluster(kmeans(Psi,7,nstart = 25,iter.max=150), geom = c("point") , data = dx3.2[,ve])+ ggtitle(paste("k=",7))




dx22<- na.omit(db.d[, c("Provincia", "Grupo.Cliente.x","Plan.Com..x", "Abrev.Cía.x","Of.Cobradora","Num.Pólizas","yimp","Forma.Pago","Clase","Est.Civ","Cobro","ind","anti","Sexo","Reg..Laboral")])



dx1<- na.omit(db.d[, c("Est.Civ","Sexo","Grupo.Cliente.x","Cobro","Provincia", "Plan.Com..x", "Abrev.Cía.x","Of.Cobradora","Num.Pólizas","PR..Media","yc","Forma.Pago","Clase","ind","anti","Reg..Laboral")])


save(dx1, file = "E:/Universidad/Stats2021/tfg/ShinyApp/dx1.RData")

save(dx2, file = "E:/Universidad/Stats2021/tfg/ShinyApp/dx2.RData")
save(db.d, file = "E:/Universidad/Stats2021/tfg/ShinyApp/dbd.RData")


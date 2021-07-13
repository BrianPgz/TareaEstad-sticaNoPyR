---
title: "Tarea_Est_1"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---
Pruebas de bondad de ajuste, problema 3. 






## Pruebas de bondad de ajuste
## Problema 3
La siguiente muestra aleatoria hace referencia a los rendimientos positivos de
cierta acción a lo largo del tiempo.

0.2513, 0.2566, 0.3459, 0.6379, 2.0505, 1.803, 2.1906,
1.5299, 0.35005, 0.3128, 1.2726, 2.3674, 2.3214, 2.4373, 0.6548

Llamamos a la libreria nortest para hacer la prueba lilliforce hasta el final.

```r
library(nortest)
```
Ho: La muestra sigue una distribución normal VS Ha: La muestra no sigue una distribución normal

Metemos los datos en un vector y ordenamos los datos.


```r
Dirty_data=c(0.2513, 0.2566, 0.3459, 0.6379, 2.0505, 1.803, 2.1906,
             1.5299, 0.35005, 0.3128, 1.2726, 2.3674, 2.3214, 2.4373, 0.6548)
Data=sort(Dirty_data)
```


```
##  [1] 0.25130 0.25660 0.31280 0.34590 0.35005 0.63790 0.65480 1.27260 1.52990
## [10] 1.80300 2.05050 2.19060 2.32140 2.36740 2.43730
```


Como queremos probar normalidad pero no sabemos los parámetros tenemos que estimarlos, primero queremos hacer una función para calcular la varianza muestral.



```r
S2<-function(x){
  mean_aux=mean(x)
  n_aux=length(x)
  s2=0
  for (i in x){
    s2=s2+(i-mean_aux)^2
  }
  s2=s2/(n_aux-1)
  return(s2)
}
```

Calculamos los estimadores máximos verosímiles. 


```r
mean_est=mean(Data)
S2_est=S2(Data)
n=length(Data)
```


```
## [1] 1.252137
```

```
## [1] 0.7810794
```

```
## [1] 15
```

Calculamos la función de distribución empírica necesaria para la prueba de lillieforce, donde "ecdf" es la función para crear la distribución empírica, "f_n" son los percentiles de la distribución empírica y "f_r" son los percentiles de la distribución pero desplazados una unidad. 



```r
f_emp=ecdf(Data)
f_n=f_emp(Data)
f_r=f_n-(1/n) 
```

Para para prueba de lillieforce necesitamos estandarizar los datos. 


```r
Z=(Data-mean_est)/sqrt(S2_est)
```


```
##  [1] -1.13244110 -1.12644418 -1.06285419 -1.02540173 -1.02070603 -0.69500536
##  [7] -0.67588310  0.02315415  0.31428766  0.62329879  0.90334365  1.06186602
## [13]  1.20986549  1.26191423  1.34100569
```

Ahora usaremos la función de distribución de una normal estandar para los valores antes calculados. 


```r
Probas=pnorm(Z)
```


```
##  [1] 0.1287245 0.1299888 0.1439240 0.1525868 0.1536969 0.2435260 0.2495574
##  [8] 0.5092363 0.6233487 0.7334559 0.8168282 0.8558517 0.8868347 0.8965102
## [15] 0.9100407
```


Tenemos que calcular: $$D_{i}^{+}= | \phi(Z_{i}) - \frac{i}{n}|$$  
$$D_{i}^{-}= | \phi(Z_{i}) - \frac{i-1}{n}|$$

```r
Di_p=abs(Probas-f_n)
Di_n=abs(Probas-f_r)
```

Visualizamos todos los cálculos. 


```r
Visual=data.frame(Dirty_data,Data,Z,Probas,Di_p,Di_n)
```


```
##    Dirty_data    Data           Z    Probas        Di_p       Di_n
## 1     0.25130 0.25130 -1.13244110 0.1287245 0.062057851 0.12872452
## 2     0.25660 0.25660 -1.12644418 0.1299888 0.003344555 0.06332211
## 3     0.34590 0.31280 -1.06285419 0.1439240 0.056075960 0.01059071
## 4     0.63790 0.34590 -1.02540173 0.1525868 0.114079830 0.04741316
## 5     2.05050 0.35005 -1.02070603 0.1536969 0.179636464 0.11296980
## 6     1.80300 0.63790 -0.69500536 0.2435260 0.156474028 0.08980736
## 7     2.19060 0.65480 -0.67588310 0.2495574 0.217109233 0.15044257
## 8     1.52990 1.27260  0.02315415 0.5092363 0.024096990 0.04256968
## 9     0.35005 1.52990  0.31428766 0.6233487 0.023348715 0.09001538
## 10    0.31280 1.80300  0.62329879 0.7334559 0.066789235 0.13345590
## 11    1.27260 2.05050  0.90334365 0.8168282 0.083494899 0.15016157
## 12    2.36740 2.19060  1.06186602 0.8558517 0.055851744 0.12251841
## 13    2.32140 2.32140  1.20986549 0.8868347 0.020168078 0.08683474
## 14    2.43730 2.36740  1.26191423 0.8965102 0.036823158 0.02984351
## 15    0.65480 2.43730  1.34100569 0.9100407 0.089959302 0.02329264
```

Definimos $$D^{+}= max\{D_{i}^{+}\}$$ $$D^{-}= max\{D_{i}^{-}\}$$ 
 

```r
D_p=max(Di_p)
D_n=max(Di_n)
```


```
## [1] 0.2171092
```

```
## [1] 0.1504426
```

Definimos $$D= max\{ D^{+}, D^{-} \}$$


```r
Dn=max(D_p,D_n)
```



```
## [1] 0.2171092
```


Declaramos nuestro nivel de significancia $$\alpha = 0.10$$ entonces $$1-\alpha = 0.90$$.



Comparamos con el nivel crítico $$ W_{0.10}^{15}= 0.2016 $$, para el tamaño de muestra 15 en tablas.


```r
Est=0.2016
Rechazamos_H0=Dn>Est
```


```
## [1] TRUE
```

Como $$D > W_{0.10}^{15}$$, rechazamos Ho y por lo tanto no hay evidencia para que los datos sigan una distribución $$ N(\mu,\sigma^2) $$

Hacemos la prueba en unas líneas. 


```r
p_value=lillie.test(Data)[[2]]

Rechazamos_p_value=p_value<alpha
```


```
## [1] 0.05540837
```

```
## [1] TRUE
```

Confirmando la prueba anterior. 


El gerente del banco asume que la muestra se distribuye sigue una distribución lognormal con media 0 y varianza 1. Realice la prueba correspondiente para verificar la suposición del gerente con un nivel de significancia alpha = 0.01. 

Llamamos a la función de distribución de una lognormal.


```r
ProbasB=plnorm(Data) 
```



```
##  [1] 0.0836229 0.0868775 0.1225789 0.1442074 0.1469328 0.3265089 0.3359924
##  [8] 0.5952465 0.6646554 0.7222209 0.7636471 0.7835314 0.8001537 0.8055991
## [15] 0.8135061
```

Calculamos $$ D_{i}^{+} $$ $$ D_{i}^{-} $$


```r
Di_pB=abs(ProbasB-f_n)
Di_nB=abs(ProbasB-f_r)
```

Ahora para $D^{+} \quad D^{-}$


```r
D_pB=max(Di_pB)
#Calculamos D-
D_nB=max(Di_nB)
```

Entonces obtenemos $$D$$

```r
DnB=max(D_pB,D_nB)
```


```
## [1] 0.1864939
```

Declaramos nuestro nivel de significancia $$\alpha = 0.01$$ entonces $$1-\alpha = 0.99$$. 



Tenemos que en tablas $$W_{0.01}^{15}=0.40420$$



```r
EstB=0.40420

Rechazamos_H0B=DnB>EstB
```


```
## [1] FALSE
```

Se cumple que $$W_{0.01}^{15} > D$$ entonces no hay evidencia para rechazar Ho, entonces 
los datos siguen una distribución lognormal con media de 1 y varianza 0.

Usamos la prueba específicando la distribución lognormal con sus parámetros.


```r
p_valueB=ks.test(Data,plnorm,0,1)[[2]]

Rechazamos_p_valueB=p_valueB<alphaB
```


```
## [1] 0.6089758
```

```
## [1] FALSE
```

Confirmamos la prueba anterior. 


## Pruebas de bondad de ajuste
## Problema 4


Un cierto banco otorga crédito a las personas con una tasa preferencial, de tal
manera que los acreditados pueden pagar en cualquier momento desde que piden
el prestamo hasta 8 semanas posteriores para que les sea respetada la tasa
preferencial . Se seleccionaron aleatoriamente a 1,000 personas y observaron su
comportamiento, generando de esta manera la siguiente tabla de frecuencia:


```
## data frame with 0 columns and 5 rows
```

Sea X v.a. que modela semanas completas que se tarda el cliente en hacer el pago. 

Enunciamos la prueba de hipótesis: 

Ho: Se sigue una distribución Bin(n=10,p=0.25)  VS   Ha: No se sigue una distribución Bin(n,p)

Nuestros parámetros son: 


```r
p=0.25
n=10
```

Nuestras observaciones fueron: 


```r
oi=c(64,195,287,241,140,51,25,4,1)
```














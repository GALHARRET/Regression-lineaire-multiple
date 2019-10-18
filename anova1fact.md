ANOVA et Régression linéaire
================
JM GALHARRET
10/15/2019

# Introduction

Charger la librairie suivante qui sera utile pour faire les comparaisons
post-hoc:

``` r
library(multcomp)
```

On va également voir comment on peut  définir des
contrastes dans un modèle linéaire et ainsi réaliser des comparaisons
planifiées en amont de l’analyse. Je ne vais pas ici redétailler les
définitions des modèles d’ANOVA ni les modèles linéaires.

Il existe un très bon livre sur ce lien entre ces modèles [ANOVA et
ANCOVA: a GLM
approach](https://www.wiley.com/en-us/ANOVA+and+ANCOVA%3A+A+GLM+Approach%2C+2nd+Edition-p-9780470385555)

Tous les exemples seront ici traités avec R, on peut également utiliser
[JAMOVI](https://www.jamovi.org/) ou
[JASP](https://jasp-stats.org/download/) qui sont des logiciels
pour débuter en statistique.

## Rappels sur les contrastes

On considère ![J](https://latex.codecogs.com/png.latex?J "J") groupes de
moyennes respectives
![m\_1,...,m\_J](https://latex.codecogs.com/png.latex?m_1%2C...%2Cm_J
"m_1,...,m_J"). On appelle contraste entre ces
![J](https://latex.codecogs.com/png.latex?J "J") groupes le nombre   
![\\psi=\\sum\_{j=1}^Ja\_jm\_j
](https://latex.codecogs.com/png.latex?%5Cpsi%3D%5Csum_%7Bj%3D1%7D%5EJa_jm_j%20
"\\psi=\\sum_{j=1}^Ja_jm_j ")  
où ![a\_1,...,a\_J](https://latex.codecogs.com/png.latex?a_1%2C...%2Ca_J
"a_1,...,a_J") sont des nombres réels tels que
![\\sum\_{j=1}^Ja\_j=0](https://latex.codecogs.com/png.latex?%5Csum_%7Bj%3D1%7D%5EJa_j%3D0
"\\sum_{j=1}^Ja_j=0"). Deux contrastes
![\\psi\_1](https://latex.codecogs.com/png.latex?%5Cpsi_1 "\\psi_1") et
![\\psi\_2](https://latex.codecogs.com/png.latex?%5Cpsi_2 "\\psi_2")
sont orthogonaux si de plus on a
![\\sum\_{j=1}^Ja\_jb\_j=0](https://latex.codecogs.com/png.latex?%5Csum_%7Bj%3D1%7D%5EJa_jb_j%3D0
"\\sum_{j=1}^Ja_jb_j=0") où
![a\_1,...,a\_J](https://latex.codecogs.com/png.latex?a_1%2C...%2Ca_J
"a_1,...,a_J") et
![b\_1,...,b\_J](https://latex.codecogs.com/png.latex?b_1%2C...%2Cb_J
"b_1,...,b_J") sont les coefficients respectifs des contrastes
![\\psi\_1](https://latex.codecogs.com/png.latex?%5Cpsi_1 "\\psi_1") et
![\\psi\_2](https://latex.codecogs.com/png.latex?%5Cpsi_2 "\\psi_2"). Un
système de contraste
![(\\psi\_1,...,\\psi\_{J-1})](https://latex.codecogs.com/png.latex?%28%5Cpsi_1%2C...%2C%5Cpsi_%7BJ-1%7D%29
"(\\psi_1,...,\\psi_{J-1})") est complet si il est constitué de
contrastes deux à deux orthogonaux. Pour tester l’hypothèse
![H\_0:\\psi=0](https://latex.codecogs.com/png.latex?H_0%3A%5Cpsi%3D0
"H_0:\\psi=0") on utilise la statistique   
![ t= \\frac{\\psi}{\\displaystyle \\sqrt{s^2\_R\\sum\_{j=1}^J\\left(
\\frac{a\_j^2}{n\_j}
\\right)}}](https://latex.codecogs.com/png.latex?%20t%3D%20%5Cfrac%7B%5Cpsi%7D%7B%5Cdisplaystyle%20%5Csqrt%7Bs%5E2_R%5Csum_%7Bj%3D1%7D%5EJ%5Cleft%28%20%5Cfrac%7Ba_j%5E2%7D%7Bn_j%7D%20%5Cright%29%7D%7D
" t= \\frac{\\psi}{\\displaystyle \\sqrt{s^2_R\\sum_{j=1}^J\\left( \\frac{a_j^2}{n_j} \\right)}}")  
qui sous ![H\_0](https://latex.codecogs.com/png.latex?H_0 "H_0") suit
une loi de Student ![\\mathcal
S\_{df\_R}](https://latex.codecogs.com/png.latex?%5Cmathcal%20S_%7Bdf_R%7D
"\\mathcal S_{df_R}")

# ANOVA à un facteur

Considérons une variable réponse
![Y](https://latex.codecogs.com/png.latex?Y "Y") et un facteur
![A](https://latex.codecogs.com/png.latex?A "A") ayant trois modalités
![a\_1,a\_2,a\_3,a\_4](https://latex.codecogs.com/png.latex?a_1%2Ca_2%2Ca_3%2Ca_4
"a_1,a_2,a_3,a_4"). On considère un échantillon de 140 valeurs de
![Y](https://latex.codecogs.com/png.latex?Y "Y"):

``` r
head(ex1)
```

    ##   X    Y  A
    ## 1 1  9.6 a1
    ## 2 2 10.0 a1
    ## 3 3  9.5 a1
    ## 4 4  8.8 a1
    ## 5 5 11.4 a1
    ## 6 6  9.2 a1

``` r
by(ex1$Y,ex1$A,mean)
```

    ## ex1$A: a1
    ## [1] 10.285
    ## -------------------------------------------------------- 
    ## ex1$A: a2
    ## [1] 9.853333
    ## -------------------------------------------------------- 
    ## ex1$A: a3
    ## [1] 14.704
    ## -------------------------------------------------------- 
    ## ex1$A: a4
    ## [1] 17.0575

La question qui se pose ici est de savoir si le facteur
![A](https://latex.codecogs.com/png.latex?A "A") influence la variable
réponse ![Y](https://latex.codecogs.com/png.latex?Y "Y") ou bien
autrement dit si il existe une différence entre les valeurs de
![Y](https://latex.codecogs.com/png.latex?Y "Y") selon la modalité de
![A](https://latex.codecogs.com/png.latex?A "A") considérée. On suppose
ici (et c’est le cas) que les échantillons permettent de considérer un
modèle d’ANOVA (c’est à dire distribution des résidus du modèle est
normale et qu’il y a homogénéité des variances). On n’insistera pas sur
ces conditions.

Le problème revient donc à tester
![H\_0:\\mu\_{a\_1}=\\mu\_{a\_2}=\\mu\_{a\_3}=\\mu\_{a\_4}](https://latex.codecogs.com/png.latex?H_0%3A%5Cmu_%7Ba_1%7D%3D%5Cmu_%7Ba_2%7D%3D%5Cmu_%7Ba_3%7D%3D%5Cmu_%7Ba_4%7D
"H_0:\\mu_{a_1}=\\mu_{a_2}=\\mu_{a_3}=\\mu_{a_4}").

## Le modèle d’ANOVA

``` r
anova<-aov(Y~A,data=ex1)
summary(anova)
```

    ##              Df Sum Sq Mean Sq F value Pr(>F)    
    ## A             3 1178.5   392.8   266.5 <2e-16 ***
    ## Residuals   136  200.5     1.5                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Il existe donc une différence significative entre les moyennes de
![Y](https://latex.codecogs.com/png.latex?Y "Y") selon la modalité de
![A](https://latex.codecogs.com/png.latex?A "A") (i.e. entre les groupes
![a\_1,a\_2,a\_3,a\_4](https://latex.codecogs.com/png.latex?a_1%2Ca_2%2Ca_3%2Ca_4
"a_1,a_2,a_3,a_4")). En général on fait ensuite une analyse post-hoc
pour voir quelles moyennes sont différentes des autres.

``` r
tukey<-glht(anova,linfct=mcp(A="Tukey"))
summary(tukey)
```

    ## 
    ##   Simultaneous Tests for General Linear Hypotheses
    ## 
    ## Multiple Comparisons of Means: Tukey Contrasts
    ## 
    ## 
    ## Fit: aov(formula = Y ~ A, data = ex1)
    ## 
    ## Linear Hypotheses:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## a2 - a1 == 0  -0.4317     0.3505  -1.232    0.605    
    ## a3 - a1 == 0   4.4190     0.3212  13.756   <1e-04 ***
    ## a4 - a1 == 0   6.7725     0.3325  20.367   <1e-04 ***
    ## a3 - a2 == 0   4.8507     0.2804  17.299   <1e-04 ***
    ## a4 - a2 == 0   7.2042     0.2933  24.566   <1e-04 ***
    ## a4 - a3 == 0   2.3535     0.2576   9.137   <1e-04 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## (Adjusted p values reported -- single-step method)

## Le modèle linéaire

On code les modalités du facteur (ceci se fait automatiquement sous R)
en
![C\_1=1\_{A=a\_1}](https://latex.codecogs.com/png.latex?C_1%3D1_%7BA%3Da_1%7D
"C_1=1_{A=a_1}"),
![C\_2=1\_{A=a\_2}](https://latex.codecogs.com/png.latex?C_2%3D1_%7BA%3Da_2%7D
"C_2=1_{A=a_2}"),
![C\_3=1\_{A=a\_3}](https://latex.codecogs.com/png.latex?C_3%3D1_%7BA%3Da_3%7D
"C_3=1_{A=a_3}") et
![C\_4=1\_{A=a\_4}](https://latex.codecogs.com/png.latex?C_4%3D1_%7BA%3Da_4%7D
"C_4=1_{A=a_4}"). Pour que le modèle soit identifiable, on choisit l’une
des modalités comme la modalité de référence, ici ce sera
![a\_1](https://latex.codecogs.com/png.latex?a_1 "a_1"). Le modèle est
alors   
![Y=b\_0+b\_2 1\_{A=a\_2}+b\_3 1\_{A=a\_3}+b\_4 1\_{A=a\_4}
+\\varepsilon.](https://latex.codecogs.com/png.latex?Y%3Db_0%2Bb_2%201_%7BA%3Da_2%7D%2Bb_3%201_%7BA%3Da_3%7D%2Bb_4%201_%7BA%3Da_4%7D%20%20%2B%5Cvarepsilon.
"Y=b_0+b_2 1_{A=a_2}+b_3 1_{A=a_3}+b_4 1_{A=a_4}  +\\varepsilon.")  
Dans ce cas ![b\_0](https://latex.codecogs.com/png.latex?b_0 "b_0") est
une estimation de la moyenne de
![Y](https://latex.codecogs.com/png.latex?Y "Y") sur le groupe
![A=a\_1](https://latex.codecogs.com/png.latex?A%3Da_1 "A=a_1"),
![b\_j](https://latex.codecogs.com/png.latex?b_j "b_j") est la
différence de moyennes entre le groupe
![A=a\_j](https://latex.codecogs.com/png.latex?A%3Da_j "A=a_j") et le
groupe de référence.

``` r
mod<-lm(Y~A,data=ex1)
summary(mod)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ A, data = ex1)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3040 -0.7898 -0.0713  0.6960  4.3960 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  10.2850     0.2715  37.882   <2e-16 ***
    ## Aa2          -0.4317     0.3505  -1.232     0.22    
    ## Aa3           4.4190     0.3212  13.756   <2e-16 ***
    ## Aa4           6.7725     0.3325  20.367   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.214 on 136 degrees of freedom
    ## Multiple R-squared:  0.8546, Adjusted R-squared:  0.8514 
    ## F-statistic: 266.5 on 3 and 136 DF,  p-value: < 2.2e-16

On remarque que les ![b\_j](https://latex.codecogs.com/png.latex?b_j
"b_j") et que les statistiques de test
![t](https://latex.codecogs.com/png.latex?t "t") coincident avec les
valeurs obtenues avec le test de Tukey. Par contre la
![p-](https://latex.codecogs.com/png.latex?p- "p-")value est différente
dans la mesure où pour le test de Tukey on a réalisé un ajustement pour
tenir compte des comparaisons multiples qui ont été réalisées.

# Les contrastes :

## Exemple 1 :

Admettons qu’a priori on veuille montrer que la moyenne de la modalité
![a\_3](https://latex.codecogs.com/png.latex?a_3 "a_3") est différente
des deux autres. Il s’agit donc de tester
![\\psi\_1=3\\mu\_{a\_4}-\\mu\_{a\_3}-\\mu\_{a\_2}-\\mu\_{a\_1}](https://latex.codecogs.com/png.latex?%5Cpsi_1%3D3%5Cmu_%7Ba_4%7D-%5Cmu_%7Ba_3%7D-%5Cmu_%7Ba_2%7D-%5Cmu_%7Ba_1%7D
"\\psi_1=3\\mu_{a_4}-\\mu_{a_3}-\\mu_{a_2}-\\mu_{a_1}"). On peut
réaliser le calcul “à la main” ou bien définir un système complet de
contrastes : il suffit pour cela d’ajouter au contraste précédent le
contraste
![\\psi\_2=2\\mu\_{a\_3}-\\mu\_{a\_1}-\\mu\_{a\_2}](https://latex.codecogs.com/png.latex?%5Cpsi_2%3D2%5Cmu_%7Ba_3%7D-%5Cmu_%7Ba_1%7D-%5Cmu_%7Ba_2%7D
"\\psi_2=2\\mu_{a_3}-\\mu_{a_1}-\\mu_{a_2}")
et![\\psi\_3=-\\mu\_{a\_1}+\\mu\_{a\_2}](https://latex.codecogs.com/png.latex?%5Cpsi_3%3D-%5Cmu_%7Ba_1%7D%2B%5Cmu_%7Ba_2%7D
"\\psi_3=-\\mu_{a_1}+\\mu_{a_2}") . Un contraste n’est pas unique (il
est défini à un coefficient multiplicatif près). Pour obtenir des
estimations des
![\\psi\_j](https://latex.codecogs.com/png.latex?%5Cpsi_j "\\psi_j")
définis précédememnt dans R il faut diviser chaque des coefficients
précédents par le nombre de comparaison effectué dans le contraste donc
on définit la matrice de contraste comp :

``` r
comp<-matrix(c(-1/4,-1/4,-1/4,3/4,
               -1/3,-1/3,2/3,0,
               -1/2,1/2,0,0),nrow=4)
mod2<-lm(Y~A,data=ex1,contrasts = list(A=comp))
summary(mod2)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ A, data = ex1, contrasts = list(A = comp))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3040 -0.7898 -0.0713  0.6960  4.3960 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  12.9750     0.1087 119.320   <2e-16 ***
    ## A1            5.4434     0.2319  23.472   <2e-16 ***
    ## A2            4.6348     0.2454  18.890   <2e-16 ***
    ## A3           -0.4317     0.3505  -1.232     0.22    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.214 on 136 degrees of freedom
    ## Multiple R-squared:  0.8546, Adjusted R-squared:  0.8514 
    ## F-statistic: 266.5 on 3 and 136 DF,  p-value: < 2.2e-16

La valeur Aj correspond à une estimation de
![\\psi\_j](https://latex.codecogs.com/png.latex?%5Cpsi_j "\\psi_j"). On
peut également définir “à la main” les trois contrastes précédentes en
ajoutant trois nouvelles variables au jeu de données la variable C1 dans
laquelle on met ![-1/4](https://latex.codecogs.com/png.latex?-1%2F4
"-1/4") lorsque
![A=a\_1,A=a\_2,A=a\_3](https://latex.codecogs.com/png.latex?A%3Da_1%2CA%3Da_2%2CA%3Da_3
"A=a_1,A=a_2,A=a_3") et
![+3/4](https://latex.codecogs.com/png.latex?%2B3%2F4 "+3/4") lorsque
![A=a\_4](https://latex.codecogs.com/png.latex?A%3Da_4 "A=a_4") et ainsi
de suite pour C2 et
C3.

``` r
C1<- -as.numeric(ex1$A=="a1")/4-as.numeric(ex1$A=="a2")/4-as.numeric(ex1$A=="a3")/4+3*as.numeric(ex1$A=="a4")/4
C2<- -as.numeric(ex1$A=="a2")/3-as.numeric(ex1$A=="a1")/3+2*as.numeric(ex1$A=="a3")/3
C3<- -as.numeric(ex1$A=="a2")/2+as.numeric(ex1$A=="a1")/2
mod3<-lm(Y~C1+C2+C3,data=ex1)
summary(mod3)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ C1 + C2 + C3, data = ex1)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3040 -0.7898 -0.0713  0.6960  4.3960 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  12.9750     0.1087 119.320   <2e-16 ***
    ## C1            5.4434     0.2319  23.472   <2e-16 ***
    ## C2            4.6348     0.2454  18.890   <2e-16 ***
    ## C3            0.4317     0.3505   1.232     0.22    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.214 on 136 degrees of freedom
    ## Multiple R-squared:  0.8546, Adjusted R-squared:  0.8514 
    ## F-statistic: 266.5 on 3 and 136 DF,  p-value: < 2.2e-16

``` r
mod3<-lm(Y~A,data=ex1)
summary(mod3)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ A, data = ex1)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3040 -0.7898 -0.0713  0.6960  4.3960 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  10.2850     0.2715  37.882   <2e-16 ***
    ## Aa2          -0.4317     0.3505  -1.232     0.22    
    ## Aa3           4.4190     0.3212  13.756   <2e-16 ***
    ## Aa4           6.7725     0.3325  20.367   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.214 on 136 degrees of freedom
    ## Multiple R-squared:  0.8546, Adjusted R-squared:  0.8514 
    ## F-statistic: 266.5 on 3 and 136 DF,  p-value: < 2.2e-16

Remarque: si on prend d’autres contrastes équivalents à

## Exemple 2:

On considère cette fois-ci que les groupes
![a\_1,a\_2](https://latex.codecogs.com/png.latex?a_1%2Ca_2 "a_1,a_2")
se ressemblent et que les groupes
![a\_3,a\_4](https://latex.codecogs.com/png.latex?a_3%2Ca_4 "a_3,a_4")
se ressemblent et qu’ils sont différents entre eux.

``` r
comp<-matrix(c(-1/4,-1/4,1/4,1/4,
               -1/2,1/2,0,0,
               0,0,1/2,-1/2),nrow=4)
mod4<-lm(Y~A,data=ex1,contrasts = list(A=comp))
summary(mod4)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ A, data = ex1, contrasts = list(A = comp))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3040 -0.7898 -0.0713  0.6960  4.3960 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  12.9750     0.1087 119.320  < 2e-16 ***
    ## A1           11.6232     0.4350  26.722  < 2e-16 ***
    ## A2           -0.4317     0.3505  -1.232     0.22    
    ## A3           -2.3535     0.2576  -9.137 8.01e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.214 on 136 degrees of freedom
    ## Multiple R-squared:  0.8546, Adjusted R-squared:  0.8514 
    ## F-statistic: 266.5 on 3 and 136 DF,  p-value: < 2.2e-16

Là encore on peut définir “à la main” les
contrastes

``` r
D1<-as.numeric(ex1$A=="a4")/4+as.numeric(ex1$A=="a3")/4-as.numeric(ex1$A=="a2")/4-as.numeric(ex1$A=="a1")/4
D2<--as.numeric(ex1$A=="a2")/2+as.numeric(ex1$A=="a1")/2
D3<--as.numeric(ex1$A=="a3")/2+as.numeric(ex1$A=="a4")/2
mod5<-lm(Y~D1+D2+D3,data=ex1)
summary(mod5)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ D1 + D2 + D3, data = ex1)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3040 -0.7898 -0.0713  0.6960  4.3960 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  12.9750     0.1087 119.320  < 2e-16 ***
    ## D1           11.6232     0.4350  26.722  < 2e-16 ***
    ## D2            0.4317     0.3505   1.232     0.22    
    ## D3            2.3535     0.2576   9.137 8.01e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.214 on 136 degrees of freedom
    ## Multiple R-squared:  0.8546, Adjusted R-squared:  0.8514 
    ## F-statistic: 266.5 on 3 and 136 DF,  p-value: < 2.2e-16

Toutes ces comparaisons sont évidemment faisables avec JASP ou JAMOVI.
Il suffit de définir les colonnes C1,C2,C3 et D1,D2,D3 dans la base de
données (directement dans le logiciel ou sur le .csv).

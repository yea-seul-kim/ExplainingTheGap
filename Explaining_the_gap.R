#main study (the voting result dataset)
#'visual': visual conditions, 1: text conditions
#Analyzing absolute errors for visual conditions

> full.model <- lmer(gap ~  visual + explain + predict.explain+ predict.feedback + predictOnly + spatial 
                   + I(visual*spatial)+I(visual*predict.explain) + I(visual*explain) 
                   + I(visual*predictOnly)  + (1|id)+ (1|race), data=absolute)

> summary(full.model.absolute)
#################################################################################
Linear mixed model fit by REML ['lmerMod']
Formula: gap ~ visual + explain + predict.explain + predict.feedback +  
    predictOnly + spatial + I(visual * spatial) + I(visual *  
    predict.explain) + I(visual * explain) + I(visual *  
    predictOnly) + (1 | id) + (1 | race)
   Data: absolute

REML criterion at convergence: 32364.2

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-3.2822 -0.6138 -0.2137  0.4147  6.8504 

Random effects:
 Groups   Name        Variance Std.Dev.
 id       (Intercept) 15.537   3.942   
 race     (Intercept)  9.702   3.115   
 Residual             72.853   8.535   
Number of obs: 4476, groups:  id, 373; race, 3

Fixed effects:
                                Estimate Std. Error t value
(Intercept)                      14.1435     2.0110   7.033
visual                           -3.7651     1.3701  -2.748
explain                          -3.5248     1.0167  -3.467
predict.explain                  -4.0831     1.0146  -4.025
predict.feedback                 -3.5386     1.0144  -3.488
predictOnly                      -2.2979     1.0233  -2.246
spatial                          -0.4393     0.1209  -3.635
I(visual * spatial)               0.1301     0.1938   0.671
I(visual * predict.explain)       2.3369     1.4332   1.631
I(visual * explain)               1.2286     1.4571   0.843
I(visual * predictOnly)           1.7092     1.4506   1.178

Correlation of Fixed Effects:
            (Intr) tstVsl explan prdct.x prdct.f prdctO spatil I(V*s) I(V*p. I(V*e)
visual      -0.294                                                                 
explain     -0.233  0.342                                                          
predct.xpln -0.258  0.378  0.497                                                   
prdct.fdbck -0.251  0.368  0.499  0.500                                            
predictOnly -0.230  0.338  0.499  0.494   0.496                                    
spatial     -0.270  0.397 -0.068  0.020  -0.006  -0.073                            
I(tstV*spt)  0.169 -0.672  0.042 -0.012   0.004   0.046 -0.623                     
I(tstV*pr.)  0.182 -0.563 -0.352 -0.708  -0.354  -0.350 -0.014  0.057              
I(tstV*exp)  0.163 -0.551 -0.698 -0.347  -0.348  -0.349  0.047  0.048  0.498       
I(tstV*prO)  0.162 -0.516 -0.352 -0.349  -0.350  -0.705  0.052 -0.005  0.496  0.493
#################################################################################

#calculate p-value for fixed effects. 

> coefs <- data.frame(coef(summary(full.model.absolute)))
> coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
                                  Estimate Std..Error    t.value          p.z
(Intercept)                     14.1434733  2.0109899  7.0330900 2.020162e-12
visual                          -3.7651438  1.3701308 -2.7480178 5.995675e-03
explain                         -3.5247706  1.0167138 -3.4668269 5.266410e-04
predict.explain                 -4.0831357  1.0145656 -4.0245161 5.709259e-05
predict.feedback                -3.5386052  1.0143820 -3.4884346 4.858577e-04
predictOnly                     -2.2979210  1.0232964 -2.2456065 2.472922e-02
spatial                         -0.4392904  0.1208535 -3.6348990 2.780896e-04
I(visual * spatial)              0.1300759  0.1938416  0.6710424 5.021935e-01
I(visual * predict.explain)      2.3369120  1.4331859  1.6305715 1.029808e-01
I(visual * explain)              1.2285528  1.4571250  0.8431348 3.991531e-01
I(visual * predictOnly)          1.7092476  1.4506186  1.1782887 2.386815e-01

#################################################################################

#calculate confidence interval
> confint(full.model.absolute)
                                     
                                     2.5 %     97.5 %
(Intercept)                        9.8172273 18.4697216
  visual                          -6.4209726 -1.1093150
  explain                         -5.4955443 -1.5539969
  predict.explain                 -6.0497455 -2.1165259
  predict.feedback                -5.5048591 -1.5723514
  predictOnly                     -4.2814544 -0.3143877
  spatial                         -0.6735499 -0.2050308
I(visual * spatial)               -0.2456620  0.5058138
I(visual * predict.explain)       -0.4411413  5.1149653
I(visual * explain)               -1.5959036  4.0530091
I(visual * predictOnly)           -1.1025970  4.5210921

#################################################################################



#Analyzing absolute errors for text conditions

#################################################################################
#Explain-Only-Text condition vs. None-Text

> summary(glht(full.model.absolute, linfct = c( "explain + visual:explain =0")))
> confint(glht(full.model.absolute, linfct = c( "explain + visual:explain =0")))

	 Simultaneous Tests for General Linear Hypotheses
Linear Hypotheses:
                                  Estimate Std. Error z value Pr(>|z|)  
explain + visual:explain == 0   -2.296      1.044    -2.2   0.0278 *

                                  Estimate lwr     upr    
explain + visual:explain == 0 -2.2962  -4.3420 -0.2504

#################################################################################

#Predict-Explain-Text condition vs. None-Text
> summary(glht(full.model.absolute, linfct = c( "predict.explain + visual:predict.explain =0")))
> confint(glht(full.model.absolute, linfct = c( "predict.explain + visual:predict.explain =0")))

	 Simultaneous Tests for General Linear Hypotheses

Linear Hypotheses:
                                                  Estimate Std. Error z value Pr(>|z|)  
predict.explain + visual:predict.explain == 0   -1.746      1.012  -1.725   0.0845 .

                                                  Estimate lwr     upr    
predict.explain + visual:predict.explain == 0 -1.7462  -3.7302  0.2378

#################################################################################

#Predict-Only-Text condition vs. None-Text
> summary(glht(full.model.absolute, linfct = c( "predictOnly + visual:predictOnly =0")))
> confint(glht(full.model.absolute, linfct = c( "predictOnly + visual:predictOnly =0")))

	 Simultaneous Tests for General Linear Hypotheses

Linear Hypotheses:
                                          Estimate Std. Error z value Pr(>|z|)
predictOnly + visual:predictOnly == 0  -0.5887     1.0282  -0.573    0.567
(Adjusted p values reported -- single-step method)
 
                                          Estimate lwr     upr    
predictOnly + visual:predictOnly == 0 -0.5887  -2.6039  1.4265

#################################################################################

#The effect of spatial ability in text conditions

> summary(glht(full.model.absolute, linfct = c( "spatial + visual:spatial =0")))
> confint(glht(full.model.absolute, linfct = c( "spatial + visual:spatial =0")))

	 Simultaneous Tests for General Linear Hypotheses


Linear Hypotheses:
                                  Estimate Std. Error z value Pr(>|z|)  
spatial + visual:spatial == 0  -0.3092     0.1516   -2.04   0.0413 *

                                  Estimate lwr      upr     
spatial + visual:spatial == 0 -0.30921 -0.60626 -0.01217

#################################################################################


#Analyzing relative errors for visual conditions

> full.model.relative <- lmer(gap ~  visual + explain + predict.explain+ predict.feedback + predictOnly + spatial 
                            + I(visual*spatial)+I(visual*predict.explain) + I(visual*explain) 
                            + I(visual*predictOnly)  + (1|id)+ (1|race), data=relative)

#################################################################################
> summary(full.model.absolute)
Linear mixed model fit by REML ['lmerMod']
Formula: gap ~ visual + explain + predict.explain + predict.feedback +  
    predictOnly + spatial + I(visual * spatial) + I(visual *  
    predict.explain) + I(visual * explain) + I(visual *  
    predictOnly) + (1 | id) + (1 | race)
   Data: relative

REML criterion at convergence: 16094.9

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.6444 -0.5105 -0.1755  0.4461  6.2585 

Random effects:
 Groups   Name        Variance Std.Dev.
 id       (Intercept)  9.093   3.016   
 race     (Intercept) 27.453   5.240   
 Residual             70.894   8.420   
Number of obs: 2238, groups:  id, 373; race, 3

Fixed effects:
                                Estimate Std. Error t value
(Intercept)                     11.49818    3.15198   3.648
visual                          -0.66147    1.34779  -0.491
explain                          0.11145    1.00014   0.111
predict.explain                 -2.06210    0.99802  -2.066
predict.feedback                -2.79178    0.99784  -2.798
predictOnly                     -0.74822    1.00661  -0.743
spatial                         -0.06309    0.11888  -0.531
I(visual * spatial)             -0.13088    0.19068  -0.686
I(visual * predict.explain)      0.74273    1.40982   0.527
I(visual * explain)             -0.05828    1.43337  -0.041
I(visual * predictOnly)          0.61679    1.42697   0.432

Correlation of Fixed Effects:
            (Intr) tstVsl explan prdct.x prdct.f prdctO spatil I(V*s) I(V*p. I(V*e)
visual      -0.185                                                                 
explain     -0.146  0.342                                                          
predct.xpln -0.162  0.378  0.497                                                   
prdct.fdbck -0.157  0.368  0.499  0.500                                            
predictOnly -0.144  0.338  0.499  0.494   0.496                                    
spatial     -0.170  0.397 -0.068  0.020  -0.006  -0.073                            
I(tstV*spt)  0.106 -0.672  0.042 -0.012   0.004   0.046 -0.623                     
I(tstV*pr.)  0.114 -0.563 -0.352 -0.708  -0.354  -0.350 -0.014  0.057              
I(tstV*exp)  0.102 -0.551 -0.698 -0.347  -0.348  -0.349  0.047  0.048  0.498       
I(tstV*prO)  0.102 -0.516 -0.352 -0.349  -0.350  -0.705  0.052 -0.005  0.496  0.493

#################################################################################

#calculate p values for fixed effects

> coefs <- data.frame(coef(summary(full.model.relative)))
> coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
                              Estimate Std..Error     t.value          p.z
(Intercept)                11.49818471  2.5745762  4.46604946 7.967720e-06
visual                     -0.66147399  1.3477932 -0.49078301 6.235799e-01
explain                     0.11144749  1.0001380  0.11143211 9.112737e-01
predict.explain            -2.06210208  0.9980249 -2.06618298 3.881121e-02
predictOnly                -0.74821837  1.0066133 -0.74330267 4.572985e-01
spatial                    -0.06308867  0.1188832 -0.53067765 5.956422e-01
predict.feedback           -2.79178036  0.9978443 -2.79781170 5.145010e-03
visual:explain             -0.05828041  1.4333691 -0.04065974 9.675672e-01
visual:predict.explain      0.74273359  1.4098203  0.52682857 5.983126e-01
visual:predictOnly          0.61679302  1.4269688  0.43224002 6.655670e-01
visual:spatial             -0.13088472  0.1906813 -0.68640548 4.924574e-01

#################################################################################

#calculate confidence interval
> confint(full.model.relative)
                                     2.5 %     97.5 %
(Intercept)                      4.4724997 18.5239093
visual                          -3.2740631  1.9511151
explain                         -1.8272399  2.0501349
predict.explain                 -3.9966934 -0.1275108
predict.feedback                -4.7260215 -0.8575392
predictOnly                     -2.6994576  1.2030209
spatial                         -0.2935343  0.1673569
I(visual * spatial)             -0.5005052  0.2387358
I(visual * predict.explain)     -1.9900900  3.4755571
I(visual * explain)             -2.8367515  2.7201907
I(visual * predictOnly)         -2.1492716  3.3828577

#################################################################################

#Analyzing relative errors for text conditions
#Explain-Only-Text condition vs. None-Text
> summary(glht(full.model.relative, linfct = c( "explain + visual:explain =0")))
> confint(glht(full.model.relative, linfct = c( "explain + visual:explain =0")))

	 Simultaneous Tests for General Linear Hypotheses

                                  Estimate Std. Error z value Pr(>|z|)
explain + visual:explain == 0  0.05317    1.02678   0.052    0.959
(Adjusted p values reported -- single-step method)

                                  Estimate lwr      upr     
explain + visual:explain == 0  0.05317 -1.95928  2.06561


#################################################################################
#Predict-Explain-Text condition vs. None-Text
> summary(glht(full.model.relative, linfct = c( "predict.explain + visual:predict.explain =0")))
> confint(glht(full.model.relative, linfct = c( "predict.explain + visual:predict.explain =0")))

	 Simultaneous Tests for General Linear Hypotheses

                                                  Estimate Std. Error z value Pr(>|z|)
predict.explain + visual:predict.explain == 0  -1.3194     0.9958  -1.325    0.185
(Adjusted p values reported -- single-step method)

                                                  Estimate lwr     upr    
predict.explain + visual:predict.explain == 0 -1.3194  -3.2710  0.6323



#################################################################################
#Predict-Only-Text condition vs. None-Text
> summary(glht(full.model.relative, linfct = c( "predictOnly + visual:predictOnly =0")))
> confint(glht(full.model.relative, linfct = c( "predictOnly + visual:predictOnly =0")))

	 Simultaneous Tests for General Linear Hypotheses

                                          Estimate Std. Error z value Pr(>|z|)
predictOnly + visual:predictOnly == 0  -0.1314     1.0114   -0.13    0.897
(Adjusted p values reported -- single-step method)

                                          Estimate lwr     upr    
predictOnly + visual:predictOnly == 0 -0.1314  -2.1138  1.8509


#################################################################################
#The effect of spatial ability in text conditions

> summary(glht(full.model.relative, linfct = c( "spatial + visual:spatial =0")))
> confint(glht(full.model.relative, linfct = c( "spatial + visual:spatial =0")))

	 Simultaneous Tests for General Linear Hypotheses

                                  Estimate Std. Error z value Pr(>|z|)
spatial + visual:spatial == 0  -0.1940     0.1491  -1.301    0.193
(Adjusted p values reported -- single-step method)

                                  Estimate lwr      upr     
spatial + visual:spatial == 0 -0.19397 -0.48617  0.09823


#################################################################################

#replication study (a scientific experiment result dataset)

#################################################################################
#Analyzing absolute error

full.model.absolute <- lmer(gap ~  explain + predict.explain+ predict.feedback + predictOnly + spatial+ (1|id)+ (1|race), data=absolute)

# The size of fixed effects 
> summary(full.model.absolute)
Linear mixed model fit by REML ['lmerMod']
Formula: gap ~ explain + predict.explain + predict.feedback + predictOnly +  
    spatial + (1 | id) + (1 | race)
   Data: absolute

REML criterion at convergence: 14983.9

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.3441 -0.7215 -0.2248  0.5954  4.4621 

Random effects:
 Groups   Name        Variance Std.Dev.
 id       (Intercept)  3.045   1.745   
 race     (Intercept)  0.000   0.000   
 Residual             21.184   4.603   
Number of obs: 2508, groups:  id, 209; race, 3

Fixed effects:
                 Estimate Std. Error t value
(Intercept)       7.92197    0.44344  17.865
explain          -1.78476    0.47640  -3.746
predict.explain  -1.27189    0.48467  -2.624
predict.feedback -1.30572    0.48491  -2.693
predictOnly      -0.69135    0.48209  -1.434
spatial          -0.23660    0.06109  -3.873

Correlation of Fixed Effects:
            (Intr) explan prdct.x prdct.f prdctO
explain     -0.579                              
predct.xpln -0.524  0.507                       
prdct.fdbck -0.516  0.506  0.501                
predictOnly -0.517  0.509  0.504   0.504        
spatial     -0.635  0.037 -0.034  -0.046  -0.049

> coefs <- data.frame(coef(summary(full.model.absolute)))
> coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))

                   Estimate Std..Error   t.value          p.z
(Intercept)       7.9219663 0.44343592 17.864963 0.0000000000
explain          -1.7847568 0.47639839 -3.746354 0.0001794237
predict.explain  -1.2718880 0.48467078 -2.624231 0.0086844924
predict.feedback -1.3057157 0.48490895 -2.692703 0.0070875427
predictOnly      -0.6913503 0.48209151 -1.434064 0.1515538737
spatial          -0.2365994 0.06109488 -3.872655 0.0001076563

> confint(full.model.absolute)
                      2.5 %     97.5 %
(Intercept)       7.0612127  8.7827199
explain          -2.7092245 -0.8602891
predict.explain  -2.2124086 -0.3313674
predict.feedback -2.2466985 -0.3647329
predictOnly      -1.6268658  0.2441652
spatial          -0.3551561 -0.1180426

#################################################################################
#Analyzing relative error

full.model.relative <- lmer(gap ~  explain + predict.explain+ predict.feedback + predictOnly + spatial+ (1|id)+ (1|race), data=relative)

Linear mixed model fit by REML ['lmerMod']
Formula: gap ~ explain + predict.explain + predict.feedback + predictOnly +  
    spatial + (1 | id) + (1 | race)
   Data: relative

REML criterion at convergence: 7991

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.1199 -0.7137 -0.1125  0.6352  4.6325 

Random effects:
 Groups   Name        Variance Std.Dev.
 id       (Intercept)  8.2041  2.8643  
 race     (Intercept)  0.1971  0.4439  
 Residual             29.1636  5.4003  
Number of obs: 1254, groups:  id, 209; race, 3

Fixed effects:
                 Estimate Std. Error t value
(Intercept)       10.7245     0.7744  13.848
explain           -1.5200     0.7851  -1.936
predict.explain   -1.2500     0.7988  -1.565
predict.feedback  -1.1561     0.7992  -1.447
predictOnly       -0.4528     0.7945  -0.570
spatial           -0.2956     0.1007  -2.936

Correlation of Fixed Effects:
            (Intr) explan prdct.x prdct.f prdctO
explain     -0.546                              
predct.xpln -0.495  0.507                       
prdct.fdbck -0.487  0.506  0.501                
predictOnly -0.488  0.509  0.504   0.504        
spatial     -0.599  0.037 -0.034  -0.046  -0.049
> coefs <- data.frame(coef(summary(full.model.relative)))
> coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
> coefs
                   Estimate Std..Error    t.value         p.z
(Intercept)      10.7244810  0.7744473 13.8479158 0.000000000
explain          -1.5199507  0.7851349 -1.9359104 0.052878679
predict.explain  -1.2499582  0.7987683 -1.5648570 0.117616444
predict.feedback -1.1560775  0.7991608 -1.4466143 0.148004984
predictOnly      -0.4527776  0.7945175 -0.5698775 0.568760808
spatial          -0.2956105  0.1006883 -2.9358987 0.003325829
> confint(full.model.relative)
Computing profile confidence intervals ...
                      2.5 %       97.5 %
(Intercept)       9.2358081 12.213157467
explain          -3.0450735  0.005172011
predict.explain  -2.8015638  0.301647496
predict.feedback -2.7084456  0.396290673
predictOnly      -1.9961261  1.090570868
spatial          -0.4911972 -0.100023805

#################################################################################

#replication study (fast food calorie content dataset)
#Analyzing absolute error

full.model.absolute <- lmer(gap ~  explain + predict.explain+ predict.feedback + predictOnly + spatial+ (1|id)+ (1|race), data=absolute)

> summary(full.model.absolute)
Linear mixed model fit by REML ['lmerMod']
Formula: gap ~ explain + predict.explain + predict.feedback + predictOnly +  
    spatial + (1 | id) + (1 | race)
   Data: absolute

REML criterion at convergence: 33019.4

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-3.0602 -0.5736 -0.2110  0.3513  6.9803 

Random effects:
 Groups   Name        Variance Std.Dev.
 id       (Intercept) 1997.1   44.69   
 race     (Intercept)  219.3   14.81   
 Residual             8299.3   91.10   
Number of obs: 2760, groups:  id, 230; race, 3

Fixed effects:
                 Estimate Std. Error t value
(Intercept)       109.612     12.443   8.809
explain           -12.087     10.783  -1.121
predict.explain    -2.299     10.892  -0.211
predict.feedback   -5.603     10.799  -0.519
predictOnly       -13.736     10.587  -1.297
spatial            -2.711      1.309  -2.071

Correlation of Fixed Effects:
            (Intr) explan prdct.x prdct.f prdctO
explain     -0.390                              
predct.xpln -0.367  0.484                       
prdct.fdbck -0.382  0.487  0.486                
predictOnly -0.417  0.492  0.488   0.492        
spatial     -0.408 -0.066 -0.113  -0.085  -0.021
> coefs <- data.frame(coef(summary(full.model.absolute)))
> coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
> coefs
                   Estimate Std..Error    t.value       p.z
(Intercept)      109.611811  12.443420  8.8088173 0.0000000
explain          -12.087217  10.783167 -1.1209339 0.2623160
predict.explain   -2.299011  10.892451 -0.2110646 0.8328368
predict.feedback  -5.602581  10.798600 -0.5188248 0.6038829
predictOnly      -13.735708  10.586643 -1.2974563 0.1944742
spatial           -2.710754   1.308784 -2.0712011 0.0383400
> confint(full.model.absolute)
Computing profile confidence intervals ...
                      2.5 %      97.5 %
(Intercept)       84.941636 134.2819053
explain          -33.069172   8.8947377
predict.explain  -23.493611  18.8955886
predict.feedback -26.614565  15.4094027
predictOnly      -34.335266   6.8638504
spatial           -5.257393  -0.1641146

#################################################################################
#Analyzing reletive error
full.model.relative <- lmer(gap ~  visual * (explain + predict.explain + predictOnly + spatial) + predict.feedback 
                            + (1|id)+ (1|group), data=relative)

> summary(full.model.relative)
Linear mixed model fit by REML ['lmerMod']
Formula: gap ~ explain + predict.explain + predict.feedback + predictOnly +  
    spatial + (1 | id) + (1 | race)
   Data: relative

REML criterion at convergence: 16322

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.5546 -0.5067 -0.1790  0.3104  8.4020 

Random effects:
 Groups   Name        Variance Std.Dev.
 id       (Intercept) 1606.0   40.07   
 race     (Intercept)  676.6   26.01   
 Residual             7108.0   84.31   
Number of obs: 1380, groups:  id, 230; race, 3

Fixed effects:
                 Estimate Std. Error t value
(Intercept)        86.215     17.617   4.894
explain             2.116     10.986   0.193
predict.explain   -10.194     11.097  -0.919
predict.feedback   39.524     11.001   3.593
predictOnly       -14.557     10.785  -1.350
spatial            -1.530      1.333  -1.148

Correlation of Fixed Effects:
            (Intr) explan prdct.x prdct.f prdctO
explain     -0.281                              
predct.xpln -0.264  0.484                       
prdct.fdbck -0.275  0.487  0.486                
predictOnly -0.300  0.492  0.488   0.492        
spatial     -0.293 -0.066 -0.113  -0.085  -0.021
> coefs <- data.frame(coef(summary(full.model.relative)))
> coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
> coefs
                   Estimate Std..Error    t.value          p.z
(Intercept)       86.214852  17.616796  4.8939007 9.885671e-07
explain            2.115686  10.985631  0.1925866 8.472827e-01
predict.explain  -10.193591  11.096967 -0.9185926 3.583087e-01
predict.feedback  39.524111  11.001353  3.5926590 3.273208e-04
predictOnly      -14.557441  10.785418 -1.3497336 1.771014e-01
spatial           -1.530356   1.333357 -1.1477465 2.510732e-01
> confint(full.model.relative)
Computing profile confidence intervals ...
                      2.5 %     97.5 %
(Intercept)       48.889452 123.540358
explain          -19.265551  23.496923
predict.explain  -31.791519  11.404338
predict.feedback  18.112274  60.935949
predictOnly      -35.549004   6.434123
spatial           -4.125458   1.064745

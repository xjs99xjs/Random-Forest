
> for(mtry in 1:14) {
  +   rf=randomForest(DPQsum ~ RIAGENDR+INDFMPIR+DRQSDIET+DR1TTFAT+AIALANGA
                      +                   +DR1TS060+DR1TCAFF+DR1TKCAL+DR1TPROT+DR1TCARB
                      +                   +DR1TSUGR+DR1TFIBE+FIAINTRP+DR1TALCO , data = dat , subset = train,mtry=mtry,ntree=400) 
  +   oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  +   
    +   pred<-predict(rf,test) #Predictions on Test Set for each Tree
    +   test.err[mtry]= with(dat[-train,], mean( (DPQsum - pred)^2))
    +   cat(mtry," ")
    + }
1  2  3  4  5  6  7  8  9  10  11  12  13  14  > 
  > 
  > test.err
[1] 12.578509  6.615550  6.313120  6.280922  6.237985
[6]  6.279919  6.338724  6.363391  6.401104  6.396304
[11]  6.408346  6.445201  6.443134  6.487446
> oob.err
[1] 13.064556  7.091621  6.797043  6.858752  6.848386
[6]  6.910250  6.855400  6.932430  6.955314  6.968371
[11]  6.980657  7.029657  6.977070  7.072912


> importance(dat_rf)
IncNodePurity
RIAGENDR     2688.3980
INDFMPIR    24457.4970
DRQSDIET     2610.4099
DR1TTFAT    17891.4170
AIALANGA     1872.5053
DR1TS060    18013.5337
DR1TCAFF    18444.1307
DR1TKCAL    16882.8383
DR1TPROT    19179.5084
DR1TCARB    17329.2692
DR1TSUGR    19365.2923
DR1TFIBE    19964.8124
FIAINTRP      651.2963
DR1TALCO     5731.0788

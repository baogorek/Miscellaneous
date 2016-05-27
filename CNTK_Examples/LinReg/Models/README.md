
In the file Models/LinReg.dnn.__AllNodes__.txt, you'll see the following output
which contains the fitted OLS regression parameters:


``` 
Beta=LearnableParameter [1,2]   learningRateMultiplier=1.000000  NeedsGradient=true 
 0.314060867 -0.518696964 
 #################################################################### 
CE=SquareError ( labels , LinPred ) 
features=InputValue [ 2 x 1 ] 
Int=LearnableParameter [1,1]   learningRateMultiplier=1.000000  NeedsGradient=true 
 3.54583812 
 #################################################################### 
labels=InputValue [ 1 x 1 ] 
LinPred=Plus ( Times1 , Int ) 
Times1=Times ( Beta , features )
```

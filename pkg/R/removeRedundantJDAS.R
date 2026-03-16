#----------------- Korte Uitleg ------------------------------------------------#
# Onderstaande code vereenvoudigt onnodig ingewikkelde edits en verwijdert overbodige edits.
# 
# Het proces bestaat uit vier stappen
#
# Stap 1:  Controleer of de edits feasible zijn, d.w.z. controleer of er minimaal 1 oplossing is die aan alle edits voldoet.
#          De stappen 2, 3 en 4 worden alleen uitgevoerd als de edits feasible zijn.       
# Stap 2:  Het opsporen van de 'fixed values', variabelen die slechts één mogelijke waarde mogen aannemen. 
#          bijv: x3 die alleen de waarde 0 mag aannemen.
#          die variabelen worden in eerste instantie geimputeerd en zodoende verwijderd.
#          aan het eind wordt er een edit toegevoegd die de fixed value weergeeft (bijv. x3=0)
# Stap 3:  Aanpassen van conditionele edits
#    3.A.  Vereenvoudigen van samengestelde conditionele edits
#          Samengestelde conditionele edits zijn van de vorm:
#          "IF A1 en A2 en ... THEN B1 of B2 of ...."
#          Componenten ( A1, A2, B1, B2, etc) die op grond van de andere edits niet voor kunnen komen worden verwijderd  
#          bijv: edit1 x>0; edit 2 "if y > 0 then x <0 of z<0"; impliceren dat edit 2 vereenvoudigd kan worden tot if y >0 then z <0  
#    3.B   Vervangen van conditionele edits door onconditionele edit (waar mogelijk)
#          bijv.de edits ( if x< 0 then y>=0) en (if x>=0 then y>=0) impliceren de onconditionele edit y>=0.
#          de onconditionele edit wordt toegevoegd. Vervolgens worden de overbodige conditionele edits verwijderd.
# Stap 4.  Verwijderen van alle overige overbodige edits, bijv:  de edit x < 5 is overbodig als er ook een edit x<6 bestaat.
# 
# De belangrijkste functie is CleanEdits. Die functie roept alle andere functies aan. 
# Hieronder worden de functies per type weergegeven. De functies CLeanedits staat achteraan.

#----Inititalisatie: werkdirectory en libraries benoemen...aanpassen indien nodig

setwd("G:/onderhanden_werk/gaafmaakonderzoek2013")
path <- paste(getwd(),"/", sep="")
.libPaths("//dmkv1f/dmk1/kennR/R/R2.15")
library(editrules)
library(deducorrect)
library(lpSolveAPI)


#----Algemene functies, eenvoudige hulpfuncties----------------------------------------------------------------------------------------#

# isCategoricalVariable geeft aan of een variabele geheeltallig is. 
isCategoricalVariable <- function(E){
  categoricalvar <-  grepl(".",getVars(E), fixed=TRUE)   #Variabelen met een punt in de naam zijn geheeltallig. 
  return(categoricalvar)}

# isNumericalEdits geeft aan welke edits numeriek zijn
isNumericalEdit <- function (E) {
  numericals <- grepl("num",rownames(E), fixed=TRUE) 
  return(numericals)}

# isMixedEdit geeft aan of een edit een conditionele (dwz IF-THEN) edit is. 
isMixedEdit <- function (E){
  numericals <- grepl("num",rownames(E), fixed=TRUE) # numerieke edit
  mixed <- rep(FALSE,nrow(E))
  if (sum(isCategoricalVariable(E)) > 0 ) {
      containscategorical <-rowSums(contains(E,var=getVars(E)[isCategoricalVariable(E),drop=F]))>0
      mixed <- numericals & containscategorical  
  }      
return(mixed)}

# isCatEdit geeft weer of een edit categoriaal is
isCatEdit <- function(E){
  catEdits <- grepl("num",rownames(E), fixed=TRUE) ==FALSE  
  return(catEdits)
}  
  
#DeleteEdit verwijdert een edit uit de editset E
DeleteEdit <- function (E, i){
  return(E[c(1:nrow(E))!=i])
}

#--------------Algemene functies, aanroep van de solver

# AdaptToMip verandert de operatoren van edits, met als doel de edits leesbaar te maken voor de solver:
#    == wordt = 
#    '< rhs' wordt '<= rhs - epsilon'
# De lp solver kan alleen '<=',  '>='  en '=' operatoren aan, dus geen < en ==
AdaptToMip    <- function(E, epsilon=as.mip(E)$epsilon)  {
  ops  <- getOps(E)  	  # operator van de edits in de invoer	"==" "'<" OF "<=" (normaalvorm)			 
  rhs <- getb(E)                  # right hand side van de editmatrix (de b in het stelsel Ax <= b)  
  rhs[ops== "<"] <-  rhs[ops== "<"] - epsilon  
  ops[ops== "<"] <-  "<="  # "kleiner dan wordt vervangen door kleiner of gelijk dan"
  ops[ops== "=="] <- "="   # dubbel gelijkteken wordt vervangen door enkel gelijkteken
  return(list("A"=getA(E),"ops"=ops,"rhs"=rhs))
}


# FillMip creates a new lpSolve linear program model object en vult deze. NB: De doelfunctie wordt leeggelaten.
FillMip <- function(A, ops, b, iscat, epsilon=as.mip(E)$epsilon)  {
  nvar <- ncol(A)    # aantal variabelen 
  ncon <- nrow(A)             #  aantal constraints 
  p   <- make.lp(ncon, nvar)   #  lp-probleem wordt aangemaakt met nvar variabelen en ncon constraints
  for (j in 1: nvar)  {set.column(p, j, A[,j])}  # kolomsgewijs inlezen van de coefficienten matrix van de editmatrix
  set.constr.type(p,ops)	
  set.rhs(p,b)
  set.bounds(p,lower= rep(-Inf,nvar))		# lower bound van iedere variabele is min oneinding
  set.bounds(p,upper= rep(Inf,nvar))		# upper bound van iedere variabele is plus oneinding   
  set.type(p,which(iscat),"binary")     # benoemen van de geheeltallige variabelen; iscat wordt gebruikt als indicator.
  return(p)
} 



isFeasible <- function (p) {
  lp.control(p, break.at.first = TRUE, epsint= 1.0e-15, epspivot=1.0e-15)  #Zodra er een toegelaten oplossing is gevonden kan de zoektocht naar een optimale oplossing worden  afgebroken; we  hoeven namelijk alleen te weten of het probleem 'feasible' is
  result <-solve(p)  # start optimalisatie
  feas <- (result !=2)   # result = 2 geeft aan dat probleem infeasible is; iedere andere code betekent 'feasible'.
  return(feas)
}



#-------------------Algemene functie voor het veranderen van het teken van de edits (bepalen van de negatie)--------

# ChangeOperatorToLargerThan verandert het teken van edit i: de edit wordt van het type "groter dan".
# De functie is bedoeld om toe te passen op "<=" edits. De functie bepaalt dus de negatie van de edit.
# Aangezien de solver geen ">" edits aankan, wordt de operator ">=" en wordt er epsilon opgeteld bij de right hand side van de edit.
ChangeOperatorToLargerThan <- function (A, ops, b, i, iscat, epsilon =as.mip(E)$epsilon) {
  ops[i] <- ">="
  b[i] <- b[i] + epsilon
  return(list("A"=A,"ops"=ops,"b"=b))
}

# ChangeOperatorToSmallerThan verandert het teken van edit i: de edit wordt van het type "kleiner dan"
# De functie is bedoeld om toe te passen op ">=" edits. De functie bepaalt dus de negatie van de edit.
# Aangezien de solver geen "<" edits aankan, wordt de operator "<=" en wordt er epsilon afgetrokken van de right hand side van de edit.
ChangeOperatorToSmallerThan <- function (A, ops, b, i, epsilon=as.mip(E)$epsilon) {   
  ops[i] <- "<="
  b[i] <- b[i] - epsilon
  return(list("A"=A,"ops"=ops,"b"=b))
}

# In de MIp-formulering worden IF-THEN edits omgezet in een aantal categoriale edits.
#  De geheeltallige variabelen worden gebruikt om aan te geven dat aan één van een aantal lineaire restricties moet worden voldaan.
# In ChangeOperatorsMixedEdits wordt de negatie van de conditionele edits bepaald.
# Die negatie luidt dat aan alle lineaire restricties behorende bij een IF_THEN restrictie niet wordt voldaan.
# Van alle lineaire restricties die bij één IF-THEN statement horen wordt het teken veranderd: een <= restrictie wordt veranderd in een > restrictie.
# Aangezien de solver geen ">" edits aankan, wordt de operator ">=" en wordt er epsilon opgeteld bij de right hand side van de edit.
# Bovendien worden de geheeltallige variabelen verwijderd uit de betreffende lineaire restricties.
# De geheeltallige variabelen zijn hier niet nodig omdat aan ieder van de omgekeerde lineaire edits moet worden voldaan.
ChangeOperatorsMixedEdits <- function (A, ops, b, i, iscat, epsilon =as.mip(E)$epsilon) {
  catvarsInEdit <- which(A[i,]*iscat!=0)   # geeft aan welke categoriale variabelen voorkomen in edit i
  catnames       <- names(catvarsInEdit)  # de namen van deze variabelen  
  catedits      <- rownames(A) %in% catnames  #In de MIP-formulering wordt er voor iedere  categoriale variabele  een  linaire edit gedefinieerd. De naam van die lineaire edit is de naam van de categoriale variabele. Catedits verwijst naar de hulpedits die horen bij edit i .
  A[, A[i,]*iscat!=0]<-0  # de categoriale variabelen die in edit i voorkomen worden verwijderd in alle edits. HUn coefficienten worden nul in alle edits
  ops[catedits] <- ">="   # teken wordt 'groter dan' 
  b[catedits] <- b[catedits] + epsilon
  return(list("A"=A,"ops"=ops,"b"=b))
}

#---------------Algemene functies, opschonen van edits 
# Na aanpassing van één of meerdere edits, kunnen er mogelijk nog meer edits worden aangepast. 
# Hieronder staan enkele functies die aangepaste edits verder vereenvoudigen.
# De hoofdfunctie is SimplyEdits. 

# Imputeer nul voor alle categoriale variabelen die in mixed edits voorkomen met een rhs van nul. 
# bijv. l1 + l2 <= 0.  dan l1 =0 en l2=0.
ImputeFixedCatVarsZero   <- function(E){
  A <-getA(E)
  b <-getb(E)
  iscat <-  isCategoricalVariable(E)
  numericalEdits <- isNumericalEdit(E)
  ANumEditsCatvars <- A[numericalEdits, iscat, drop=FALSE]
  bNumEditsCatvars <- b[numericalEdits, drop=FALSE]
  Redundant <- (rowSums(ANumEditsCatvars)>0)  & (bNumEditsCatvars==0)
  if (sum(Redundant)>0)  {
    RedundantEdits <- which(Redundant)
    varsinRedundantEdits <- contains(E[RedundantEdits,,drop=F])
    RedundantVar <- getVars(E)[colSums(varsinRedundantEdits)>0, drop=F]    
    if (length( RedundantVar)>0){E <-  substValue(E,RedundantVar,rep(0, length(RedundantVar)),reduce=TRUE, removeredundant=TRUE)  }
  }
  return(E)
}

# Categorical variables that necesarily have to be equal to one are imputed
#  if-then edits resulteren in mixed edits met de eigenschap dat de rhs kleiner is dan het aantal categoriale variabelen   
#  Categoriale variabelen die niet (meer) in zulke edits voorkomen kunnen worden geelimineerd
#  stel bijv dat .l1 en .l2 uitsluitend in de volgende mix edits voorkomen: .l1 + .l2 <= 2 
# in dat geval mag voor .l1 en .l2 de waarde 1 worden gesubstitueerd.
# als daarnaast de edit .l1 + .l3 <= 1 zou voorkomen dan mag alleen .l2 worden gesubstitueerd. .l1 komt namelijk voor in een conditionele edit
ImputeFixedCatVarsOne   <- function(E){
  iscat <-  isCategoricalVariable(E)
  A <-getA(E)
  b <-getb(E)
  numericalEdits <- isNumericalEdit(E)
  ANumEditsCatvars <- A[numericalEdits, iscat, drop=FALSE]
  bNumEditsCatvars <- b[numericalEdits, drop=FALSE]
  Enum <- E[numericalEdits,,drop=F]
  RedundantMixEdits <- (rowSums(ANumEditsCatvars)== b[numericalEdits])  & (rowSums(ANumEditsCatvars)>0)
  if (sum(RedundantMixEdits)>0)  {
    NonRedundantVars <- colSums(contains(Enum[!RedundantMixEdits ,,drop=F], var= getVars(E)[iscat]))>0
    RedundantVar <- !NonRedundantVars
    if (length(RedundantVar)>0){E <-  substValue(E,names(which(RedundantVar)),rep(1, sum(RedundantVar)),reduce=TRUE, removeredundant=TRUE)  }
  }
  return(E)
}  

# Categoriale edits met categoriale variabelen die niet (meer) in een numerieke edit voorkomen kunnen worden weggelaten.
DeleteRedundantCatEdits <- function(E){
  iscat <-  isCategoricalVariable(E)
  catvarnames <- getVars(E)[iscat,drop=F]
  isnumericalEdits <- isNumericalEdit(E)
  NumericalEdit <-E[isnumericalEdits,,drop=F]
  if (sum(iscat)>0)  {
      isRedundantCatvar <-colSums(contains(NumericalEdit,var=catvarnames ))==0
      RedundantCatvar <- catvarnames[isRedundantCatvar, drop=F]
      RedundantCatEdit <- RedundantCatvar
      if (length(RedundantCatEdit) >0) {
        E<-E[rownames(E) %in% RedundantCatEdit==F,,drop=F]     
    }        
  }   
  return(E)
}  

#categoriale edits, die na een imputatie, geen categoriale variabelen meer omvatten, zijn in feite numerical edits geworden.
# de naamgeving wordt hierop aangepast.  
ChangeEditNameintoNumerical <- function(E){
  iscat <-  isCategoricalVariable(E)
  iscatEdits <- isCatEdit(E) 
  if (sum(iscatEdits)>0) {
    isHiddenNumerical <-  iscatEdits[iscatEdits,drop=F]  
    if (sum(iscat)>0 ) {
       isHiddenNumerical <-rowSums(contains(E[iscatEdits,,drop=F], var=getVars(E)[iscat, drop=F]))==0
    }          
    #nieuwe naam wordt Num plus de naam van de categoriale variabele, maar dan zonder punt. bijv .l4 wordt numl4
    rownames(E)[iscatEdits][isHiddenNumerical]<-paste ("num", gsub(".", "",  rownames(E[iscatEdits,,drop=F])[isHiddenNumerical,drop=F], fixed=TRUE ), sep= "")          
  }    
  return(E)
}

#(nieuwe) numerieke edits met een rhs groter dan 0.5*M of kleiner dan -0,5*M worden overbodig verondersteld.
DeleteRedundantNumEdits <- function(E){
  iscat <-  isCategoricalVariable(E)
  isnumericalEdits <- isNumericalEdit(E)
  isRedundant <- rep(FALSE, nrow(E))
  b <-as.matrix(getb(E))
  if (sum(isnumericalEdits)>0 ) {
    isRedundant[isnumericalEdits] <- abs(b[isnumericalEdits])>= 0.5*as.mip(E)$M
    E <-E [!isRedundant, drop=FALSE]  
  }
  return(E)  
}  

# edits die overbodig zijn geworden worden verwijderd.
SimplifyNewEdits <- function(E){
  E <- ImputeFixedCatVarsZero(E)
  E <- ImputeFixedCatVarsOne(E)
  E<-DeleteRedundantCatEdits(E)
  E<-ChangeEditNameintoNumerical(E)
  E<-DeleteRedundantNumEdits(E)
  return(E)
}

#--------------Algemene functies, transformaties van editset naar editmatrix en vice versa
# De in- en uitvoer is een editset. De bewerkingen voor het vereenvoudigen van de edits vinden plaats op een editmatrix.
# Het is dan ook nodig om de editset uit de invoerbestanden om te zetten in een editmatrix.
# Nadat de edits zijn bewerkt is het ook nodig om de editmatrix terug te vertalen naar een editset. 
# Hieronder staan twee functies voor deze transformaties. 

# PrepareEdits creeert een editmatrix in de zogenaamde normaalvorm.
PrepareEdits<-function (E, epsilon=as.mip(E)$epsilon) {
  E   <- as.mip(E, epsilon )$E    
  E   <- normalize(E)      
  return(E)
}

# EditMatrixToEditSet zet een  editmatrix om in een editset.
 # Voor de mixed-edits zijn enige bewerkingen nodig. In een editset worden categoriale variabelen gebruikt.
#  Een mixed statement heeft de vorm:  C1 of C2 of C3....
#  Ze worden omgezet in de vorm: IF (niet C1) THEN C2 of C3 etc.
EditMatrixToEditSet <- function (E){
  Editlist <- ""
  if (nrow(E)>0) {
    # Basis voor de output zijn de niet-categoriale edits uit E. De categoriale edits komen niet in de editset. 
    # De niet-categoriale edits (Puur numeriek en mixed) worden eerst opgeslagen in een matrixformaat.
    Em <- as.matrix  (as.data.frame(as.editset(E)))  #Em zijn de edits uit E in matrixformaat (Tekst)
    Editlist <-    Em[!grepl(".",Em[,1],fixed=TRUE),, drop=FALSE]  #de niet categoriale edits uit Em...(basis voor de output). 
    # De mixed edits uit de editlist vereisen enkele bijzondere bewerkingen. Hieronder worden ze geidentificeerd
    ismixed <- grepl(".l",Editlist[,2],fixed=TRUE)  # de mixed edits uit editlist ...een 1 betekent mixed edit; een nul betekent puur numeriek
    mixedname <- which( grepl(".l",Editlist[,2],fixed=TRUE) ) # de posities van de  mixed edits  in Editlist
    #changes the names in Editlist...de naam begint met mix of num, gevolgd door een volgnummer.
    Editlist[,1] <- paste( ifelse(ismixed,"mix", "num") , (row(Editlist)[,1]), sep="")  
    # De categoriale variabelen uit de editmatrix komen niet voor in de editset. Daarom worden zij geimputeerd met nul.
    catVarsinE <- getVars(E)[grepl(".",getVars(E),fixed=TRUE),drop=F] #namen van de categoriale variabelen
    if (length(catVarsinE > 0)) {E<-substValue(E,catVarsinE,rep(0,length(catVarsinE)),reduce=FALSE, removeredundant=FALSE)}#imputeer de waarde nul voor alle categoriale variablen in the editmatrix E...de categoriale variabelen komen namelijk niet voor in een editset
    #aanmaken van een lijst met daarin op de rijen de mixed edits en in de kolommen  de categoriale varhiabelen die in die  mixed edit voorkomen.
    VarListMixEdits <-Editlist[ismixed,2]   # voorbeeld edit l1+L2 <= 1
    if (length (VarListMixEdits) >0){
      VarListMixEdits <- gsub("<", "+", VarListMixEdits)   #vervang < door +  bijvoorbeeld: l1 + l2 <=1 wordt l1 + l2 +=1   
      CatVarsinMixEdits <- (strsplit((VarListMixEdits),"+" , fixed=TRUE ))  # splitsen op +..  je krijgt dan  l1,l2, =1
      # Omzetten van de mixed edits naar ' leesbare' IF-then edits.
      for (i in 1 : length(CatVarsinMixEdits))  { 
        for (j in 1  :length(CatVarsinMixEdits[[i]])-1 )  {   # in alle kolommen, muv de laatste, staan categoriale variabelen.      
          editname <- gsub(" ","",CatVarsinMixEdits[[i]][j])  # de namen van de categoriale variabelen corresponderen met categoriale edits in E
          edit <- E[which(rownames(E)==editname),]
          editdf <- as.data.frame(edit)$edit        # de betreffende edit wordt gekopieerd uit E en omgezet in een dataframe
          if (j==1)  {
            editdf <- gsub("<=", ">", editdf)    #een mixed edit wordt omgezet van het formaat C1 of C2...of Cn naar IF not C1 THen C2 of...of Cn.
            newedit <- paste(" if (", editdf, ")", sep=" ")   # de eerste component (j==1) komt in het if-deel. Het <= teken vervangen door >               
          }
          if (j>1){   # tweede, derder, vierde term komen in het "THEN" gedeelte
            newedit <- paste (newedit, editdf , sep =" " )  # in het then deel wordt de editdf gewoon overgenomen
            if (j < length(CatVarsinMixEdits[[i]])-1) { newedit <- paste(newedit, "|", sep=" ") }        
          }
        }
        Editlist[mixedname[i],2] <- newedit  #plaats de aangepaste mixed edit terug in de editlist
      }
    }  
  }   
  return(Editlist)  
}


#------functies   voor Stap 1----detecteren of het stelsel edits stijdig is.

isEditsFeasible<-function(E, epsilon=as.mip(E)$epsilon) {  
  iscat <-  isCategoricalVariable(E)  # iscat is 1 voor alle geheeltallige variabelen
  A  <-  AdaptToMip(E)$A    # De coefficientenmatrix, operators en rhs worden aangepast zodanig dat deze leesbaar worden voor de solver.
  ops <- AdaptToMip(E)$ops
  b   <- AdaptToMip(E)$rhs
  p <-   FillMip(A, ops, b, iscat, epsilon) # lineair programmeringsprobleem wordt aangemaakt.
  feasible <- isFeasible(p)    # test op feasibility
  return(feasible)
}

#------functies   voor Stap 2----aanpassen van de edits voor fixed values: variabelen die slechts één mogelijke waarde mogen aannemen

# geeft de kleinst mogelijke waarde van variabele i, gegeven de edits die worden weergegen in een lp-solve object p
MinimumValue <-function(A, p ,i){
  minval <- -9999
  objective <- rep(0, ncol(A))   # objective is een vector met de coefficienten van de doelfunctie
  objective[i] <- 1   # het i-de element is 1; variabele i wordt immers geoptimaliseerd
  set.objfn(p,objective)
  result <-solve(p)
  if (result ==0) {minval <- get.objective(p)} # result=0 betekent dat een eindig minimum is gevonden
  if (result > 0) {minval <- -9999}   # indien geen eindig minimum is gevonden wordt de waarde -9999 gesubstitueerd.
  return(minval)
}

# geeft de grootst mogelijke waarde van variabele i, gegeven de edits die worden weergegen in een lp-solve object p
MaximumValue <-function(A, p ,i){
  maxval <- 9999
  objective <- rep(0, ncol(A)) # objective is een vector met de coefficienten van de doelfunctie
  objective[i] <- 1 # het i-de element is 1; variabele i wordt immers geoptimaliseerd
  lp.control(p, sense="max") # maximalisatie van de doelfunctie
  set.objfn(p,objective)
  result <-solve(p)
  if (result ==0) {maxval <- get.objective(p)}  # result is 0 betekent dat een eindig maximum is gevonden
  if (result > 0) {maxval <- 9999} # indien geen eindig minimum is gevonden wordt de waarde +9999 gesubstitueerd.
  return(maxval)
}

#
# MinimizeEachVariable resulteert in een vector met daarin de minimum waarde per variabele, gegeven een editset
# de categoriale variabelen worden niet meegenomen. 
MinimizeEachVariable <- function (A, ops, b, iscat, epsilon){
  smallest <- rep(-9999, ncol(A))  # initialisatie op -9999
  p <-   FillMip(A, ops, b, iscat, epsilon)    
  for (i in 1: ncol(A))  {
    if (!iscat[i]) {    smallest[i] <- MinimumValue(A, p, i)}  #bepalen van minimum per variable
  }
  return(smallest)}

# MinimizeEachVariable resulteert in een vector met daarin de maximum waarde per variabele, gegeven een editset
# de categoriale variabelen worden niet meegenomen.
MaximizeEachVariable <- function (A, ops, b, iscat, epsilon){
  largest <- rep(9999, ncol(A)) # initialisatie op +9999
  p <-   FillMip(A, ops, b, iscat, epsilon=0.001)    
  for (i in 1: ncol(A))  {
    if (!iscat[i]) {largest[i] <- MaximumValue(A, p, i)}  # bepalen van maximum per variabele
  }
  return(largest)}

# De functie geeft de variablen weer die slechts één mogelijke waarde kunnen aannemen.
# de categoriale variabelen worden niet meegenomen.
FixedValues <- function (E, epsilon=as.mip(E)$epsilon){
  iscat <-  isCategoricalVariable(E)  # iscat is 1 voor alle geheeltallige variabelen
  Adapt <- AdaptToMip(E)
  A  <-  Adapt$A    # De coefficientenmatrix, operators en rhs worden aangepast zodanig dat deze leesbaar worden voor de solver.
  ops <- Adapt$ops
  b   <- Adapt$rhs
  minima <- MinimizeEachVariable (A, ops, b, iscat, epsilon) # een vector met daarin de minimale waarde per variabele
  maxima <- MaximizeEachVariable (A, ops, b, iscat, epsilon) # een vector met daarin de maximale waarde per variabele 
  return(list("variables"=getVars(E)[minima==maxima],"values"=minima[minima==maxima]))  # output zijn de variablenamen met een fixed value.
}

# de edits met fixed vars worden opgeslagen in een editset.
# dit zijn edits die veranderen na de imputatie van die variabelen.
LogEditswithFixedValues <-function(E, fixedvars, fixedvals){
  DoesEditContainFixedVars <- as.matrix(rowSums(contains(E,var=fixedvars, drop=FALSE)) > 0) # indicator die aangeeft of een numerieke of categoriale edit een fixed variable omvat 
   #DoesEditContainFixedVars omvat nooit de mixed edits. 
  # mixed edits waar fixedvars in voor komen worden hieronder toegevoegd
  DoesCatEditContainFixedVars <- DoesEditContainFixedVars * isCatEdit(E)
  CatEditswithFixedVars <- rownames(E) [ DoesCatEditContainFixedVars==1 , drop=F]
  if (length( CatEditswithFixedVars )>0) {
      isMixedEditwithFixedVars <-  as.matrix( (rowSums(contains(E, var=  CatEditswithFixedVars) > 0 ) * (isMixedEdit(E)==TRUE)))# indicator die aangeeft of een numerieke of categoriale edit een fixed variable omvat   
      DoesEditContainFixedVars <-DoesEditContainFixedVars+isMixedEditwithFixedVars 
   }
   DoesEditContainFixedVars [isCatEdit(E)] <- TRUE     
   EditsWithFixedVars <- EditMatrixToEditSet(E[   DoesEditContainFixedVars==1,, drop=F])  # de edits met fixed variables 
   ChangedEditsWithFixedVars <-  EditMatrixToEditSet(substValue(E[DoesEditContainFixedVars==1,, drop=F],fixedvars,fixedvals,reduce=TRUE, removeredundant=TRUE))  # de edits met fixed variables na aanpassing
  return (list("EditsWithFixedVars"=EditsWithFixedVars,"ChangedEditsWithFixedVars"=ChangedEditsWithFixedVars))
} 
  
# values that can attain only one value are substituted
DetermineFixedValues<-function(E,epsilon=as.mip(E)$epsilon){
  fixedvars <- ""   # initialisatie
  fixedvals <- ""   # initialisatie
  LogFixed <- ""  #initialisatie
  fixed <- FixedValues(E)
  if (length (fixed$variables) > 0 ) { 
    fixedvars <-fixed$variables #numerieke variabelen die slechts één mogelijke waarde mogen aannemen
    fixedvals <-fixed$values
    LogFixed <- LogEditswithFixedValues(E,fixedvars)
  } 
  return(list("E"=E,"variables"=fixedvars,"values"=fixedvals, "EditsWithFixedVars"=LogFixed))
}



# values that can attain only one value are substituted
SubstituteFixedValues<-function(E,epsilon=as.mip(E)$epsilon){
  fixedvars <- ""   # initialisatie
  fixedvals <- ""   # initialisatie
  LogFixedEditswithFixedVars <- ""  #initialisatie
  LogFixedChangedEditsWithFixedVars <- ""
  fixed <- FixedValues(E)
  if (length (fixed$variables) > 0 ) { 
    fixedvars <-fixed$variables #numerieke variabelen die slechts één mogelijke waarde mogen aannemen
    fixedvals <-fixed$values
    LogFixed <- LogEditswithFixedValues(E,fixedvars, fixedvals)
    LogFixedEditswithFixedVars<-LogFixed$EditsWithFixedVars
    LogFixedChangedEditsWithFixedVars<-LogFixed$ChangedEditsWithFixedVars 
    
    E<-substValue(E,fixedvars,fixedvals,reduce=TRUE, removeredundant=TRUE)  # de fixed values worden gesubstitueerd
    E<-SimplifyNewEdits  (E)      # Zo mogelijk worden de resulterende edits verder vereenvoudigd.
  }
  return(list("E"=E,"variables"=fixedvars,"values"=fixedvals, "OldEditsWithFixedVars"=LogFixedEditswithFixedVars, "NewEditsWithFixedVars"= LogFixedChangedEditsWithFixedVars  ))
}


#  Uitgangssituatie: een edit set waarin de fixed values, variabelen die slechts één waarde kunnen aannemen, zijn gesubstitueerd.
# ze komen dus niet meer voor.
# Aan het einde van het gehele opschoonproces worden de fixed values weer toegevoegd in de vorm van een edit.
# Bijv. de edit Y=100.
# DE in- en uitvoer is een editset.

AddFixedValuestoanEditSet <- function (Es, fixvars, fixvalues){
  Fs<-""
  #if (Es[1]!=Fs) {
     if (Es[1] != "") {NumberofEdits <- nrow(Es)}
     if (Es[1] == "") {NumberofEdits <- 0}
     if (fixvars[1] != "") {NumberofFixed <- length(fixvars)} else {  NumberofFixed <- 0}
     Fs <- matrix (nrow= NumberofEdits +NumberofFixed, ncol= 2)
     if (NumberofEdits > 0) {Fs[1: NumberofEdits ,]<- Es}
     if (NumberofFixed > 0)  {
     for (i in 1 : NumberofFixed ) {
        Fs[ NumberofEdits +i,1]<- paste ("num", i+ NumberofEdits , sep="")
        Fs[ NumberofEdits +i,2]<- paste (fixvars[i], " ==", fixvalues[i] , sep="" )   
      }
    }
 # } 
  return(Fs)
}  

#------functies   voor Stap 3----vereenvoudigen van conditionele edits.


# Een edit "IF (A1 en A2 en A3 en ...)  THEN (B1 of B2 of B3 of ...)"  wordt intern geschreven als "niet A1" of "niet A2" of "niet A3"  of...of B1 of B2 of B3.
# oftewel: C1 of C2 of ...of Cn
# De functie onderzoekt of een van deze componenten (zeg C1) altijd waar is.
# Daartoe wordt gecontroleerd of er een oplossing is voor het probleem, dat wordt verkregen door "niet C1"  toe te voegen aan de edits.
# De combinatie van edits:
# C1 of C2 of....of Cn
# niet C1
# wordt herschreven tot
# C2 of....of Cn 
# niet C1
# Indien dit tot een infeasible stelsel leidt dan is C1 altijd waar
isPartofComposedEditAlwaysTrue<-function (E, i, iscat, epsilon=as.mip(E)$epsilon){
  A <-getA(E)
  CatHelpVariableInEdit <- names(which(A[i,]*iscat!=0))   # de naam van de geheeltallige variabele in edit i 
  F<- substValue(E, CatHelpVariableInEdit , 1,   reduce = FALSE, removeredundant = FALSE) # subsitutie van de waarde 1 voor de geheeltallige variabele. Betekenis is dat er niet wordt voldaan aan edit i. In het geval i=1 wordt de bewering C1 of ...of Cn veranderd in C2 of...of Cn.
  F[i,] <- E[i,]       #originele edit i wordt overgenomen
  F[, CatHelpVariableInEdit] <- rep(0,length(getb(F)))  #de categoriale hulpvariabele in edit i worden verwijderd...edit i maakt geen deel meer uit van de samengestelde IfThen edit, maar staat op zichzelf
  A <- AdaptToMip(F, epsilon)$A
  ops <- AdaptToMip(F, epsilon)$ops                          
  b <- AdaptToMip(F, epsilon)$rhs                      
  Ai <-  ChangeOperatorToLargerThan (A, ops, b, i,iscat, epsilon )$A  #bepalen van de negatie van edit i
  opsi<- ChangeOperatorToLargerThan (A, ops, b, i,iscat, epsilon )$ops  
  bi  <- ChangeOperatorToLargerThan (A, ops, b, i,iscat, epsilon )$b     
  p <-   FillMip(Ai, opsi, bi, iscat, epsilon=0.001)
  redundant <- !isFeasible(p) 
  return(redundant)} 

 
# De functie isPartofComposedEditAlwaysViolated onderzoekt of een edit i, een onderdeel van een conditionele edit, altijd wordt geschonden.
# Een edit "IF (A1 en A2 en A3 en ...)  THEN (B1 of B2 of B3 of ...)"  wordt intern geschreven als "niet A1" of "niet A2" of "niet A3"  of...of B1 of B2 of B3.
# oftewel: C1 of C2 of ...of Cn
# De functie onderzoekt of een van deze componenten (zeg C1) altijd wordt geschonden 
isPartofComposedEditAlwaysViolated<-function (E, i, iscat, epsilon=as.mip(E)$epsilon){
  A <-getA(E)
  CatHelpVariableInEdit <- names(which(A[i,]*iscat!=0)) # de naam van de geheeltallige variabele in edit i 
  F<- substValue(E, CatHelpVariableInEdit , 0,   reduce = FALSE, removeredundant = FALSE) #subsititutie van de waarde nul. Dit betekent dat aan edit i (een component van if-then functie) wordt voldaan
  A <- AdaptToMip(F, epsilon)$A
  ops <- AdaptToMip(F, epsilon)$ops                         
  b <- AdaptToMip(F, epsilon)$rhs                        
  p <-   FillMip(A, ops, b, iscat, epsilon=0.001)    
  redundant <- !isFeasible(p)   #als het mip probleem strijdig is dan betekent dat dat er niet aan edit i kan worden voldaan. Edit i is een onderdeel van een samengestelde if-then edit. Omdat edit i niet waar kan zijn kan het uit de samengestelde if-then edit worden verwijderd.
  return(redundant)} 


# In LogSimplifiedEdits wordt bijgehouden welke conditionele edits vereenvoudigd worden. Resultaat wordt weggeschreven in een editset
# RedundantCatVar omvat de categoriale variabelen behorende bij een  overbodige onderdeel van een if-then edit
LogSimplifiedEdits <- function(E, RedundantCatVar){
  SimplifiedComposedEdits <- ""  # initialisatie
    # SimplifiedComposedEdits is true voor alle numerieke edits die vereenvoudigd worden. 
  # Daarnaast is de waarde true voor alle categoriale variabelen (dat komt namelijk verderop goed uit, bij het omzetten van de editmatrix naar een editset zijn ze (mogelijk) nodig, dat gebeurt in de functie Editmatrixtoeditset)
  isEditSimplified <- rowSums(contains(E,RedundantCatVar,drop=FALSE))>0       
  isEditSimplified[isCatEdit(E)]<-TRUE
  if (sum(isEditSimplified[!isCatEdit(E)])>0) {
    SimplifiedEdits <- E[isEditSimplified,, drop=F]                                
    SimplifiedComposedEdits <- EditMatrixToEditSet(SimplifiedEdits)   # omzetten naar editset ...bij het omzetten van conditionele edits zijn de definities van de categoriale edits nodig. Vandaar dat alle categoriale edits hier ook worden meegenomen  
   
  }
  return( SimplifiedComposedEdits)
}

# LogRedundantPartsinMixedEdits houdt bij welke onderdelen van if-then edits overbodig zijn.
# RedundantCatVar omvat de categoriale variabelen behorende bij een  overbodige onderdeel van een if-then edit
LogRedundantPartsinMixedEdits<-function(E, RedundantCatVar){
   RedundantParts <- E[rownames(E) %in% RedundantCatVar,,drop=F] #hier worden categoriale edits geselecteerd die een component beschrijven vaan een mixed edit waar altijd aan wordt voldaan.
   RedundantParts   <- substValue( RedundantParts   ,RedundantCatVar ,rep(0,length( RedundantCatVar)),reduce=TRUE, removeredundant=TRUE) # door deze subsitutie worden de categoriale variablen uit de edit   verwijderd.  Die zijn hier niet nodig.
   rownames(RedundantParts)<-gsub(".l", "num", rownames(RedundantParts) )    # door de bovenstaande imputatie zijn de categoriale edits  veranderd in  een numerieke edit.  Naamgeving wordt hierop aangepast Is nodig voor de functie EditMatrixToEditSet, waarin een editmatrix wordt omgezet in een editset      
   RedundantParts <- EditMatrixToEditSet(RedundantParts)      # omzetten naar editset     
  
  return(RedundantParts)
}


#  Samengestelde conditionele edits worden opgeschoond; de overbodige delen worden verwijderd.
#  d.w.z.  onderdelen die altijd geschonden zijn.
#  resulteert in de nieuwe editmatrix "ENonRedundant"
#  "SimplifiedComposedEdit" is een editset met daarin de vereenvoudigde conditionele edits
#  "RedundantParts" is een editset met de overbodige onderdelen van de vereenvoudigde conditionel edit..
SimplifyComposedMixedEdits <- function (E, epsilon=as.mip(E)$epsilon){
  isredundant<-rep(FALSE,nrow(E))   #initialiseer op FALSE
  iscat <-  isCategoricalVariable(E)  # iscat is 1 voor alle geheeltallige variabelen
  isEditSimplified <- rep(FALSE,nrow(E))    #initialisatie
  SimplifiedMixEdits <- ""          #initialisatie  
  RedundantPartsinSimplifiedMixedEdits <- ""   #initialisatie
  ENonRedundant <- E  #initialisatie
  if (nrow(E)>0)  {
    for (i in 1:nrow(E)){
      if  (isNumericalEdit(E)[i]== FALSE) {isredundant[i]<-isPartofComposedEditAlwaysViolated(E, i, iscat, epsilon)}   #Voor iedere component van een samengestelde IF-THEN edit(die kan worden weergegeven met  C1 of...of Cn) wordt onderzocht of daaraan voldaan kan worden. Als nooit aan een bepaalde component kan worden voldaan kan die worden weggelaten.
    }
    RedundantCatEdit <-  rownames(E)[isredundant,drop=F]  #naam van een categoriale edit behorend bij en overbodig onderdeel van een if-then edit.  
    RedundantCatVar <-  RedundantCatEdit     #categoriale variabele behorende bij een overbodig onderdeel "if-then edit"
    ENonRedundant <- E[isredundant==FALSE,,drop=F] # de niet overbodige edits
    if (length(RedundantCatEdit) >0) {
      ENonRedundant <- substValue(ENonRedundant, RedundantCatVar ,rep(1,length( RedundantCatVar )),reduce=TRUE, removeredundant=TRUE) # subsitutie van de waarde 1 voor een categoriale var. (1 betekent dat niet aan de component Ci voldaan wordt)
      ENonRedundant<-  SimplifyNewEdits(ENonRedundant)  # verder opschonen van de editset
      SimplifiedMixEdits   <- LogSimplifiedEdits (E, RedundantCatVar)

      RedundantPartsinSimplifiedMixedEdits <- LogRedundantPartsinMixedEdits(E, RedundantCatVar)
    }
  }  
  return(list("E"=ENonRedundant,"Simplifiededits"= SimplifiedMixEdits,"SimplifiededitsRedundant" = RedundantPartsinSimplifiedMixedEdits) )
}

# Conditionele edits worden, indien mogelijk, vervangen door een onconditionele edit.
ReplaceConditionalbyUnconditional <- function  (E, epsilon=as.mip(E)$epsilon){
  unconditional <-rep(FALSE,nrow(E))   #initialiseer op FALSE
  iscat <-  isCategoricalVariable(E)  # iscat is 1 voor alle geheeltallige variabelen
  LogSimplifiedMixEdits  <- ""  #initialisatie
  LogNewUnconditional  <- ""     #initialisatie
  isEditSimplified <- rep(FALSE,nrow(E))    #indicator die aangeeft of een edit is aangepast. Initialisatie
  ESimplified <-E  #initialisatie
  if (nrow(E)>0 ){
    for (i in 1:nrow(E)){
      if  (isNumericalEdit(E)[i]== FALSE) {unconditional[i]<-isPartofComposedEditAlwaysTrue(E, i, iscat, epsilon)}  #Voor ieder onderdeel van een samengestelde IF-THEN edit wordt onderzocht of altijd aan dat onderdeel wordt voldaan.
    }
    CatVarUnconditional  <-  rownames(E[unconditional,,drop=F]) #categoriale variabelen behorende bij onderdelen van if-then edits waar altijd aan wordt voldaaan
    if (length(CatVarUnconditional ) > 0) {
      isMixedEditSimplified <- rowSums( contains(E[isMixedEdit(E),,drop=F], var=CatVarUnconditional))>0  # een indicator die aangeeft welke mixed edits overbodig zijn omdat zij een component omvatten waar altijd aan wordt voldaan
      SimplefiedMixedEdits <- E[isMixedEdit(E),,drop=F][isMixedEditSimplified==TRUE,drop=F]
      ESimplified  <- substValue(E,CatVarUnconditional ,rep(0,length(CatVarUnconditional )),reduce=TRUE, removeredundant=TRUE)     # imputatie van de categoriale variabelen die staan voor onderdelen   van if-then edits waaar altijd aan wordt voldaan.
      ESimplified  <- ESimplified  [(rownames( ESimplified ) %in% rownames(SimplefiedMixedEdits)==FALSE),,drop=FALSE]     #  de overbodige edits worden weggehaald 
      ESimplified <-SimplifyNewEdits  ( ESimplified )# verder opschonen van de editset
      LogSimplifiedMixEdits   <- LogSimplifiedEdits (E, CatVarUnconditional ) # maken logfile van aangepaste mixed edits
      LogNewUnconditional <- LogRedundantPartsinMixedEdits(E,CatVarUnconditional )             # maken logfile over de componenten van mixed edits waar altijd aan wordt voldaan       
      }
    }
   
  return(list("E"= ESimplified ,"ReplacedConditional"=LogSimplifiedMixEdits,  "NewUnconditional"= LogNewUnconditional ))
}  

# de hoofdfunctie voor het aanpassen van de conditionele edits.
SimplifyConditionalEdits <- function(E, epsilon=as.mip(E)$epsilon){
  Simplify<-SimplifyComposedMixedEdits(E, epsilon=as.mip(E)$epsilon)
  Replace<-ReplaceConditionalbyUnconditional(E=Simplify$E, epsilon=as.mip(E)$epsilon)
  return(list("E"=Replace$E,"Simplified"=Simplify$Simplifiededits, "RedundantParts"=Simplify$SimplifiededitsRedundant, "ReplacedConditional"=Replace$ReplacedConditional,  "NewUnconditional" = Replace$NewUnconditional))
}


#------functies   voor Stap 4----verwijderen van overbodige edits

# isUnconditionalEditRedundant geeft aan of een onconditionele edit i  redundant is.  
# Uitgangssituatie: een normalised editset, met "<=" en "=" edits.
# Strikte ongelijkheden (<) komen niet voor; voorafgaande aan deze procedure worden zij omgezet in niet-strikte ongelijkheden
# Om uit te zoeken of een "<= edit" redundant is, wordt het het teken van de edit omgedraaid: het teken wordt ">".
# Bovenstaande gebeurt door aanroep van ChangeOperatorToLargerThan
# als dit er toe leidt dat er geen feasible solution is,dan is de betreffende edit redundant.
# Gelijkheidsrestricties worden geschreven als combinatie van een "<=" en een ">=" restrictie; beide worden op redundantie getest.
# ALs beide restricties redundant zijn, is de oorspronkelijke gelijkheidsrestrictie redundant.
isUnconditionalEditRedundant <- function (A, ops, b, i,iscat, epsilon=as.mip(E)$epsilon ){ 
  Ai <-  ChangeOperatorToLargerThan (A, ops, b, i,iscat, epsilon )$A   
  opsi<- ChangeOperatorToLargerThan (A, ops, b, i,iscat, epsilon )$ops  
  bi  <- ChangeOperatorToLargerThan (A, ops, b, i,iscat, epsilon )$b
  p <-   FillMip (Ai, opsi, bi, iscat, epsilon=0.001)
  redundant <- !isFeasible(p)
  if (ops[i] == "=" & redundant==TRUE) {
    Aj <-  ChangeOperatorToSmallerThan (A, ops, b, i,  epsilon )$A  #toetsen van redundantie van het ">=" deel van een gelijkheidsrestrictie"
    opsj<- ChangeOperatorToSmallerThan (A, ops, b, i, epsilon )$ops	
    bj  <- ChangeOperatorToSmallerThan (A, ops, b, i, epsilon )$b	
    p <-   FillMip(Aj, opsj, bj, iscat, epsilon=0.001)
    redundant <- !isFeasible(p) 
  }
  return(redundant)
} 

# isConditionalEditRedundant geeft aan of een conditionele edit i  redundant is.  
# Uitgangssituatie: een conditionele "<=" edit. (<= teken in iF en THEN deel)
# Strikte ongelijkheden (<) komen niet voor; voorafgaande aan deze procedure worden zij omgezet in niet-strikte ongelijkheden
# Gelijkheidsrestricties komen niet voor. Editrules accepteert geen gelijkheden in conditionele restricties.
# Om uit te zoeken of een "<= edit" redundant is, wordt het het teken van alle bijbehorende categoriale edits omgedraaid: "<=" wordt ">" en de categoriale variabelen worden verwijderd. 
#  als dit er toe leidt dat er geen feasible solution is,dan is de betreffende edit redundant.
isConditionalEditRedundant <- function (A, ops, b, i,iscat, epsilon=as.mip(E)$epsilon ){
  Aj <-  ChangeOperatorsMixedEdits(A, ops, b, i,  epsilon )$A
  opsj<- ChangeOperatorsMixedEdits(A, ops, b, i, epsilon )$ops
  bj  <- ChangeOperatorsMixedEdits(A, ops, b, i, epsilon )$b	
  p <-   FillMip(Aj, opsj, bj, iscat, epsilon=0.001)
  redundant <- !isFeasible(p) 
  return(redundant)
}

# isEditRedundant is 1 als een numerieke edit i overbodig is. 
isEditRedundant<-function(E, i, epsilon=as.mip(E)$epsilon) {
  redundant<-FALSE   #initialiseer op FALSE
  iscat <-  isCategoricalVariable(E)  # iscat is 1 voor alle geheeltallige variabelen
  A  <-  AdaptToMip(E)$A # De coefficientenmatrix, operators en rhs worden aangepast zodanig dat deze leesbaar worden voor de solver.
  ops <- AdaptToMip(E)$ops
  b   <- AdaptToMip(E)$rhs
  isMixed <- isMixedEdit(E)
  if  (isNumericalEdit(E)[i]==TRUE) {
    if (isMixed[i]==FALSE) { 
      redundant<-isUnconditionalEditRedundant(A, ops, b, i,iscat, epsilon )
    }     else {
      redundant<-  isConditionalEditRedundant(A, ops, b, i,iscat, epsilon ) 
    } 
  }
  return(redundant)
} 

# isRedundant geeft weer welke numerieke edits overbodig zijn
isRedundant<-function(E, epsilon=as.mip(E)$epsilon) {
  redundant<-rep(FALSE,nrow(E))   #initialiseer op FALSE
  for (i in 1:nrow(E)){    redundant[i] <- isEditRedundant(E,i, epsilon)}
  return(redundant[isNumericalEdit(E)])
} 

# In LogredundantEdits maakt een editset aan met de redundante edits 
LogRedundantEdits <- function(E, isRedundant){
  # EditSelectiontoEditSet omvat de overbodige numerieke edits en alle categoriale edits.
  # de categoriale edits worden allemaal geselecteerd, omdat sommige categoriale edits nodig zijn bij het omzetten van een editmatrix naar een editset .
  isEditSelected  <- ifelse(isCatEdit(E), TRUE, isRedundant )
  RedundantEdits <-  EditMatrixToEditSet(E[isEditSelected,,drop=F])  # omzetten naar editset
  return(RedundantEdits)  
}
  
# RemoveRedundantEdits verwijdert alle overbodige numerieke (puur numerieke en mixed) edits
RemoveRedundantEdits<-function(E,epsilon=as.mip(E)$epsilon){
  Estart <- E
  isredundant<-rep(FALSE,nrow(E))   #initialiseer op FALSE
  LogRedundant <- ""  # initialisatie
  NumberRedundant<-0  #initialisatie
  if (nrow(E)>0) {
    NumberofEdits <- nrow(E)
    NumberRedundant <-0
    for (i in NumberofEdits:1){               # achteraan beginnen....lijkt makkelijker.
      if( isEditRedundant(E,i, epsilon)){   # hier worden overbodige numerieke edits gedetecteerd. 
        NumberRedundant <-NumberRedundant +1
        E <- DeleteEdit(E,i)   # weghalen overbodige numerieke edit
        isredundant[i]<-TRUE   
        E<-SimplifyNewEdits (E)  #na het weghalen van een overbodige numerieke edit kan het zo zijn dat er ook mixed edits overbodig worden. In deze functie wordt bekeken of de edits verder vereenvoudigd kunnen worden.  
      } 
    }
  }  
  if (NumberRedundant >0) {  LogRedundant <- LogRedundantEdits(Estart, isredundant)}  # omzetten naar editset
  return(list("E"=E, "redundant"=LogRedundant))  
} 




#------Aanroep van het geheel 


CleanEdits <- function(E, epsilon= as.mip(E)$epsilon){
  E<-PrepareEdits(E)
  if (isEditsFeasible(E,epsilon)) {
    SubstFixed <- SubstituteFixedValues(E, epsilon)
   # SubstFixed <- DetermineFixedValues(E, epsilon)#
    SimpleConditional <- SimplifyConditionalEdits(SubstFixed$E,epsilon)
    
    RemoveRedundant <- RemoveRedundantEdits(SimpleConditional$E, epsilon)
    E <- EditMatrixToEditSet (RemoveRedundant$E)
    E <- AddFixedValuestoanEditSet  (E, SubstFixed$variables, SubstFixed$values)#
  }  else {
    E <- "NOT FEASIBLE"
    fixvars <- "NOT FEASIBLE"
    fixvalues<- "NOT FEASIBLE"
    redundant <- "NOT FEASIBLE"
    Edits_Simplified_BecauseOfFixedVars<- "NOT FEASIBLE"
    NewEdits_after_imputation_fixedvars <- "NOT FEASIBLE"
    SimplifiedConditional<- "NOT FEASIBLE"
    ReplacedConditional <- "NOT FEASIBLE"
    NewUnconditional<- "NOT FEASIBLE"
    redundant<- "NOT FEASIBLE"
    RedundantParts <- "NOT FEASIBLE"
  }
  return(list("CleanedEdits" =E,"Fixedvariables"=SubstFixed$variables, "Fixedvalues"=SubstFixed$values, "Edits_Simplified_BecauseOfFixedVars"=SubstFixed$OldEditsWithFixedVars,"NewEdits_after_imputation_fixedvars"=SubstFixed$NewEditsWithFixedVars, "SimplifiedConditionalEdits"=  SimpleConditional$Simplified,"SimplifiedConditionalEditsRedundantParts"=SimpleConditional$RedundantParts, "NewUnconditional"=SimpleConditional$NewUnconditional, "ConditionalReplacedbyUnconditional"=SimpleConditional$ReplacedConditional ,"RemovedEdits_BecauseofRedundancy"=RemoveRedundant$redundant )  )}





















  
  

#----------------------------------------------------------VOORBEELDEN van een aanroep van CleanEdits-----------------------------



E <- editfile("set2.txt")  
H<- CleanEdits(E)
HC<- H$CleanedEdits
HCE <- editset(HC[,2])

filename<-paste(path,"Set2Cleaned.txt")
write.csv2(HCE,file=filename, row.names=FALSE, sep=" ", quote=FALSE)

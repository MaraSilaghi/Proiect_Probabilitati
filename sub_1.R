
#a
frepcomgen1 <- function(m, n) {
  #generam x1, x2,..., xn, respectiv y1, y2,..., ym, care vor fi numere de la 1 la n, respectiv m
  X <- 1:n
  Y <- 1:m
  
  #cream matricea cu repartitia comuna, unde generam aleatoriu n*m numere intre 0 si 1
  pi_ij <- matrix(runif(n * m), nrow = n, ncol = m)
  #normalizam valorile astfel incat suma lor sa fie 1
  pi_ij <- pi_ij / sum(pi_ij)
  
  #cream tabelul complet
  tabel <- matrix(NA, nrow = n+2, ncol = m+2)
  tabel[1, 1] <- "X/Y"
  tabel[1, m+2] <- "pi"
  tabel[n+2, 1] <- "qj"
  
  tabel[2:(n+1), 1] <- as.integer(X)   #indicii pentru X si Y
  tabel[1, 2:(m+1)] <- as.integer(Y) 
  
  tabel[(n+2), (m+2)] <- 1    #1 in coltul din dreapta jos
  
  tabel[2:(n+1), 2:(m+1)] <- pi_ij     #adaugam valorile lui pi_ij in tabel
  
  tabel[(n+2), 2:(m+1)] <- apply(pi_ij, 2, sum)  # q sunt sumele coloanelor din pi_ij
  tabel[2:(n+1), (m+2)] <- apply(pi_ij, 1, sum)  # p sunt sumele liniilor din pi_ij
  
  #acum avem tabela completa, dar vrem sa afisam doar un numar suficient de valori
  
  # calculam numarul total de celule fara margini si coltul din dreapta jos
  nr_total_celule <- (n * m) + n + m - 1
  
  
  # calculam numarul de valori NA care trebuie plasate
  nr_valori_NA <- nr_total_celule - (n * m)
  
  # initializam contorul pentru valori NA
  contor_NA <- 0
  
  # cream o lista de pozitii eligibile pentru valori NA - fara prima linie/coloana si fara
  #coltul din dreapta jos
  pozitii_eligibile <- expand.grid(x = 2:(n+2), y = 2:(m+2))
  pozitii_eligibile <- subset(pozitii_eligibile, !(x == n+2 & y == m+2))
  
  
  # plasam valori NA in pozitii aleatorii pana cand atingem nr_valori_NA
  while(contor_NA < nr_valori_NA) {
    # alegem o pozitie aleatorie
    idx <- sample(nrow(pozitii_eligibile), 1)
    pozitie <- pozitii_eligibile[idx, ]
    
    # plasam NA in pozitie daca nu este deja NA
    if (!is.na(tabel[pozitie$x, pozitie$y])) {
      tabel[pozitie$x, pozitie$y] <- NA
      contor_NA <- contor_NA + 1
    }
    
    # eliminam pozitia din lista de pozitii eligibile
    pozitii_eligibile <- pozitii_eligibile[-idx, ]
  }
  
  return(tabel)
}


#b
fcomplrepcom <- function(tabel) {
  # identificam dimensiunile tabelului
  n <- nrow(tabel) - 2
  m <- ncol(tabel) - 2
  
  repeat {
    na_modified <- FALSE
    
    # cautam celulele NA
    for (i in 2:(n+2)) {
      for (j in 2:(m+2)) 
        {
        if (is.na(tabel[i, j])) 
          {
          
          #verificam linia aferenta celulei
          # verificam daca mai exista valori NA pe linie, caz in care ok>1
          ok_linie <- sum(is.na(tabel[i, 2:(m+2)])) == 1
          #daca nu, inseamna ca putem afla valoarea
          if (ok_linie) 
            {
            #daca valoarea e marginala (qj), ea va fi suma elementelor de pe linie
            if (j == m+2) 
              tabel[i, j] <- sum(as.numeric(tabel[i, 2:(m+1)]), na.rm = TRUE)
            #daca valoarea face parte din rep comuna, va fi qj-suma
            else      
              tabel[i, j] <- as.numeric(tabel[i, m+2]) - sum(as.numeric(tabel[i, 2:(m+1)]), na.rm = TRUE)
            na_modified <- TRUE
          } 
          else 
          {
            # verificam daca mai exista valori NA pe coloana
            ok_coloana <- sum(is.na(tabel[2:(n+2), j])) == 1
            #daca nu, inseamna ca putem afla valoarea
            
            if (ok_coloana) {
              #daca valoarea e marginala (pi), ea va fi suma elementelor de pe coloana
              if (i == n+2) 
                tabel[i, j] <- sum(as.numeric(tabel[2:(n+1), j]), na.rm = TRUE)
              
              #daca valoarea face parte din rep comuna, va fi pi-suma
               else  
                tabel[i, j] <- as.numeric(tabel[n+2, j]) - sum(as.numeric(tabel[2:(n+1), j]), na.rm = TRUE)
              
              na_modified <- TRUE
            }
          }
        }
      }
    }
    
    #daca nu s-a modificat tabelul dupa o intreaga parcurgere a sa, am terminat
    if (!na_modified) {
      break
    }
  }
  
  return(tabel)
}


#c
frepmarginal <- function(tabel)  {
  # identificam dimensiunile tabelului
  n <- nrow(tabel) - 2
  m <- ncol(tabel) - 2
  
  Y<-matrix(0, nrow = 2, ncol = m) #repartitie marginala Y
  X<-matrix(0, nrow = 2, ncol = n) #repartitie marginala X
  for(i in 1:m) #construiesc repartitie marginala Y
  {
    Y[1, i] <- tabel[1, i+1] #aici iau fiecare y din repartitie
    Y[2, i] <- tabel[n+2, i+1] #aici iau fiecare q din repartitie
  }
  for(i in 1:n) #construiesc repartitie marginala X
  {
    X[1, i] <- tabel[i+1, 1] #aici iau fiecare x din repartitie
    X[2, i] <- tabel[i+1, m+2] #aici iau fiecare p din repartitie
  }
  print(X) #afisez X
  print(Y) #afisez Y
}


#d
fpropcov <- function(tabel, a, b, c, d){
  # identificam n si m
  n <- nrow(tabel) - 2
  m <- ncol(tabel) - 2
  
  #cov(aX+bY, cX+dY)=a*c*Var(X)+(a*d+b*c)*cov(X,Y)+b*d*Var(Y)
  #cov(X,Y)=E(X*Y)-E(X)*E(Y)
  #Var(X)=E(X^2)-E(X)*E(X)
  
  E_X <- 0 
  #calculam E(X)
  for(i in 1:n)
  {
    E_X <- E_X + as.numeric(tabel[i+1, 1])*as.numeric(tabel[i+1, m+2])
  }
  
  #calculam E(Y)
  E_Y <- 0
  for(i in 1:m)
  {
    E_Y <- E_Y + as.numeric(tabel[1, i+1])*as.numeric(tabel[n+2, i+1])
  }
  
  #calculam E(X^2)
  E_X_p <- 0
  for(i in 1:n)
  {
    E_X_p <- E_X_p + as.numeric(tabel[i+1, 1])*as.numeric(tabel[i+1, 1])*as.numeric(tabel[i+1, m+2])
  }
  
  #calculam E(Y^2)
  E_Y_p <- 0
  for(i in 1:m)
  {
    E_Y_p <- E_Y_p + as.numeric(tabel[1, i+1])*as.numeric(tabel[1, i+1])*as.numeric(tabel[n+2, i+1])
  }
  
  
  #calculam variantele
  Var_X <- E_X_p - E_X*E_X
  Var_Y <- E_Y_p - E_Y*E_Y
  
  #calculam E(X*Y)
  E_X_Y <- 0
  for(i in 1:n)
  {
    for(j in 1:m)
    {
      E_X_Y <- E_X_Y + as.numeric(tabel[1, j+1])*as.numeric(tabel[i+1, 1])*as.numeric(tabel[i+1, j+1])
    }
  }
  
  #calculam covarianta
  Cov_X_Y <- E_X_Y - E_X*E_Y
  
  Cov <- a*c*Var_X+(a*d+b*c)*Cov_X_Y+b*d*Var_Y
  print(Cov)
}


#e
fPcond <- function(tabel, x, y){
  # identificam n si m
  n <- nrow(tabel) - 2
  m <- ncol(tabel) - 2
  
  #facem tabelele aferente X|Y=y, Y|X=x
  XcondY <- matrix(0, nrow = 2, ncol = n)
  YcondX <- matrix(0, nrow = 2, ncol = m)
  
  
  #X|Y=y
  for(i in 1:n)
  {
    #plasam indicii repartitiei pe linia 1
    XcondY[1, i] <- as.numeric(tabel[i+1, 1])
    
    #gasim coloana cu indicele cautat
    for(j in 1:m)
    {
      if(tabel[1, j+1]==y)
      {
        #adaugam valori in repartitia conditionata 
        XcondY[2,i] <- as.numeric(tabel[i+1, j+1])/as.numeric(tabel[n+2, j+1])
      }
    }
  }
  
  
  #Y|X=x
  for(i in 1:m)
  {
    #plasam indicii repartitiei pe linia 1
    YcondX[1, i] <- tabel[1, i+1]
    
    #gasim linia cu indicele cautat
    for(j in 1:n)
    {
      if(tabel[j+1, 1]==x)
      {
        #adaugam valori in repartitia conditionata 
        YcondX[2, i] <- as.numeric(tabel[j+1, i+1])/as.numeric(tabel[j+1, m+2])
      }
    }
  }
  print(XcondY)
  print(YcondX)
}


#f
fPcomun <- function(tabel, condX, condY) {
  # identificam n si m
  n <- nrow(tabel) - 2
  m <- ncol(tabel) - 2
  
  # extragem valorile indicilor X, respectiv Y din tabel
  X_vals <- as.numeric(tabel[2:(n+1), 1])   
  Y_vals <- as.numeric(tabel[1, 2:(m+1)])   
  
  probabilitate <- 0    #aici pastram suma probabilitatilor comune ale caror indici respecta cerinta
  
  #parcurgem tabelul de repartitii comune
  for (i in 2:(n+1)) {
    for (j in 2:(m+1)) {
      # verificam daca indicii celulei curente respecta cerinta
      if (condX(X_vals[i-1]) && condY(Y_vals[j-1])) {
        probabilitate <- probabilitate + as.numeric(tabel[i, j])
      }
    }
  }
  print(probabilitate)
}


#g
fPconditional <- function(tabel, condX, condY) {
  # identificam n si m
  n <- nrow(tabel) - 2
  m <- ncol(tabel) - 2
  
  # extragem valorile indicilor X, respectiv Y din tabel
  X_vals <- as.numeric(tabel[2:(n+1), 1])   
  Y_vals <- as.numeric(tabel[1, 2:(m+1)]) 
  
  prob_total <- 0
  prob_condY <- 0
  
  # Parcurgem tabelul de repartitie comuna = prob_total / prob_condY
  for (i in 2:(n+1)) {
    for (j in 2:(m+1)) {
      val_prob <- as.numeric(tabel[i, j]) # probabilitatea curenta
        if (condY(Y_vals[i-1])) {  
          prob_condY <- prob_condY + val_prob # P(Y > 0.3) - numitor
          if (condX(X_vals[j-1])) {
            prob_total <- prob_total + val_prob # daca se respecta si condX
          }
        }
    }
  }
  
  # Calculam probabilitatea conditionata
  if (prob_condY > 0) {
    prob_conditional <- prob_total / prob_condY
  } else {
    prob_conditional <- NA # pentru a evita impartirea la zero
  }
  
  print(prob_conditional)
}


#h
fverind <- function(repartitie) {
  n <- nrow(tabel) - 2
  m <- ncol(tabel) - 2
  semafor <- 1 #initial, presupunem ca cele doua variabile sunt independente
  for(i in 1:n)
  {
    for(j in 1:m)
    {
      if(as.numeric(repartitie[i+1,j+1])!=(as.numeric(repartitie[i+1,m+2])*as.numeric(repartitie[n+2,j+1])))
        semafor <- 0 #daca pi*qj nu este egal cu produsul din tabel atunci nu sunt independente
    }
  }
  if(semafor==0)
    print("Cele doua variabile nu sunt independente")
  else
    print("Cele doua variabile sunt independente")
}
fvernecor <- function(repartitie) {
  #Calculez covarianta pentru cele doua variabile
  #Cov(X,Y)=E(X*Y)-E(X)*E(Y)
  #daca covarianta este egala cu 0 atunci cele doua variabile sunt necorelate
  n <- nrow(tabel) - 2 #numarul de linii
  m <- ncol(tabel) - 2 #numarul de coloane
  E_X <- 0 #aici calculez E[X]
  for(i in 1:n)
  {
    E_X <- E_X + as.numeric(repartitie[i+1, 1])*as.numeric(repartitie[i+1, m+2])
  }
  E_Y <- 0 #aici calculez E[Y]
  for(i in 1:m)
  {
    E_Y <- E_Y + as.numeric(repartitie[1, i+1])*as.numeric(repartitie[n+2, i+1])
  }
  E_X_Y <- 0 #aici calculez E[X*Y]
  for(i in 1:n)
  {
    for(j in 1:m)
    {
      E_X_Y <- E_X_Y + as.numeric(repartitie[1, j+1])*as.numeric(repartitie[i+1, 1])*as.numeric(repartitie[i+1, j+1])
    }
  }
  #calculez covarianta
  cov <- E_X_Y - E_X * E_Y
  #verific daca este egala cu 0 sau nu 
  if(cov==0)
    print("Variabilele X si Y sunt necorelate")
  else
    print("Variabilele X si Y sunt corelate")
}






 
#apel a
tabel <- frepcomgen1(3, 2)        #nr coloane, nr linii
print(tabel, row.names = FALSE, col.names = FALSE)


#apel b
tabel_completat <- fcomplrepcom(tabel)
print(tabel_completat, row.names = FALSE, col.names = FALSE)


#apel c
frepmarginal(tabel_completat)


#aoel d
fpropcov(tabel_completat, 1, 2, 3, 4)


#apel e
fPcond(tabel_completat, 1, 1)   #Y|X=x, X|Y=y


#apel f
# definim conditiile pentru X si Y
condX <- function(x) {!is.na(x) && x < 1.2 }   #fara conditia de !is.na(x) am eroare
condY <- function(y) {!is.na(y) && y < 1.7 }
fPcomun(tabel_completat, condX, condY)


#apel g
#1) Cov(5X+9,-3Y-2)
fpropcov(tabel_completat, 5, 9, -3, -2)

#2) P(0<X<0.8∣Y>0.3)
condX <- function(x) {!is.na(x) && x > 0 && x < 0.8 }  #fara conditia de !is.na(x) am eroare
condY <- function(y) {!is.na(y) && y > 0.3 }
fPconditional(tabel_completat, condX, condY)


#3) P(X>0.2,Y<1.7)
condX <- function(x) {!is.na(x) && x > 0.2 }   #fara conditia de !is.na(x) am eroare
condY <- function(y) {!is.na(y) && y < 1.7 }
fPcomun(tabel_completat, condX, condY)


#apel h
fverind(tabel_completat)
fvernecor(tabel_completat)
  





##Task 1

Pone=function(n,k,strategy,nreps=10000){
  box=sample(1:n)
  a=0
  if (strategy==1){
    for (i in c(1:nreps)){
      time=0
      number=k
      while (box[number]!=k && time<=(n/2)){
        time=time+1
        number=box[number]
      }
      if (time<=(n/2)){
        a=a+1
      }
      box <- sample(1:n)
    }
    print(a/nreps)
  }
    
  if (strategy==2){
    for (i in c(1:nreps)){
      time=0
      number=sample(n,1)
      while (box[number]!=k && time<=(n/2)){
        time = time+1
        number = box[number]
      }
      if (time<=(n/2)){
        a=a+1
      }
      box <- sample(1:n)
    }
    print(a/nreps)
  }
  
  if (strategy==3){
    for(i in c(1:nreps)){
      number=sample(n,n/2)
      if(k %in% box[number]){
        a=a+1
      }
     box <- sample(1:n) 
    }
    print(a/nreps)
  }
  
  }

Pone(,,2,10000)    
Pone(5,1,2,10000)
Pone(100,1,2,10000)

##Task 2
Pull=function(n,k,strategy,nreps=10000){
  box=sample(1:n)
  c=0
  if (strategy==1){
    for(i in c(1:nreps)){
      time=0
      if(Pone(n,k,1,1)==1){
        time=time+1
      }
      if(time>(n/2)){
        c=c+1
      }
    }
    print(c/nreps)
  }
}
Pull(10,2,1,10000)


#Primitive STD/contact network simulator, intended as proof of
#concept.  Not yet functional....
rnstd<-function(duration,nv,entry=log(1/0.1),exit=log(1/60),exit.inf=log(1/10),pr.male=0.5,pr.inf=0.01,weak.add=log(2/nv),weak.rem=log(1),str.add=log(2/(5*nv)),str.rem=log(1/20),inf.add=-1,inf.rem=1,inf.rate=log(1),concur.add=-3,concur.rem=3,concur.limit=10,verbose=FALSE){
  warning("Not yet functional...\n")
  #Initialize the network
  n<-nv
  net<-network.initialize(n,directed=FALSE)
  net%v%"sex"<-sample(c("M","F"),n,prob=c(pr.male,1-pr.male))
  net%v%"inftime"<-sample(c(Inf,0),n,prob=c(pr.inf,1-pr.inf))
  activate.vertices(net,onset=0)
  act<-rep(T,n)
  nowinf<-rep(F,n)
  #Initialize the main log-rate array (lambda)
  #Rate array has dimensions:
  #  Concurrent ties involved: 0:concur.limit
  #  Status: addition, removal
  #  Strength: weak, strong
  #  Infection: uninfected, infected
  lambda.base<-array(dim=c(concur.limit+1,2,2,2))
  lambda.base[,1,1,]<-weak.add
  lambda.base[,2,1,]<-weak.rem
  lambda.base[,1,2,]<-str.add
  lambda.base[,2,2,]<-str.rem
  lambda.base[,1,,2]<-lambda.base[,1,,2]+inf.add
  lambda.base[,2,,2]<-lambda.base[,2,,2]+inf.rem
  lambda.base[,1,,]<-sweep(lambda.base[,1,,],1, (0:concur.limit)*concur.add,"+")
  lambda.base[,2,,]<-sweep(lambda.base[,2,,],1, (0:concur.limit)*concur.rem,"+")
  lambda.entry<-entry
  #Run the simulation loop
  time<-0
  while(time<duration){
    #Construct current event rate matrices
    lambda.exit<-c(exit,exit.inf)[nowinf[act]]
    possinf<-as.sociomatrix(net%t%time)* outer(nowinf,nowinf,"!=")[act,act]
    lambda.inf<-log(possinf)+inf.rate
    #Draw first event times for each event set
    t.base<-rexp(1,sum(exp(lambda)))
    t.inf<-rexp(1,sum(exp(lambda.inf)))
    t.exit<-rexp(1,sum(exp(lambda.exit)))
    t.entry<-rexp(1,exp(lambda.entry))
    #Find minimal event and update
    t.min<-min(c(t.base,t.inf,t.exit,t.entry))
    minev<-which(t.min==c(t.base,t.inf,t.exit,t.entry))
    if(minev==1){                #Edge change
      #Identify the event (note - time is still time+t.min)
      temp<-apply(lambda,1:4,function(z){rexp(1,z)})
      ev<-which(temp==min(temp),arr.ind=TRUE)
      #Identify the dyad and update
      if(ev[2]==1){  #Edge addition
        if(ev[3]==1){  #Weak tie
        }else{         #Strong tie
        }
      }else{         #Edge removal
        if(ev[3]==1){  #Weak tie
        }else{         #Strong tie
        }
      }
    }else if(minev==2){          #Infection
      #Identify the infected party
      r<-(row(possinf)[upper.tri(possinf)])[(possinf> 0)[upper.tri(possinf)]]
      c<-(col(possinf)[upper.tri(possinf)])[(possinf> 0)[upper.tri(possinf)]]
      v<-sample(1:length(r),1)
      if(nowinf[r[v]])
        v<-c[v]
      else
        v<-r[v]
      #Mark as infected
      nowinf[v]<-TRUE
      set.vertex.attribute(net,"inftime",time+t.min,v=v)
    }else if(minev==3){          #Exit
      #Identify the exiting party
      v<-sample((1:n)[active],1,prob=exp(lambda.exit))
      #Deactivate them (and any associated edges)
      eid<-get.edgeIDs.active(net,v,onset=time+t.min)
      if(length(eid)>0)
        deactivate.edges(net,onset=time+t.min,e=eid)
      deactivate.vertices(net,onset=time+t.min,v=v)
      act[v]<-FALSE
    }else{                       #Entry
      n<-n+1
      add.vertices(net,1,list(sex=sample(c("M","F"),1,prob=c(pr.male, 1-pr.male)),inftime=sample(c(Inf,time+t.min),1,prob=c(pr.inf,1-pr.inf))))
      activate.vertices(net,onset=time+t.min,v=n)
      act<-c(act,TRUE)
    }
    #Advance time
    time<-time+t.min
  }
  #Return the results
  net
}


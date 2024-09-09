initialiseaub <- function(N = 100){
  inds           <- array(data = 0, dim = c(100, 15));
  colnames(inds) <- c("ID","Con.mort","Fun.mort","Wasp.mort","Mate","Sex",
                      "Offspring aubergine","Dead","Mate ID","Mum ID","Dad ID",
                      "xloc", "yloc", "Wing length/mm","application");
  inds[,1]<-1:100;
  inds[,2] <- rbinom(n = dim(inds)[1], size = 1, prob = 0.02)
  inds[,3] <- rbinom(n = dim(inds)[1], size = 1, prob = 0.65) # mortality avg
  inds[,4] <- rbinom(n = dim(inds)[1], size = 1, prob = 0.69)
  inds[,5] <-0
  inds[,6] <- rbinom(n=dim(inds)[1], size=1, prob=0.5);#female(0) male(1)
  inds[,7] <- abs(round(rpois(n = dim(inds)[1],lambda=500), digits = 0))
  inds[,8] <-0
  inds[,9] <-0
  inds[,10] <-0
  inds[,11] <-0
  inds[,12] <-sample(x = 1:5, size = dim(inds)[1], replace = TRUE);
  inds[,13] <- sample(x = 1:5, size = dim(inds)[1], replace = TRUE); # 5 ha 
  inds[,14]<-abs(round(rnorm(n = dim(inds)[1], mean = 23.25, sd = 2.89), 
                       digits = 0))
  inds[,15]<-0
  return(inds)
}

inds <- initialiseaub(N=100)
 View(inds)

# for tomato fecundity mean= 6.647268, sd=3.715)
initialisetom <- function(N = 100){
  inds           <- array(data = 0, dim = c(100, 15));
  colnames(inds) <- c("ID","Con.mort","Fun.mort","Wasp.mort","Mate","Sex",
                      "Offspring tomato","Dead","Mate ID","Mum ID","Dad ID",
                      "xloc", "yloc", "Wing length/mm","application");
  inds[,1]<-1:100;
  inds[,2] <- rbinom(n = dim(inds)[1], size = 1, prob = 0.02)
  inds[,3] <- rbinom(n = dim(inds)[1], size = 1, prob = 0.65)# mortality average from my data
  inds[,4] <- rbinom(n = dim(inds)[1], size = 1, prob = 0.69)
  inds[,5] <-0
  inds[,6] <- rbinom(n=dim(inds)[1], size=1, prob=0.5);#female(0) male(1)
  inds[,7] <- abs(round(rpois(n = dim(inds)[1], lambda = 200), digits = 0))
  inds[,8] <-0
  inds[,9] <-0
  inds[,10] <-0
  inds[,11] <-0
  inds[,12] <-sample(x = 1:5, size = dim(inds)[1], replace = TRUE);
  inds[,13] <- sample(x = 1:5, size = dim(inds)[1], replace = TRUE); # 5 ha in size
  inds[,14]<-abs(round(rnorm(n = dim(inds)[1], mean = 23.25, sd = 2.89), digits = 0))
  inds[,15]<-0
  return(inds)
}

inds <- initialisetom(N=100)
# View(inds)
#add mate function in, males sex= 1, females sex=0
#if a female, mate column=1 and mate ID every female is mated with a random male from column 5.
#if a male offspring columns= 0 

#need to add in if in same location they mate.
  
mate <- function(inds){
  males <- subset(inds, inds[,6]==1, select = 1:15)
  females <- subset(inds, inds[,6]==0, select = 1:15)
  for(i in 1:nrow(females)){
    while(females[i, 9] == 0){
      females[i, 5] <- females[i, 5] + 1
      females[i, 9] <- sample(males[,1], size=1, replace=TRUE)
    }
    for(i in 1:nrow(males)){
      males[i, 7] <- 0
    }
  }
  inds <- rbind(males,females)
  return(inds);
}
  
inds <- mate(inds)

 View(inds)
 


add_offspring <- function(inds)
  {
  N                <- dim(inds)[1]; # Total inds rows (number of individuals)
  traits           <- dim(inds)[2]; # Total inds columns (traits for all inds)
  ID_last          <- max(inds[,1]);
  the_dads         <- which(inds[,6] > 0);
  dad_off          <- sum(inds[the_dads, 7]);
  the_mums         <- which(inds[,7] > 0); # 
  tot_offs         <- sum(inds[the_mums, 7]);
  if(dad_off  > 0){
    stop("Dad's are producing offspring for some reason.");
  }
  if(tot_offs > 1) # this makes a new matrix with the offspring in 
  { 
    new_inds  <- matrix(data = 0, nrow = tot_offs, ncol = traits, byrow = TRUE);
    off_poss  <- 0;
    for(mum_row in the_mums){ # BD: <- Fixed a potential bug here (was 1:N)
      while(inds[mum_row, 7] > 0){
        off_poss <- off_poss + 1;
        if(off_poss > tot_offs){
          stop("Offspring position exceeds table rows for some reason?")  
          
          # Subsample mothers into a new table based on the number of offspring 
          
          #then add offspring only for those females
        }
        
        mum_ID   <- inds[mum_row, 1];
        dad_ID   <- inds[mum_row, 9];
        xloc     <-inds[mum_row,12]
        yloc     <-inds[mum_row,13]
        dad_row <- which(inds[,1]== dad_ID)[1];
        new_inds[off_poss ,1] <- ID_last + 1
        ID_last <- max(new_inds[,1]);
        new_inds[off_poss, 6]  <- rbinom(n = 1, size = 1, prob = 0.5);
        new_inds[off_poss, 5]  <- 0; #offspring are unmated at the start
        
        #new offspring get average of both parents resistance
        
        new_inds[off_poss, 2]  <- abs(0.5 * (inds[mum_row, 2] + 
                                               inds[dad_row, 2]) +
                                        rnorm(n = 1, mean = 0, sd = 0.01));
        new_inds[off_poss, 3]  <- abs(0.5 * (inds[mum_row, 3] + 
                                               inds[dad_row, 3]) + 
                                        rnorm(n = 1, mean = 0, sd = 0.01));
        new_inds[off_poss, 4]  <- abs(0.5 * (inds[mum_row, 4] + 
                                               inds[dad_row, 4]) + 
                                        rnorm(n = 1, mean = 0, sd = 0.01));
        new_inds[off_poss, 7]  <- abs(round(rpois(n = 1 , lambda=500), digits = 0)) #change this for tom
        new_inds[off_poss, 10] <- inds[mum_row, 1];  # Saves mum_ID as in inds
        new_inds[off_poss, 11] <- inds[mum_row, 9]; # Saves dad_ID as in inds
        new_inds[off_poss,12]<- inds[mum_row, 12]
        new_inds[off_poss,13]<- inds[mum_row, 13]    
        inds[mum_row, 7]       <- inds[mum_row, 7] - 1;
       
      }
      
    }
    
    inds[, 8] <- 1 # old inds are dead
    
  }
  
  if(dim(new_inds)[1] > 100){
    
    surv_inds    <- sample(x = 1:dim(new_inds)[1], size = 100, replace = FALSE);
    
    new_inds     <- new_inds[surv_inds, ];
    inds         <- rbind(inds, new_inds);
    
  }
  
  inds[inds[,2] < 0, 2] <- 0;
  inds[inds[,3] < 0, 3] <- 0;
  inds[inds[,4] < 0, 4] <- 0;
  inds[inds[,2] > 1, 2] <- 1;
  inds[inds[,3] > 1, 3] <- 1;
  inds[inds[,4] > 1, 4] <- 1;
  return(inds);
  
}
inds <- add_offspring(inds)
 View(inds)

old_die <- function(inds){
  dead_inds <- inds[inds[,8] == 1];
  inds <- inds[-dead_inds,];
  return(inds);
}
inds <- old_die(inds)
 View(inds)

cont_mort <- function(inds){
  N          <- dim(inds)[1];
  Traits     <- dim(inds)[2];
  cont_dead  <- rbinom(n = N, size = 1, prob = 0.98)
  inds <- inds[-cont_dead,]
  return(inds);
  
}

use_parasitoids <- function(inds){
  N          <- dim(inds)[1];
  para_dead <- rbinom(n = N, size = 1, prob = inds[,4])
  rm <-  which(para_dead == 1)
  inds[,15]<-2
  inds <- inds[-rm,]
  return(inds);
  
}

View(inds)

use_fung <- function(inds){
  N          <- dim(inds)[1];
  fun_dead <- rbinom(n = N, size = 1, prob = inds[,3])
  rm <-  which(fun_dead == 1)
  inds[,15]<-1
  inds <- inds[-rm,]
  return(inds);
  
}



#adding in movement, if get to edge they move back in again

movement <- function(inds, xloc = 12, yloc = 13, xmax = 5, ymax = 5){
  total_inds   <- dim(inds)[1]; # Get the number of individuals in inds
  move_dists   <- c(-1, 0, 1);  # Define the possible distances to move
  x_move       <- sample(x = move_dists, size = total_inds, replace = TRUE);
  y_move       <- sample(x = move_dists, size = total_inds, replace = TRUE);
  inds[, xloc] <- inds[, xloc] + x_move;
  inds[, yloc] <- inds[, yloc] + y_move;
  # =========   The reflecting boundary is added below
  for(i in 1:total_inds){               # For each individual i in the array
    if(inds[i, xloc] > xmax){         # If it moved passed the maximum xloc
      inds[i, xloc] <- xmax - 1;    # Then move it back toward the centre
    }
    if(inds[i, xloc] < 1){            # If it moved below 1 on xloc
      inds[i, xloc] <- 1;           # Move it toward the centre (2)
    }
    if(inds[i, yloc] > ymax){         # If it moved passed the maximum yloc
      inds[i, yloc] <- ymax - 1;    # Then move it back toward the centre
    }
    if(inds[i, yloc] < 1){            # If it moved below 1 on yloc
      inds[i, yloc] <- 1;           # Then move it toward the centre (2)
    }
  } 
  # =========  Now all individuals should stay on the landscape
  return(inds);
}
#inds<-movement(inds)
View(inds)

# in this timestep set up using alternating treatments wasp (if odd number) 
N_init <- 10;

# Treatment: Control, encarsia, fungal, alternating

run_sim <- function(aubergine = TRUE, N_init = 100, treatment = "alternating",
                    time_steps = 10, burn_in = 20){
  ts         <- 0;
  inds_hist  <- NULL;
  if(aubergine == TRUE){
    inds <- initialiseaub(N = N_init);
  }else{
    inds <- initialisetom(N = N_init);
  }
  while(ts < time_steps){
    inds            <- mate(inds);
    inds            <- add_offspring(inds);
    inds            <- old_die(inds);
    inds            <- cont_mort(inds);
    if(treatment == "encarsia" & ts > burn_in){
      inds <- use_parasitoids(inds)
    }
    if(treatment == "fungal" & ts > burn_in){
      inds <- use_fung(inds);
    }
    if(treatment == "alternating" & ts > burn_in){
      if((ts %% 2) == 0){
        inds <- use_parasitoids(inds)
      }else{
        inds <- use_fung(inds);
      }
    }
    ts              <- ts + 1;
    print(ts)
    inds_hist[[ts]] <- inds;
  }
  return(inds_hist);
}


simulations <- function(aubergine = TRUE, N_init = 1000, treatment = "control",
                        time_steps = 200, replicates = 40, burn_in = 20){
  
  rep      <- 1;
  big_list <- NULL;
  while(rep <= replicates){
    sim  <- run_sim(aubergine = aubergine, N_init = N_init, burn_in = burn_in,
                    treatment = treatment, time_steps = time_steps);
    
    tmp  <- NULL;
    LL   <- length(sim);
    for(i in 1:LL){
      inm       <- dim(sim[[i]])[1]
      time_step <- rep(x = i, times = inm);
      nsm       <- cbind(time_step, sim[[i]]);
      tmp       <- rbind(tmp, nsm);
    }
    dtmp      <- dim(tmp)[1];
    replicate <- rep(x = rep, times = dtmp);
    tmp       <- cbind(replicate, tmp);
    big_list  <- rbind(big_list, tmp);
    rep       <- rep + 1;
  }
  return(big_list);
}

### The code runs with this function#
sim_data <- simulations(aubergine = TRUE, N_init = 1000, treatment = "alternating",
                        time_steps = 200, replicates = 20, burn_in = 20);



View(inds)
ind_abund <- array(data = NA, dim = c(10, 3));
for(i in 1:10){
  ind_abund[i, 1] <- i;                      # Save the time step
  ind_abund[i, 2] <- dim(inds_hist[[i]])[1]; # rows in inds_hist[[i]]
  ind_abund[i, 3] <- inds_hist[[i]][1,15] ;  
}
colnames(ind_abund) <- c("time_step", "abundance");
print(ind_abund)



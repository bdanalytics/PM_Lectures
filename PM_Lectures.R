#rm(list=ls())
setwd("~/Documents/Work/Courses/Coursera/process-mining/Projects/PM_Lectures")

petrinet <- function(name, places_df, trans_df, arcs_df) {
    trans <- trans_df[, c("id","name","x","y")]
    places <- places_df[, c("id","name","x","y","M0")]

    #No of places
    p <- nrow(places)
    #No of transitions
    t <- nrow(trans)

    #A p \times t matrix.
    Cin <- matrix(0,t,p)
    Cout <- matrix(0,t,p)

    #Attach correct names to C'tables
    dimnames(Cin) <- list(trans$name,places$name)
    dimnames(Cout) <- list(trans$name,places$name)

    for (row in (1:nrow(arcs_df))) {
        t_out <- match(arcs_df[row, "begin"], trans$name)
        t_in  <- match(arcs_df[row, "end"],   trans$name)
        p_out <- match(arcs_df[row, "begin"], places$name)
        p_in  <- match(arcs_df[row, "end"],   places$name)
        if ((is.na(t_in) & is.na(p_in)) |
            (is.na(t_out) & is.na(p_out)))
            stop("this should not happen")

             if (!is.na(t_in) & !is.na(p_out)) Cin [t_in,  p_out] <- 1
        else if (!is.na(t_out) & !is.na(p_in)) Cout[t_out, p_in ] <- 1
        else stop("this should not happen")
    }

    #Update Matrix (i.e. token transition)
    C <- Cout - Cin

    ##Use initital marking from CPN File and give names.
    M <- places$M0

    #Initial time
    time <- 0

    #Define petri net object using a list construct.
    pn <- list( name=name, trans=trans, places=places,
                Cin=Cin,Cout=Cout,C=C,
                p=p,t=t,M0=M,M=M,time=time)

    class(pn) <- "petrinet"
    #print(pn)
    return(pn)
}

###################################################################
# Generate a list of length equal to the no of transitions indicating
# for each whether it is enabled at the current marking or not.
###################################################################
enabled.transitions <- function(pn) {
    #Repeat Marking for each transition
    Mrep <- matrix(rep(pn$M,pn$t),pn$t,pn$p,byrow=TRUE)
    #Check that M>=Cin
    return(apply( Mrep >= pn$Cin,MARGIN=1,prod))
}

######################################################################
# Plot a Petri net with initial marking.
#
# Params:
#  pn - the petri net to draw
#  draw.enabled - boolean, all enabled transitions are shown as green
######################################################################

plot.petrinet <- function(x,...) {
    #Hack stuff to get from S3 representation to a more readable thing.
    pn <- x
    opts <- list(...)
    #Draw places and transitions
    rx <- range(pn$places$x,pn$trans$x)
    ry <- range(pn$places$y,pn$trans$y)
    xlim = rx + c(-1,1)*diff(rx)/10
    ylim = ry + c(-1,1)*diff(ry)/10
    dx <- diff(xlim)*1/15
    dy <- diff(ylim)*1/25
    #Different radia
    r1 <- 4/5*dx
    r2 <- 4/5*dy
    r3  <- dx/3
    #Draw places and transitions.
    plot(pn$places$x,pn$places$y,cex=10,xlim=xlim,ylim=ylim,axes=FALSE,xlab="",ylab="")
    rect(pn$trans$x-dx,pn$trans$y-dy,pn$trans$x+dx,pn$trans$y+dy)

    ######################################################################
    #Draw arrows by looping over all trans x places pairs.
    ######################################################################
    for (t in 1:nrow(pn$trans)) {
        for (p in 1:nrow(pn$places)) {
            #In arrows: place -> transition
            if (pn$Cin[t,p]>0) {
                #Show enabled transitions - if so then in green (col=3)?
                #col <- opts$draw.enabled * enabled.transitions(pn)[t]*2+1
                if ((pn$places$y[p] - pn$trans$y[t]) > 0) {
                    beg_dy <- 0 - dy; end_dy <- dy
                } else if ((pn$places$y[p] - pn$trans$y[t]) < 0) {
                    beg_dy <- dy; end_dy <- 0 - dy
                } else {
                    beg_dy <- 0; end_dy <- 0
                }
                if ((pn$places$x[p] - pn$trans$x[t]) > 0) {
                    beg_dx <- 0 - dx; end_dx <- dx
                } else if ((pn$places$x[p] - pn$trans$x[t]) < 0) {
                    beg_dx <- dx; end_dx <- 0 - dx
                } else {
                    beg_dx <- 0; end_dx <- 0
                }


                if (enabled.transitions(pn)[t] > 0)
                    arrows(pn$places$x[p]+beg_dx,pn$places$y[p]+2.5*beg_dy,
                           pn$trans$x[t]+0.5*end_dx,pn$trans$y[t]+end_dy,
                           lwd=2, col="green")
                else
                    arrows(pn$places$x[p]+beg_dx,pn$places$y[p]+2.5*beg_dy,
                           pn$trans$x[t]+0.5*end_dx,pn$trans$y[t]+end_dy,
                           lwd=2)
            }
            #Out arrows, i.e. transition -> place
            if (pn$Cout[t,p]>0) {
                if ((pn$places$y[p] - pn$trans$y[t]) < 0) {
                    beg_dy <- 0 - dy; end_dy <- dy
                } else if ((pn$places$y[p] - pn$trans$y[t]) > 0) {
                    beg_dy <- dy; end_dy <- 0 - dy
                } else  {
                    beg_dy <- 0; end_dy <- 0
                }
                if ((pn$places$x[p] - pn$trans$x[t]) < 0) {
                    beg_dx <- 0 - dx; end_dx <- dx
                } else if ((pn$places$x[p] - pn$trans$x[t]) > 0) {
                    beg_dx <- dx; end_dx <- 0 - dx
                } else  {
                    beg_dx <- 0; end_dx <- 0
                }

                arrows(pn$trans$x[t]+0.5*beg_dx,pn$trans$y[t]+beg_dy,
                       pn$places$x[p]+0.5*end_dx,pn$places$y[p]+2.5*end_dy,
                       lwd=2)
            }

            #show weight if greater than one
            cx <- (pn$places$x[p] + pn$trans$x[t])/2
            cy <- (pn$places$y[p] + pn$trans$y[t])/2
            if (pn$Cin[t,p]>1 )  { text(cx,cy,pn$Cin[t,p],cex=1.5,col=4)}
            if (pn$Cout[t,p]>1)  { text(cx,cy,pn$Cout[t,p],cex=1.5,col=4)}

        }
    }

    ######################################################################
    #Draw names
    ######################################################################
    text(pn$places$x,pn$places$y-dy,pn$places$name,col=4)
    text(pn$trans$x,pn$trans$y-dy,pn$trans$name)

    ######################################################################
    #Plot markings
    ######################################################################
    for (i in 1:pn$p) {
        mark <- pn$M[i]
        #If the place is marked then show all marks
        #Draw marks so its centered about the place's centre
        if (mark>0) {
            #Center
            cx <- pn$places$x[i]
            cy <- pn$places$y[i]
            #If there is only one mark its put in the center
            if (mark==1) {
                symbols(x=cx,y=cy,circles=dx/3,inches=FALSE,add=TRUE,bg=2)
            } else if (mark<5) {
                #If there are more than one angles and radius to place mark balls on
                angles <- (0:(mark-1))*2*3.141592/mark
                for (m in 1:mark) {
                    symbols(x=cx+r1*cos(angles[m]),y=cy+r2*sin(angles[m]),circles=r3,inches=FALSE,add=TRUE,bg=2)
                }
            } else {
                ##Too much to draw just print the number
                symbols(x=cx,y=cy,circles=2*r3,inches=FALSE,add=TRUE,bg=2)
                text(cx,cy,mark)
            }
        }
    }

    invisible()
}

######################################################################
# Petri Net simulator. With our without animation.
#
# Params:
#  pn - petri net to work on
#  steps - number of steps to simulate
#  animate - play the token game and show the graphics
#  reset - start with M0 or continue on current marking
#  wait - waiting time to next move * timestep
#  file - logfile , default is stdout.
######################################################################

token.game_save <- function(pn,steps=1e99,animate=TRUE,reset=FALSE,
                       wait=1000000,file="") {
    #The rewind option goes back to the initial marking.
    if (reset) {pn$M <- pn$M0; pn$time = 0}

    #Find the set of enabled transitions.
    isenabled <- enabled.transitions(pn)
    enabled <- (1:pn$t)[isenabled==TRUE]

    #Step counter.
    stepCounter <- 1

    #Write header if reset
    if (reset) {
        names <- paste("\"",paste(pn$places$name,col="\""),sep="")
        cat("time\ttrans\t",names,"\n",file=file,append=FALSE)
    }
    #Loop until there are no more possible transitions
    while (length(enabled)>0 && (stepCounter <= steps)) {
        #If no transition rates then ordinary token
        if (is.null(pn$Lambda)) {
            timestep <- 1
            #select one of the enabled transitions by chance.
            #if length==1 sample doesnt work so we got to branch on it
            if (length(enabled) == 1) {
                seltrans <- enabled
            } else {
                seltrans <- sample(enabled,size=1)
            }
        } else {
            ######################################################################
            ##Stochastic petri net -- possibly with state dependent rates
            ######################################################################

            #Deduce state dependent rates by calculating rate for
            #each enabled transition
            rates <- sapply(enabled,function(i) { pn$Lambda[[i]](pn$M)})

            #Sample waiting time for each
            waits <- rexp(length(enabled),rates)
            #Race against the machine - the fastest wins
            seltrans <- enabled[which.min(waits)]
            timestep <- min(waits)
        }

        #Fire the transition and update marking.
        pn$M <- pn$M + pn$C[seltrans,]
        pn$time <- pn$time + timestep

        #Log info - Debug info.
        cat(formatC(pn$time,format="f"),"\t",seltrans,"\t",pn$M,"\n",file=file,append=TRUE)
        #!(reset & (stepCounter==1)))

        #Plot it.
        if (animate) {plot(pn,draw.enabled=TRUE)}

        #Wait
        for (i in 1:wait*timestep) {}

        #Find the new set of enabled transitions.
        isenabled <- enabled.transitions(pn)
        enabled <- (1:pn$t)[isenabled==TRUE]

        #Did one step
        stepCounter <- stepCounter + 1;
    }
    return(pn)
}

# Lecture 21
## Page 3
lec_21_page_3_pn <- petrinet(name="lec_21_page_3",
        trans_df=data.frame(id=1:3, name=c("rg", "go", "or"),
                            x=c(0, 0, -5), y=c(15, 5, 10)),
        places_df=data.frame(id=1:3, name=c("red", "green", "orange"),
                            x=c(0, 0, 0), y=c(20, 10, 0),
                            M0=c(1, 0, 0)),
        arcs_df=data.frame(
            begin=c("red", "rg",    "green", "go",     "orange", "or"),
            end  =c("rg",  "green", "go",    "orange", "or",     "red")))
plot.petrinet(lec_21_page_3_pn)
junk_pn <- token.game(lec_21_page_3_pn, steps=3, animate=TRUE, reset=TRUE,
                      wait=100, file="")

## Page 5
lec_21_page_5_pn <- petrinet(name="lec_21_page_5",
                             trans_df=data.frame(id=1:3, name=c("rg", "go", "or"),
                                                 x=c(0, 0, -5), y=c(15, 5, 10)),
                             places_df=data.frame(id=1:3, name=c("red", "green", "orange"),
                                                  x=c(0, 0, 0), y=c(20, 10, 0),
                                                  M0=c(2, 0, 0)),
                             arcs_df=data.frame(
                                 begin=c("red", "rg",    "green", "go",     "orange", "or"),
                                 end  =c("rg",  "green", "go",    "orange", "or",     "red")))
plot.petrinet(lec_21_page_5_pn)
junk_pn <- token.game(lec_21_page_5_pn, steps=3, animate=TRUE, reset=TRUE,
                      wait=100, file="")

## Page 9
lec_21_page_9_pn <- petrinet(name="lec_21_page_9",
    trans_df=data.frame(id=1:1, name=c("t1"), x=c(0), y=c(0)),
    places_df=data.frame(id=1:5, name=c("p1", "p2", "p3", "p4", "p5"),
                        x=c(-5, 5, -5, 0, 5), y=c(5, 5, -5, -5, -5),
                        M0=c(4, 3, 1, 3, 3)),
    arcs_df=data.frame(
                    begin=c("p1", "p2", "p3", "t1", "t1", "t1"),
                    end  =c("t1", "t1", "t1", "p3", "p4", "p5")))
plot.petrinet(lec_21_page_9_pn)
junk_pn <- token.game(lec_21_page_9_pn, steps=5, animate=TRUE, reset=TRUE,
                      wait=100, file="")

## Page 10
lec_21_page_10_pn <- petrinet(name="lec_21_page_10",
        trans_df=data.frame(id=1:2, name=c("t1", "t2"), x=c(5, 15), y=c(5, 5)),
        places_df=data.frame(id=1:5, name=c("p1", "p2", "p3", "p4", "p5"),
                                            x=c( 0, 10, 0, 5, 10),
                                            y=c(10, 10, 0, 0,  0),
                                            M0=c(0, 1, 1, 0, 1)),
                             arcs_df=data.frame(
                        begin=c("p1", "p2", "p2", "p3", "t1", "t1", "t1", "t2"),
                        end  =c("t1", "t1", "t2", "t1", "p3", "p4", "p5", "p5")))
plot.petrinet(lec_21_page_10_pn)
junk_pn <- token.game(lec_21_page_10_pn, steps=5, animate=TRUE, reset=TRUE,
                      wait=100, file="")

## Page 11
lec_21_page_11_pn <- petrinet(name="lec_21_page_11",
        trans_df=data.frame(id=1:3, name=c("t1", "t2", "t3"),
                            x=c(0, 5, 10), y=c(5, 5, 5)),
        places_df=data.frame(id=1:6,
                             name=c("p1a", "p2a", "p3a", "p1b", "p2b", "p3b"),
                                                   x=c( 0,  5, 10, 0,  5, 10),
                                                   y=c(10, 10, 10, 0,  0,  0),
                                                   M0=c(1, 1, 1, 1, 1, 1)),
                    arcs_df=data.frame(
                        begin=c("p1a", "p2a", "p3a", "t1",  "t2",  "t3"),
                        end  =c("t1",  "t2",  "t3",  "p1b", "p2b", "p3b")))
plot.petrinet(lec_21_page_11_pn)
junk_pn <- token.game(lec_21_page_11_pn, steps=5, animate=TRUE, reset=TRUE,
                      wait=100, file="")

######################################################################
# Petri Net simulator. With our without animation.
#
# Params:
#  pn - petri net to work on
#  high.priority.trans - if multiple transitions are enabled, which one
#       is a high priority; if this is not one of the enabled transitions
#       then a random enabled transition is fired
#  steps - number of steps to simulate
#  animate - play the token game and show the graphics
#  reset - start with M0 or continue on current marking
#  wait - waiting time to next move * timestep
#  file - logfile , default is stdout.
######################################################################

token.game_old <- function(pn, high.priority.trans=NULL, steps=1e99,
                       animate=TRUE,reset=FALSE,
                            wait=1000000,file="") {
    #The rewind option goes back to the initial marking.
    if (reset) {pn$M <- pn$M0; pn$time = 0}

    #Find the set of enabled transitions.
    isenabled <- enabled.transitions(pn)
    enabled <- (1:pn$t)[isenabled==TRUE]

    # Check if high.priority.trans is one of the enabled transitions
    if (!missing(high.priority.trans)) {
        fire.enabled.trans.ix <- grep(high.priority.trans, pn$trans$name)
        if (length(fire.enabled.trans.ix) == 0) fire.enabled.trans.ix <- 0
    } else fire.enabled.trans.ix <- 0

    #Step counter.
    stepCounter <- 1

    #Write header if reset
    if (reset) {
        names <- paste("\"",paste(pn$places$name,col="\""),sep="")
        cat("time\ttrans\t",names,"\n",file=file,append=FALSE)
    }
    #Loop until there are no more possible transitions
    while (length(enabled)>0 && (stepCounter <= steps)) {
        #If no transition rates then ordinary token
        if (is.null(pn$Lambda)) {
            timestep <- 1
            #select one of the enabled transitions by chance.
            #if length==1 sample doesnt work so we got to branch on it
            if (length(enabled) == 1) {
                seltrans <- enabled
            } else {
                if (fire.enabled.trans.ix == 0) {
                    seltrans <- sample(enabled,size=1)
                } else seltrans <- fire.enabled.trans.ix
                cat(formatC(pn$time,format="f"),
                    "\tmultiple enabled transitions: ", pn$trans$name[enabled],
                    "\tfiring: ", pn$trans$name[seltrans], "\n", append=TRUE)
            }
        } else {
            ######################################################################
            ##Stochastic petri net -- possibly with state dependent rates
            ######################################################################

            #Deduce state dependent rates by calculating rate for
            #each enabled transition
            rates <- sapply(enabled,function(i) { pn$Lambda[[i]](pn$M)})

            #Sample waiting time for each
            waits <- rexp(length(enabled),rates)
            #Race against the machine - the fastest wins
            seltrans <- enabled[which.min(waits)]
            timestep <- min(waits)
        }

        #Fire the transition and update marking.
        pn$M <- pn$M + pn$C[seltrans,]
        pn$time <- pn$time + timestep

        #Log info - Debug info.
        cat("writing to log file: ", formatC(pn$time,format="f"),"\t",
            seltrans,"\t",pn$M,"\n", append=TRUE)
        cat(formatC(pn$time,format="f"),"\t",seltrans,"\t",pn$M,"\n",
            file=file,append=TRUE)
        #!(reset & (stepCounter==1)))

        #Plot it.
        if (animate) {plot(pn,draw.enabled=TRUE)}

        #Wait
        for (i in 1:wait*timestep) {}

        #Find the new set of enabled transitions.
        isenabled <- enabled.transitions(pn)
        enabled <- (1:pn$t)[isenabled==TRUE]

        #Did one step
        stepCounter <- stepCounter + 1;
    }
    return(pn)
}


######################################################################
#Estimate transition rate matrix of a continous time markov chain.
#
#Parameters:
# times - time of change from one state to another
# states - sequence of states of the embedded chain
######################################################################
ctmc.estimate.trm <- function(times,states) {
    #Count total time spent in each state
    wait <- diff(times)
    gamma <- as.numeric(tapply(wait,states[-length(states)],sum))

    #How many different states?
    k <- length(unique(states))

    #count number of jumps from state i to state j
    n <- table(states[-length(states)],states[-1])
    #lambda_ij = n_ij / gamma_i, i.e. divide gamma column wise.
    Lambda <- n / matrix(gamma,k,k)
    #Ensure row sum is 0 in each, i.e. fix diagonal.
    diag(Lambda) <- -apply(Lambda,1,sum)

    #Done
    return(Lambda)
}

######################################################################
#Calculate stationary distribution of a CTMC given its
#transition rate matrix.
#
#Params:
# Lambda - Transition Rate Matrix.
######################################################################
ctmc.stationary <- function(Lambda) {
    #Use calculation directly on Lambda - alternative: using the embedded.
    #Problem: Lambda is singular, constraint of \sum pi  = 1 has to be
    #builtin.
    U <- t(Lambda)
    U[nrow(U),] <- 1
    pi <- solve(U,c(rep(0,nrow(U)-1),1))
    names(pi) <- dimnames(U)[[1]]

    return(pi)
}

######################################################################
# Analyze the log file of a Petri-Net simulation
#
# Params:
#  pn - Obj. of class petrinet to be anaylsed.
#  file - The log file - typically generated by token.game
#  burnin - In case the initial burnin samples should be skipped.
#  stationary - Deduce stationary distribution from empirical state space
#               in case state space is large this can be quite heavy!!
#
# Returns:
#  the entire log file.
######################################################################

analyze.petrisim_old <- function(pn,file="log.txt",burnin=0,stationary=FALSE) {
    #Analyze log files.
    log <- read.table(paste(file,sep=""),header=TRUE)
    #Remove burnin to steady state.
    if (burnin>0) {log <- log[-c(1:burnin),]}

    #No of sims
    n <- nrow(log)
    #Col index of places
    places <- 3:(3+pn$p-1)

    ##Find all possible markings in the log file. I.e.
    ##convert places log to strings using paste by surfing the rows.
    M <- apply(log[,places],MARGIN=1,paste,sep="",collapse="|")
    M <- factor(M)

    # List all unique markings (states)
    markings_df <- data.frame(marking_id=1:(length(levels(M))+1))
    markings_df[, pn$places$name] <- NA
    markings_df[1, pn$places$name] <- pn$M0
    for (marking_ix in 1:length(levels(M)))
        markings_df[marking_ix+1, pn$places$name] <-
            as.numeric(unlist(strsplit(levels(M)[marking_ix], "[|]")))
    #print(markings_df)

    #Max number of tokens in the system
    maxTok <- max(log[,places])

    ##Make a ts-plot of no. of tokens at each place over time
    matplot(log$time,log[,places],type="h",main="No. of Tokens over time",
            ylab="No of tokens @ place",xlab="time")
    legend(3/4*max(log$time),maxTok,dimnames(log)[[2]][places],lty=1,col=1:length(places))

    #Running mean of.
    matplot(log$time,apply(log[,places],2,cumsum)/(1:n),type="l",
            lty=2,main="Running means",xlab="t",ylab="Running mean of no. of tokens")

    legend(3/4*max(log$time),maxTok,dimnames(log)[[2]][places],lty=1,col=1:length(places))

    ##Mean number of tokens.
    tokentab <- apply(log[,places],2,mean)
    ##Mean number of firings.
    transtab <- table(log$trans)/length(log$trans)
    names(transtab) <- pn$trans$name

    #show summary?
    print("Unique markings:")
    print(markings_df)
    print("Mean no. of tokens of embedded chain")
    print(tokentab)
    print("Distribution of firings.")
    print(transtab)
    par(mfcol=c(2,1))
    barplot(tokentab,main="Mean number of tokens at place.")
    barplot(t(transtab),main="Empirical distribution of firing transition")
    par(mfcol=c(1,1))


    if (stationary) {
        #Estimate transition rate matrix
        Lambda.pn <- ctmc.estimate.trm(log$time,M)
        #Based on this find approximate (all states are not observed)
        #stationary distribution.
        pi.pn   <- ctmc.stationary(Lambda.pn)
        #Show results.
        #print(Lambda.pn)
        print("Stationary distribution:")
        print(pi.pn)
        barplot(pi.pn,type="h",cex.names=0.6,las=2)
    } else {
        pi.pn <- NULL
        Lambda.pn <- NULL
    }

    invisible(list(log=log,pi=pi.pn,Lambda=Lambda.pn))
}

######################################################################
# Depth first search of Petri-Net simulation
#
# Params:
#   pn - Obj. of class petrinet to be analyzed.
#   indent - diagnostics indentation
#   file - The log file - typically generated by token.game
#
# Returns:
#   the entire log file
######################################################################
dfs.petrisim_old <- function(pn, indent=0, file="log.txt") {
    lvl_pn <- pn
    lvl_enabled_trans <- enabled.transitions(lvl_pn)
    if (sum(lvl_enabled_trans) == 0) {
        # reached a leaf
        invisible(lvl_pn)
    } else if (sum(lvl_enabled_trans) == 1) {
        while(sum(lvl_enabled_trans) == 1) {
            lvl_pn <- token.game(lvl_pn, steps=1, animate=TRUE, reset=FALSE,
                                 wait=100, file=file)
            lvl_enabled_trans <- enabled.transitions(lvl_pn)
        }
        lvl_pn <- dfs.petrisim(lvl_pn, indent+1, file)
        invisible(lvl_pn)
    } else {
        for (lvl_trans_ix in 1:length(lvl_enabled_trans)) {
            if (lvl_enabled_trans[lvl_trans_ix]) {
                indent_prefix <- paste0(rep("    ", indent), collapse="")
#                 print(sprintf("%slvl_trans: %s; i am here", indent_prefix,
#                               lvl_pn$trans$name[lvl_trans_ix]))
                nxt_pn <-
                    token.game(lvl_pn,
                              high.priority.trans=lvl_pn$trans$name[lvl_trans_ix],
                        steps=1, animate=TRUE, reset=FALSE,  wait=100, file=file)
                nxt_pn <- dfs.petrisim(nxt_pn, indent+1, file)
            }
        }
        invisible(nxt_pn)
    }
}

## Page 12
lec_21_page_12_pn <- petrinet(name="lec_21_page_12",
                            trans_df=data.frame(id=1:3, name=c("t1", "t2", "t3"),
                                                  x=c(0, 5, 10), y=c(5, 5, 5),
                                                  stringsAsFactors=FALSE),
                            places_df=data.frame(id=1:6,
                                name=c("p1a", "p2a", "p3a", "p1b", "p2b", "p3b"),
                                                   x=c( 0,  5, 10, 0,  5, 10),
                                                   y=c(10, 10, 10, 0,  0,  0),
                                                   M0=c(1, 1, 1, 0, 0, 0),
                                                   stringsAsFactors=FALSE),
                            arcs_df=data.frame(
                                begin=c("p1a", "p2a", "p3a", "t1",  "t2",  "t3"),
                                end  =c("t1",  "t2",  "t3",  "p1b", "p2b", "p3b"),
                                  stringsAsFactors=FALSE))
plot.petrinet(lec_21_page_12_pn)
log_filename <- "lec_21_page_12_pn.log"
junk_pn <-
    token.game(lec_21_page_12_pn, steps=0, animate=TRUE,
               reset=TRUE, wait=100, file=log_filename)
junk_nxt_pn <- dfs.petrisim(lec_21_page_12_pn, indent=0, file=log_filename)
analyze.petrisim(pn=lec_21_page_12_pn, file=log_filename, burnin=0,
                 stationary=FALSE)

## Page 14
lec_21_page_14_pn <-
    petrinet(name="lec_21_page_14",
        trans_df=data.frame(id=1:2, name=c("t1", "t2"), x=c(5, 15), y=c(5, 5),
                            stringsAsFactors=FALSE),
        places_df=data.frame(id=1:5, name=c("p1", "p2", "p3", "p4", "p5"),
                                                   x=c( 0, 10, 0, 5, 10),
                                                   y=c(10, 10, 0, 0,  0),
                                                   M0=c(2, 1, 1, 1, 1),
                             stringsAsFactors=FALSE),
        arcs_df=data.frame(
                        begin=c("p1", "p2", "p2", "p3", "t1", "t1", "t1", "t2"),
                        end  =c("t1", "t1", "t2", "t1", "p3", "p4", "p5", "p5"),
                        stringsAsFactors=FALSE))
plot.petrinet(lec_21_page_14_pn)
log_filename <- "lec_21_page_14_pn.log"
junk_pn <-
    token.game(lec_21_page_14_pn, steps=0, animate=TRUE,
               reset=TRUE, wait=100, file=log_filename)
junk_nxt_pn <- dfs.petrisim(lec_21_page_14_pn, indent=0, file=log_filename)
analyze.petrisim(pn=lec_21_page_14_pn, file=log_filename, burnin=0,
                 stationary=FALSE)

######################################################################
# Analyze the log file of a Petri-Net simulation
#
# Params:
#  pn - Obj. of class petrinet to be anaylsed.
#  file - The log file - typically generated by token.game
#  burnin - In case the initial burnin samples should be skipped.
#  stationary - Deduce stationary distribution from empirical state space
#               in case state space is large this can be quite heavy!!
#
# Returns:
#  the entire log file.
######################################################################
analyze.petrisim <- function(pn,file="log.txt",burnin=0,stationary=FALSE) {
    #Analyze log files.
    log <- read.table(paste(file,sep=""),header=TRUE)
    #Remove burnin to steady state.
    if (burnin>0) {log <- log[-c(1:burnin),]}

    #No of sims
    n <- nrow(log)
    #Col index of places
    places <- 3:(3+pn$p-1)

    ##Find all possible markings in the log file. I.e.
    ##convert places log to strings using paste by surfing the rows.
    M <- apply(log[,places],MARGIN=1,paste,sep="",collapse="|")
    M <- factor(M)

    # List all unique markings (states)
    markings_df <- data.frame(marking_id=1:(length(levels(M))+1))
    markings_df[, pn$places$name] <- NA
    markings_df[1, pn$places$name] <- pn$M0
    for (marking_ix in 1:length(levels(M)))
        markings_df[marking_ix+1, pn$places$name] <-
        as.numeric(unlist(strsplit(levels(M)[marking_ix], "[|]")))
    markings_df$initial <- c(1, rep(0, nrow(markings_df)-1))
    markings_df$final <- sapply(1:nrow(markings_df),
                function(row_ix) {
                    tmp_pn <- pn
                    tmp_pn$M <- as.numeric(markings_df[row_ix, pn$places$name])
                    tmp_pn$M0 <- tmp_pn$M
                    return(ifelse(sum(enabled.transitions(tmp_pn)) > 0, 0, 1))})
    #print(markings_df)

    #Max number of tokens in the system
    maxTok <- max(log[,places])

    ##Make a ts-plot of no. of tokens at each place over time
    matplot(log$time,log[,places],type="h",main="No. of Tokens over time",
            ylab="No of tokens @ place",xlab="time")
    legend(3/4*max(log$time),maxTok,dimnames(log)[[2]][places],lty=1,col=1:length(places))

    #Running mean of.
    matplot(log$time,apply(log[,places],2,cumsum)/(1:n),type="l",
            lty=2,main="Running means",xlab="t",ylab="Running mean of no. of tokens")

    legend(3/4*max(log$time),maxTok,dimnames(log)[[2]][places],lty=1,col=1:length(places))

    ##Mean number of tokens.
    tokentab <- apply(log[,places],2,mean)
    ##Mean number of firings.
    transtab <- table(log$trans)/length(log$trans)
    names(transtab) <- pn$trans$name

    #show summary?
    print("Unique markings:")
    print(markings_df)
    print("Mean no. of tokens of embedded chain")
    print(tokentab)
    print("Distribution of firings.")
    print(transtab)
    par(mfcol=c(2,1))
    barplot(tokentab,main="Mean number of tokens at place.")
    barplot(t(transtab),main="Empirical distribution of firing transition")
    par(mfcol=c(1,1))


    if (stationary) {
        #Estimate transition rate matrix
        Lambda.pn <- ctmc.estimate.trm(log$time,M)
        #Based on this find approximate (all states are not observed)
        #stationary distribution.
        pi.pn   <- ctmc.stationary(Lambda.pn)
        #Show results.
        #print(Lambda.pn)
        print("Stationary distribution:")
        print(pi.pn)
        barplot(pi.pn,type="h",cex.names=0.6,las=2)
    } else {
        pi.pn <- NULL
        Lambda.pn <- NULL
    }

    invisible(list(log=log,pi=pi.pn,Lambda=Lambda.pn))
}

## page_15
lec_21_page_15_pn <-
    petrinet(name="lec_21_page_15",
            trans_df=data.frame(id=1:2, name=c("t1", "t2"), x=c(0, 10), y=c(5, 5),
                                 stringsAsFactors=FALSE),
            places_df=data.frame(id=1:3, name=c("p1", "p2", "p3"),
                                  x=c( 5, 0, 5),
                                  y=c(10, 0, 0),
                                  M0=c(3, 0, 0),
                                  stringsAsFactors=FALSE),
            arcs_df=data.frame(
                 begin=c("p1", "p1", "t1", "t1", "t2"),
                 end  =c("t1", "t2", "p2", "p3", "p3"),
                 stringsAsFactors=FALSE))
plot.petrinet(lec_21_page_15_pn)
log_filename <- "lec_21_page_15_pn.log"
junk_pn <-
    token.game(lec_21_page_15_pn, steps=0, animate=TRUE,
               reset=TRUE, wait=100, file=log_filename)
junk_nxt_pn <- dfs.petrisim(lec_21_page_15_pn, indent=0, file=log_filename)
analyze.petrisim(pn=lec_21_page_15_pn, file=log_filename, burnin=0,
                 stationary=FALSE)

## page_16
lec_21_page_16_pn <-
    petrinet(name="lec_21_page_16",
            trans_df=data.frame(id=1:2, name=c("t1", "t2"), x=c(0, 10), y=c(5, 5),
                                 stringsAsFactors=FALSE),
        places_df=data.frame(id=1:6, name=c("p1", "p2", "p3", "p4", "p5", "p6"),
                                  x=c( 0,  5, 10, 0, 5, 10),
                                  y=c(10, 10, 10, 0, 0,  0),
                                  M0=c(1, 4, 2, 0, 0, 0),
                                  stringsAsFactors=FALSE),
             arcs_df=data.frame(
                 begin=c("p1", "p2", "p2", "p3", "t1", "t1", "t2", "t2"),
                 end  =c("t1", "t1", "t2", "t2", "p4", "p5", "p5", "p6"),
                 stringsAsFactors=FALSE))
plot.petrinet(lec_21_page_16_pn)
log_filename <- "lec_21_page_16_pn.log"
junk_pn <-
    token.game(lec_21_page_16_pn, steps=0, animate=TRUE,
               reset=TRUE, wait=100, file=log_filename)
junk_nxt_pn <- dfs.petrisim(lec_21_page_16_pn, indent=0, file=log_filename)
analyze.petrisim(pn=lec_21_page_16_pn, file=log_filename, burnin=0,
                 stationary=FALSE)

## page_17
lec_21_page_17_pn <-
    petrinet(name="lec_21_page_17",
            trans_df=data.frame(id=1:2, name=c("t1", "t2"), x=c(0, 10), y=c(5, 5),
                                 stringsAsFactors=FALSE),
        places_df=data.frame(id=1:6, name=c("p1", "p2", "p3", "p4", "p5", "p6"),
                                  x=c( 0,  5, 10, 0, 5, 10),
                                  y=c(10, 10, 10, 0, 0,  0),
                                  M0=c(2, 3, 2, 0, 0, 0),
                                  stringsAsFactors=FALSE),
             arcs_df=data.frame(
                 begin=c("p1", "p2", "p2", "p3", "t1", "t1", "t2", "t2"),
                 end  =c("t1", "t1", "t2", "t2", "p4", "p5", "p5", "p6"),
                 stringsAsFactors=FALSE))
plot.petrinet(lec_21_page_17_pn)
log_filename <- "lec_21_page_17_pn.log"
junk_pn <-
    token.game(lec_21_page_17_pn, steps=0, animate=TRUE,
               reset=TRUE, wait=100, file=log_filename)
junk_nxt_pn <- dfs.petrisim(lec_21_page_17_pn, indent=0, file=log_filename)
analyze.petrisim(pn=lec_21_page_17_pn, file=log_filename, burnin=0,
                 stationary=FALSE)

######################################################################
# Petri Net simulator. With our without animation.
#
# Params:
#  pn - petri net to work on
#  high.priority.trans - if multiple transitions are enabled, which one
#       is a high priority; if this is not one of the enabled transitions
#       then a random enabled transition is fired
#  steps - number of steps to simulate
#  animate - play the token game and show the graphics
#  reset - start with M0 or continue on current marking
#  wait - waiting time to next move * timestep
#  file - logfile , default is stdout.
######################################################################

token.game <- function(pn, high.priority.trans=NULL, steps=1e99,
                           animate=TRUE,reset=FALSE,
                           wait=1000000,file="") {
    #The rewind option goes back to the initial marking.
    if (reset) {pn$M <- pn$M0; pn$time = 0}

    #Find the set of enabled transitions.
    isenabled <- enabled.transitions(pn)
    enabled <- (1:pn$t)[isenabled==TRUE]

    # Check if high.priority.trans is one of the enabled transitions
    if (!missing(high.priority.trans)) {
        fire.enabled.trans.ix <- grep(high.priority.trans, pn$trans$name)
        if (length(fire.enabled.trans.ix) == 0) fire.enabled.trans.ix <- 0
    } else fire.enabled.trans.ix <- 0

    #Step counter.
    stepCounter <- 1

    #Write header if reset
    if (reset) {
        names <- paste("\"",paste(pn$places$name,col="\""),sep="")
        cat("time\ttrans\t",names,"\n",file=file,append=FALSE)
    }
    #Loop until there are no more possible transitions
    while (length(enabled)>0 && (stepCounter <= steps)) {
        #If no transition rates then ordinary token
        if (is.null(pn$Lambda)) {
            timestep <- 1
            #select one of the enabled transitions by chance.
            #if length==1 sample doesnt work so we got to branch on it
            if (length(enabled) == 1) {
                seltrans <- enabled
            } else {
                if (fire.enabled.trans.ix == 0) {
                    seltrans <- sample(enabled,size=1)
                } else seltrans <- fire.enabled.trans.ix
                cat(formatC(pn$time,format="f"),
                    "\tmultiple enabled transitions: ", pn$trans$name[enabled],
                    "\tfiring: ", pn$trans$name[seltrans], "\n", append=TRUE)
            }
        } else {
            #####################################################################
            ##Stochastic petri net -- possibly with state dependent rates
            #####################################################################

            #Deduce state dependent rates by calculating rate for
            #each enabled transition
            rates <- sapply(enabled,function(i) { pn$Lambda[[i]](pn$M)})

            #Sample waiting time for each
            waits <- rexp(length(enabled),rates)
            #Race against the machine - the fastest wins
            seltrans <- enabled[which.min(waits)]
            timestep <- min(waits)
        }

        #Fire the transition and update marking.
        pn$M <- pn$M + pn$C[seltrans,]
        pn$time <- pn$time + timestep

        #Log info - Debug info.
#         cat("writing to log file: ", formatC(pn$time,format="f"),"\t",
#             seltrans,"\t",pn$M,"\n", append=TRUE)
        cat(formatC(pn$time,format="f"),"\t",seltrans,"\t",pn$M,"\n",
            file=file,append=TRUE)
        #!(reset & (stepCounter==1)))

        #Plot it.
        if (animate) {plot(pn,draw.enabled=TRUE)}

        #Wait
        for (i in 1:wait*timestep) {}

        #Find the new set of enabled transitions.
        isenabled <- enabled.transitions(pn)
        enabled <- (1:pn$t)[isenabled==TRUE]

        #Did one step
        stepCounter <- stepCounter + 1;
    }
    return(pn)
}

######################################################################
# Depth first search of Petri-Net simulation
#
# Params:
#   pn - Obj. of class petrinet to be analyzed.
#   indent - diagnostics indentation
#   maxsteps - guard against infinite recursive loop
#       uses dfs.petrisim_steps in parent environment (very RUDE ?)
#   file - The log file - typically generated by token.game
#
# Returns:
#   a petrinet with leaf marking ?
######################################################################
dfs.petrisim <- function(pn, indent=0, maxsteps=50, file) {
    # guard against infinite recursive loop
    if ((indent > 5) | (dfs.petrisim_steps >= maxsteps)) {
        warning("stopping: might be in infinite recursive loop")
#         stop()
#         invisible(pn)
        tmp_pn <- pn
        tmp_pn$M <- rep(0, nrow(pn$places))
        tmp_pn$M0 <- tmp_pn$M
        invisible(tmp_pn)
#         invisible(NULL)
    }

    lvl_pn <- pn
    lvl_enabled_trans <- enabled.transitions(lvl_pn)
    if (sum(lvl_enabled_trans) == 0) {
        # reached a leaf
        invisible(lvl_pn)
    } else if (sum(lvl_enabled_trans) == 1) {
        while(sum(lvl_enabled_trans) == 1) {
            if ((indent > 5) | (dfs.petrisim_steps >= maxsteps)) {
                warning("stopping: might be in infinite recursive loop")
                stop()
                #         tmp_pn <- pn
                #         tmp_pn$M <- rep(0, nrow(pn$places))
                #         tmp_pn$M0 <- tmp_pn$M
                #         invisible(tmp_pn)
                invisible(lvl_pn)
            }

            dfs.petrisim_steps <<- dfs.petrisim_steps + 1
            lvl_pn <- token.game(lvl_pn, steps=1, animate=TRUE, reset=FALSE,
                                 wait=100, file=file)
            lvl_enabled_trans <- enabled.transitions(lvl_pn)
        }
        lvl_pn <- dfs.petrisim(lvl_pn, indent=indent+1, file=file)
        invisible(lvl_pn)
    } else {
        for (lvl_trans_ix in 1:length(lvl_enabled_trans)) {
            if (lvl_enabled_trans[lvl_trans_ix]) {
                dfs.petrisim_steps <<- dfs.petrisim_steps + 1
                indent_prefix <- paste0(rep("    ", indent), collapse="")
                #       print(sprintf("%slvl_trans: %s; i am here", indent_prefix,
                #                               lvl_pn$trans$name[lvl_trans_ix]))
                nxt_pn <-
                    token.game(lvl_pn,
                            high.priority.trans=lvl_pn$trans$name[lvl_trans_ix],
                        steps=1, animate=TRUE, reset=FALSE,  wait=100, file=file)
                nxt_pn <- dfs.petrisim(nxt_pn, indent=indent+1, file=file)
            }
        }
        invisible(nxt_pn)
    }
}

## page_19_1
lec_21_page_19_1_pn <- petrinet(name="lec_21_page_19_1",
    trans_df=data.frame(id=1:6, name=c("rg1", "go1", "or1", "rg2", "go2", "or2"),
                                                    x=c( 5, 5,  0, 15, 15, 20),
                                                    y=c(15, 5, 10, 15,  5, 10),
                                                    stringsAsFactors=FALSE),
        places_df=data.frame(id=1:6, name=c("r1", "g1", "o1", "r2", "g2", "o2"),
                                                     x=c( 5,  5, 5, 15, 15, 15),
                                                     y=c(20, 10, 0, 20, 10,  0),
                                                     M0=c(1, 0, 0, 1, 0, 0),
                                                     stringsAsFactors=FALSE),
        arcs_df=data.frame(
                            begin=c("r1",  "rg1", "g1",  "go1", "o1",  "or1",
                                    "r2",  "rg2", "g2",  "go2", "o2",  "or2"),
                            end  =c("rg1", "g1",  "go1", "o1",  "or1", "r1",
                                    "rg2", "g2",  "go2", "o2",  "or2", "r2"),
                                    stringsAsFactors=FALSE))
plot.petrinet(lec_21_page_19_1_pn)
log_filename <- "lec_21_page_19_1_pn.log"; dfs.petrisim_steps <- 0
junk_pn <-
    token.game(lec_21_page_19_1_pn, steps=0, animate=TRUE,
               reset=TRUE, wait=100, file=log_filename)
#junk_nxt_pn <- dfs.petrisim(lec_21_page_19_1_pn, indent=0, file=log_filename)
junk_nxt_pn <-
    token.game(lec_21_page_19_1_pn, steps=50, animate=TRUE,
               reset=FALSE, wait=100, file=log_filename)
analyze.petrisim(pn=lec_21_page_19_1_pn, file=log_filename, burnin=0,
                 stationary=FALSE)

## page_19_2
lec_21_page_19_2_pn <- petrinet(name="lec_21_page_19_2",
                            trans_df=data.frame(id=1:3, name=c("rg", "go", "or"),
                                                 x=c(0, 0, -5), y=c(15, 5, 10),
                                                stringsAsFactors=FALSE),
                places_df=data.frame(id=1:3, name=c("red", "green", "orange"),
                                                  x=c(0, 0, 0), y=c(20, 10, 0),
                                                  M0=c(2, 0, 0),
                                     stringsAsFactors=FALSE),
                             arcs_df=data.frame(
                    begin=c("red", "rg",    "green", "go",     "orange", "or"),
                    end  =c("rg",  "green", "go",    "orange", "or",     "red"),
                    stringsAsFactors=FALSE))
plot.petrinet(lec_21_page_19_2_pn)
log_filename <- "lec_21_page_19_2_pn.log"; dfs.petrisim_steps <- 0
junk_pn <-
    token.game(lec_21_page_19_2_pn, steps=0, animate=TRUE,
               reset=TRUE, wait=100, file=log_filename)
#junk_nxt_pn <- dfs.petrisim(lec_21_page_19_2_pn, indent=0, file=log_filename)
junk_nxt_pn <-
    token.game(lec_21_page_19_2_pn, steps=50, animate=TRUE,
               reset=FALSE, wait=100, file=log_filename)
analyze.petrisim(pn=lec_21_page_19_2_pn, file=log_filename, burnin=0,
                 stationary=FALSE)

## page_22
lec_21_page_22_pn <- petrinet(name="lec_21_page_22",
    trans_df=data.frame(id=1:6, name=c("rg1", "go1", "or1", "rg2", "go2", "or2"),
                                                    x=c( 5, 5,  0, 15, 15, 20),
                                                    y=c(15, 5, 10, 15,  5, 10),
                                                    stringsAsFactors=FALSE),
    places_df=data.frame(id=1:7, name=c("r1", "g1", "o1", "r2", "g2", "o2", "X"),
                                                x=c( 5,  5, 5, 15, 15, 15, 10),
                                                y=c(20, 10, 0, 20, 10,  0, 25),
                                                     M0=c(1, 0, 0, 1, 0, 0, 1),
                                                     stringsAsFactors=FALSE),
    arcs_df=data.frame(
                            begin=c("r1",  "rg1", "g1",  "go1", "o1",  "or1",
                                    "r2",  "rg2", "g2",  "go2", "o2",  "or2",
                                    "or1", "or2", "X",   "X"),
                            end  =c("rg1", "g1",  "go1", "o1",  "or1", "r1",
                                    "rg2", "g2",  "go2", "o2",  "or2", "r2",
                                    "X",   "X",   "rg1", "rg2"),
                                    stringsAsFactors=FALSE))
plot.petrinet(lec_21_page_22_pn)
log_filename <- "lec_21_page_22_pn.log"; dfs.petrisim_steps <- 0
junk_pn <-
    token.game(lec_21_page_22_pn, steps=0, animate=TRUE,
               reset=TRUE, wait=100, file=log_filename)
#junk_nxt_pn <- dfs.petrisim(lec_21_page_22_pn, indent=0, file=log_filename)
junk_nxt_pn <-
    token.game(lec_21_page_22_pn, steps=50, animate=TRUE,
               reset=FALSE, wait=100, file=log_filename)
analyze.petrisim(pn=lec_21_page_22_pn, file=log_filename, burnin=0,
                 stationary=FALSE)

## page_23
lec_21_page_23_pn <- petrinet(name="lec_21_page_23",
    trans_df=data.frame(id=1:6, name=c("rg1", "go1", "or1", "rg2", "go2", "or2"),
                                                  x=c( 5, 5,  0, 20, 20, 25),
                                                  y=c(15, 5, 10, 15,  5, 10),
                                                  stringsAsFactors=FALSE),
    places_df=data.frame(id=1:8,
                        name=c("r1", "g1", "o1", "r2", "g2", "o2", "X", "Y"),
                                            x=c( 5,  5, 5, 20, 20, 20, 10, 15),
                                            y=c(20, 10, 0, 20, 10,  0, 25, 25),
                                                   M0=c(1, 0, 0, 1, 0, 0, 1, 0),
                                                   stringsAsFactors=FALSE),
    arcs_df=data.frame(
                                begin=c("r1",  "rg1", "g1",  "go1", "o1",  "or1",
                                        "r2",  "rg2", "g2",  "go2", "o2",  "or2",
                                        "or1", "or2", "Y",   "X"),
                                end  =c("rg1", "g1",  "go1", "o1",  "or1", "r1",
                                        "rg2", "g2",  "go2", "o2",  "or2", "r2",
                                        "X",   "Y",   "rg1", "rg2"),
                                  stringsAsFactors=FALSE))
plot.petrinet(lec_21_page_23_pn)
log_filename <- "lec_21_page_23_pn.log"; dfs.petrisim_steps <- 0
junk_pn <-
    token.game(lec_21_page_23_pn, steps=0, animate=TRUE,
               reset=TRUE, wait=100, file=log_filename)
#junk_nxt_pn <- dfs.petrisim(lec_21_page_23_pn, indent=0, file=log_filename)
junk_nxt_pn <-
    token.game(lec_21_page_23_pn, steps=50, animate=TRUE,
               reset=FALSE, wait=100, file=log_filename)
analyze.petrisim(pn=lec_21_page_23_pn, file=log_filename, burnin=0,
                 stationary=FALSE)

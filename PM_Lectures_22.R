#rm(list=ls())
setwd("~/Documents/Work/Courses/Coursera/process-mining/Projects/PM_Lectures")
source("~/Dropbox/datascience/R/mypetrinet.R")

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

#rm(list=ls())
setwd("~/Documents/Work/Courses/Coursera/process-mining/Projects/PM_Lectures")
source("~/Dropbox/datascience/R/mypetrinet.R")

## page_3
page_3_pn <- petrinet(name="page_3",
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
plot.petrinet(page_3_pn)
log_filename <- "page_3_pn.log"; dfs.petrisim_steps <- 0
junk_pn <-
    token.game(page_3_pn, steps=0, animate=TRUE,
               reset=TRUE, wait=100, file=log_filename)
#junk_nxt_pn <- dfs.petrisim(page_3_pn, indent=0, file=log_filename)
junk_nxt_pn <-
    token.game(page_3_pn, steps=50, animate=TRUE,
               reset=FALSE, wait=100, file=log_filename)
analyze.petrisim(pn=page_3_pn, file=log_filename, burnin=0,
                 stationary=FALSE)

## page_5
page_5_pn <- petrinet(name="page_5",
                      trans_df=data.frame(id=1:2, name=c("t1", "t2"),
                                          x=c(5, 5),
                                          y=c(5, 0),
                                          stringsAsFactors=FALSE),
                      places_df=data.frame(id=1:4, name=c("p1", "p2", "p3", "p4"),
                                           x=c(0, 10, 0, 10),
                                           y=c(5,  5, 0,  0),
                                           M0=c(2, 0, 1, 0),
                                           stringsAsFactors=FALSE),
                      arcs_df=data.frame(
                          begin=c("p1", "t1", "p3", "t2"),
                          end  =c("t1", "p2", "t2", "p4"),
                          stringsAsFactors=FALSE))
plot.petrinet(page_5_pn)
log_filename <- "page_5_pn.log"; dfs.petrisim_steps <- 0
junk_pn <-
    token.game(page_5_pn, steps=0, animate=TRUE,
               reset=TRUE, wait=100, file=log_filename)
junk_nxt_pn <- dfs.petrisim(page_5_pn, indent=0, file=log_filename)
# junk_nxt_pn <-
#     token.game(page_5_pn, steps=50, animate=TRUE,
#                reset=FALSE, wait=100, file=log_filename)
analyze.petrisim(pn=page_5_pn, file=log_filename, burnin=0,
                 stationary=FALSE)

## page_7
page_7_pn <- petrinet(name="page_7",
                      trans_df=data.frame(id=1:2, name=c("t1", "t2"),
                                          x=c(5, 5),
                                          y=c(5, 0),
                                          stringsAsFactors=FALSE),
                      places_df=data.frame(id=1:4, name=c("p1", "p2", "p3", "p4"),
                                           x=c(0, 10, 0, 10),
                                           y=c(5,  5, 0,  0),
                                           M0=c(2, 0, 1, 0),
                                           stringsAsFactors=FALSE),
                      arcs_df=data.frame(
                          begin=c("p1", "p1", "t1", "p3", "t2"),
                          end  =c("t1", "t2", "p2", "t2", "p4"),
                          stringsAsFactors=FALSE))
plot.petrinet(page_7_pn)
log_filename <- "page_7_pn.log"; dfs.petrisim_steps <- 0
junk_pn <-
    token.game(page_7_pn, steps=0, animate=TRUE,
               reset=TRUE, wait=100, file=log_filename)
junk_nxt_pn <- dfs.petrisim(page_7_pn, indent=0, file=log_filename)
# junk_nxt_pn <-
#     token.game(page_7_pn, steps=50, animate=TRUE,
#                reset=FALSE, wait=100, file=log_filename)
analyze.petrisim(pn=page_7_pn, file=log_filename, burnin=0,
                 stationary=FALSE)

## page_8
page_8_pn <- petrinet(name="page_8",
                      trans_df=data.frame(id=1:2, name=c("t1", "t2"),
                                          x=c(5, 5),
                                          y=c(5, 0),
                                          stringsAsFactors=FALSE),
                      places_df=data.frame(id=1:4, name=c("p1", "p2", "p3", "p4"),
                                           x=c(0, 10, 0, 10),
                                           y=c(5,  5, 0,  0),
                                           M0=c(3, 0, 1, 0),
                                           stringsAsFactors=FALSE),
                      arcs_df=data.frame(
                          begin=c("p1", "p1", "p1", "t1", "t1", "t1", "p3", "t2"),
                          end  =c("t1", "t2", "t2", "p2", "p2", "p2", "t2", "p4"),
                          stringsAsFactors=FALSE))
plot.petrinet(page_8_pn)
log_filename <- "page_8_pn.log"; dfs.petrisim_steps <- 0
junk_pn <-
    token.game(page_8_pn, steps=0, animate=TRUE,
               reset=TRUE, wait=100, file=log_filename)
junk_nxt_pn <- dfs.petrisim(page_8_pn, indent=0, file=log_filename)
# junk_nxt_pn <-
#     token.game(page_8_pn, steps=50, animate=TRUE,
#                reset=FALSE, wait=100, file=log_filename)
analyze.petrisim(pn=page_8_pn, file=log_filename, burnin=0,
                 stationary=FALSE)

## page_9
page_9_pn <- petrinet(name="page_9",
                      trans_df=data.frame(id=1:3, name=c("t1", "t2", "t3"),
                                          x=c( 5, 5, 15),
                                          y=c(15, 5,  5),
                                          stringsAsFactors=FALSE),
                      places_df=data.frame(id=1:3, name=c("p1", "p2", "p3"),
                                           x=c( 0, 10, 10),
                                           y=c(10, 10,  0),
                                           M0=c(1, 0, 0),
                                           stringsAsFactors=FALSE),
                      arcs_df=data.frame(
                          begin=c("p1", "t1", "p2", "t2", "p2", "p3", "t2"),
                          end  =c("t1", "p2", "t2", "p1", "t3", "t3", "p3"),
                          stringsAsFactors=FALSE))
plot.petrinet(page_9_pn)
log_filename <- "page_9_pn.log"; dfs.petrisim_steps <- 0
junk_pn <-
    token.game(page_9_pn, steps=0, animate=TRUE,
               reset=TRUE, wait=100, file=log_filename)
# junk_nxt_pn <- dfs.petrisim(page_9_pn, indent=0, file=log_filename)
junk_nxt_pn <-
    token.game(page_9_pn, steps=50, animate=TRUE,
               reset=FALSE, wait=100, file=log_filename)
analyze.petrisim(pn=page_9_pn, file=log_filename, burnin=0,
                 stationary=FALSE)

token.game(page_9_pn, steps=4, high.priority.trans="t2",
           animate=TRUE, reset=TRUE, wait=100)
token.game(page_9_pn, steps=4, high.priority.trans="t3",
           animate=TRUE, reset=TRUE, wait=100)


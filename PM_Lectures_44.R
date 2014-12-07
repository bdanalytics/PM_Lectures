rm(list=ls())
options(stringsAsFactors=FALSE)
setwd("~/Documents/Work/Courses/Coursera/process-mining/Projects/PM_Lectures")
source("~/Dropbox/datascience/R/mypetrinet.R")

#page5
page5_traces_df <- data.frame(trace=c(
rep("a,c,d,e,h"                           ,455),
rep("a,b,d,e,g"                           ,191),
rep("a,d,c,e,h"                           ,177),
rep("a,b,d,e,h"                           ,144),
rep("a,c,d,e,g"                           ,111),
rep("a,d,c,e,g"                           ,82),
rep("a,d,b,e,h"                           ,56),
rep("a,c,d,e,f,d,b,e,h"                   ,47),
rep("a,d,b,e,g"                           ,38),
rep("a,c,d,e,f,b,d,e,h"                   ,33),
rep("a,c,d,e,f,b,d,e,g"                   ,14),
rep("a,c,d,e,f,d,b,e,g"                   ,11),
rep("a,d,c,e,f,c,d,e,h"                   ,9),
rep("a,d,c,e,f,d,b,e,h"                   ,8),
rep("a,d,c,e,f,b,d,e,g"                   ,5),
rep("a,c,d,e,f,b,d,e,f,d,b,e,g"           ,3),
rep("a,d,c,e,f,d,b,e,g"                   ,2),
rep("a,d,c,e,f,b,d,e,f,b,d,e,g"           ,2),
rep("a,d,c,e,f,d,b,e,f,b,d,e,h"           ,1),
rep("a,d,b,e,f,b,d,e,f,d,b,e,g"           ,1),
rep("a,d,c,e,f,d,b,e,f,c,d,e,f,d,b,e,g"   ,1)
))
page5_traces_L_mtrx <- footprint.traces(page5_traces_df);
print(page5_traces_L_mtrx)

page12_pn <- petrinet(name="page12",
             trans_df=data.frame(id=1:5,
                                 name=c("a", "c", "d", "e", "h"),
                                 x=c(    5,   15,  15,  25,  35),
                                 y=c(    0,    5,  -5,   0,   0)),
             places_df=data.frame(id=1:7,
                            name=c("start", "p1", "p2", "p3", "p4", "p5", "end"),
                            x=c(         0,   10,   10,   20,   20,   30,    40),
                            y=c(         0,    5,   -5,    5,   -5,    0,     0),
                            M0=c(        1,    0,    0,    0,    0,    0,     0)),
             arcs_df=data.frame(
    begin=c("start","a",  "a","p1","p2", "c", "d","p3","p4", "e","p5",  "h"),
    end  =c("a",    "p1","p2", "c", "d","p3","p4", "e", "e","p5", "h","end")))
ggplot.petrinet(page12_pn)
log_filename <- "page12_pn.log"; dfs.petrisim_steps <- 0
junk_pn <-
    token.game(page12_pn, steps=0, animate=FALSE,
               reset=TRUE, wait=100, file=log_filename)
junk_nxt_pn <- dfs.petrisim(page12_pn, indent=0, file=log_filename)
# junk_nxt_pn <-
#     token.game(page12_pn, steps=50, animate=FALSE,
#                reset=FALSE, wait=100, file=log_filename)
page12_pn_markings_df <- read.table(paste(log_filename,sep=""),header=TRUE)
page12_pn_traces_df <- data.frame(trace=c(
rep("a,c,d,e,h"                           ,2),
rep("a,d,c,e,h"                           ,2)))
page12_pn_traces_L_mtrx <- footprint.traces(traces_df=page12_pn_traces_df);
print(page12_pn_traces_L_mtrx)
print(footprint.conformance(truth_L_mtrx=page5_traces_L_mtrx,
                            compare_L_mtrx=page12_pn_traces_L_mtrx))

page14_pn <- petrinet(name="page14",
                      trans_df=data.frame(id=1:8,
                                          name=c("a","b","c","d","e","f","g","h"),
                                          x=c(    5,  10, 10, 20, 20, 25, 30, 30),
                                          y=c(    0,   5, -5,  5, -5,  0,  5, -5)),
                      places_df=data.frame(id=1:3,
                                           name=c("start","p1","end"),
                                           x=c(         0, 15,    35),
                                           y=c(         0,  0,     0),
                                           M0=c(        1,  0,     0)),
                      arcs_df=data.frame(
    begin=c("start", "a","p1", "b","p1", "c","p1", "d","p1", "e","p1", "f",
"p1","p1",  "g",  "h"),
    end  =c(    "a","p1", "b","p1", "c","p1", "d","p1", "e","p1", "f","p1",
 "g", "h","end","end")))
ggplot.petrinet(page14_pn)
log_filename <- "page14_pn.log"; dfs.petrisim_steps <- 0
junk_pn <-
    token.game(page14_pn, steps=0, animate=FALSE,
               reset=TRUE, wait=100, file=log_filename)
#junk_nxt_pn <- dfs.petrisim(page14_pn, indent=0, file=log_filename)
junk_nxt_pn <-
     token.game(page14_pn, steps=50, animate=FALSE,
                reset=FALSE, wait=100, file=log_filename)
page14_pn_markings_df <- read.table(paste(log_filename,sep=""),header=TRUE)
page14_pn_traces_df <- data.frame(trace=c(
    rep("a,g"                           ,2),
    rep("a,h"                           ,2),

    rep("a,b,g"                           ,2),
    rep("a,b,h"                           ,2),
    rep("a,c,g"                           ,2),
    rep("a,c,h"                           ,2),
    rep("a,d,g"                           ,2),
    rep("a,d,h"                           ,2),
    rep("a,e,g"                           ,2),
    rep("a,e,h"                           ,2),
    rep("a,f,g"                           ,2),
    rep("a,f,h"                           ,2),

    rep("a,b,b,g"                           ,2),
    rep("a,c,c,g"                           ,2),
    rep("a,d,d,g"                           ,2),
    rep("a,e,e,g"                           ,2),
    rep("a,f,f,g"                           ,2),

    rep("a,b,c,h"                           ,2),
    rep("a,c,d,h"                           ,2),
    rep("a,d,e,h"                           ,2),
    rep("a,e,f,h"                           ,2),
    rep("a,f,b,h"                           ,2),

    rep("a,b,d,h"                           ,2),
    rep("a,c,e,h"                           ,2),
    rep("a,d,f,h"                           ,2),
    rep("a,e,b,h"                           ,2),
    rep("a,f,c,h"                           ,2)
    ))
page14_pn_traces_L_mtrx <- footprint.traces(traces_df=page14_pn_traces_df);
print(page14_pn_traces_L_mtrx)
print(footprint.conformance(truth_L_mtrx=page5_traces_L_mtrx,
                            compare_L_mtrx=page14_pn_traces_L_mtrx))


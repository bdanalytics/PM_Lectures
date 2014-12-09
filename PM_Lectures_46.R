rm(list=ls())
options(stringsAsFactors=FALSE)
setwd("~/Documents/Work/Courses/Coursera/process-mining/Projects/PM_Lectures")
source("~/Dropbox/datascience/R/mypetrinet.R")

page4_pn <- petrinet(name="page4",
                     trans_df=data.frame(id=1:5,
                                         name=c("a","b","e","c","d"),
                                         x=c(    5,  15, 15, 15, 25),
                                         y=c(    0,   5,  0, -5,  0)),
                     places_df=data.frame(id=1:6,
                            name=c("start", "p1", "p2", "p3", "p4", "end"),
                            x=c(         0,   10,   10,   20,   20,    30),
                            y=c(         0,    5,   -5,    5,   -5,     0),
                            M0=c(        1,    0,    0,    0,    0,     0)),
                     arcs_df=data.frame(
    begin=c("start","a",  "a","p1","p1","p2","p2", "b", "e", "e", "c","p3","p4"
,  "d"),
    end  =c("a",    "p1","p2", "b", "e", "e", "c","p3","p3","p4","p4", "d", "d","end")))
ggplot.petrinet(page4_pn)
print(fitness.traces(page4_pn, data.frame(trace=c(rep("a,b,c,d",    10)))))
print(fitness.traces(page4_pn, data.frame(trace=c(rep("a,b,d",    1)))))
print(fitness.traces(page4_pn, data.frame(trace=c(rep("a,c,d",    1)))))
print(fitness.traces(page4_pn, data.frame(trace=c(rep("a,d",    1)))))
print(fitness.traces(page4_pn, data.frame(trace=c(rep("a,b,b,d",    1)))))
print(fitness.traces(pn=page4_pn, traces_df=data.frame(trace=c(
    rep("a,b,c,d", 10),
    rep("a,c,b,d", 10),
    rep("a,e,d",   10),
    rep("a,b,d",    2),
    rep("a,c,d",    1),
    rep("a,d",      1),
    rep("a,b,b,d",  1)
))))
print(fitness.traces(pn=page4_pn, traces_df=data.frame(trace=c(
    rep("e",  1)
))))
print(replay.petrisim(pn=page4_pn, replay.trans="e"))

#page10
page10_pn <- petrinet(name="page10",
                     trans_df=data.frame(id=1:5,
                                         name=c("a","b","c","d","e"),
                                         x=c(    5,   5, 15, 25, 25),
                                         y=c(    5,  -5,  0,  5, -5)),
                     places_df=data.frame(id=1:6,
                                name=c("start", "p1", "p2", "p3", "p4", "end"),
                                x=c(         0,   10,   15,   15,   20,    30),
                                y=c(         0,    0,    5,   -5,    0,     0),
                                M0=c(        1,    0,    0,    0,    0,     0)),
                     arcs_df=data.frame(
    begin=c("start","start", "a", "a", "b","p1", "b","p3","p4", "c","p2","p4"
,  "d",  "e"),
    end  =c("a"    ,    "b","p1","p2","p1", "c","p3", "e", "e","p4", "d", "d"
,"end","end")))
ggplot.petrinet(page10_pn)
print(fitness.traces(pn=page10_pn, traces_df=data.frame(trace=c(
    rep("a,c,d", 10),
    rep("b,c,e", 10),
    rep("a,c,e",  5),
    rep("b,c,d",  5),
    rep("d,c,a",  1),
    rep("a,b,d",  1),
    rep("d",      1)
))))

#page13
page13_pn <- petrinet(name="page13",
                      trans_df=data.frame(id=1:6,
                                          name=c("a","b","c","d","e","f"),
                                          x=c(    5,  15, 15, 25, 20, 10),
                                          y=c(    0,   5, -5,  0,  0,  0)),
                      places_df=data.frame(id=1:7,
                            name=c("start", "p1", "p2", "p3", "p4", "p5", "end"),
                            x=c(         0,   10,   10,   20,   20,   15,    30),
                            y=c(         0,    5,   -5,    5,   -5,    0,     0),
                            M0=c(        1,    0,    0,    0,    0,    0,     0)),
                      arcs_df=data.frame(
    begin=c("start", "a", "a","p1", "f", "f","p2", "b","p5","p3", "e","p4"
, "c","p3","p4",  "d"),
    end  =c("a"    ,"p1","p2", "b","p1","p2", "c","p3", "f", "e","p5", "e"
,"p4", "d", "d","end")))
ggplot.petrinet(page13_pn)
print(fitness.traces(pn=page13_pn, traces_df=data.frame(trace=c(
    rep("a,b,e,f,c,d",     10),
    rep("a,b,b,e,f,c,c,d", 10)
))))

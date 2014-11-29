#rm(list=ls())
setwd("~/Documents/Work/Courses/Coursera/process-mining/Projects/PM_Lectures")
source("~/Dropbox/datascience/R/mypetrinet.R")

## page_14
page_14_pn <-
    petrinet(name="page_14",
        trans_df=data.frame(id=1:4, name=c("t1", "t2", "t3", "t4"),
                                           x=c(-10, 10, 0,  0),
                                           y=c( 10, 10, 0, 15),
                                           stringsAsFactors=FALSE),
        places_df=data.frame(id=1:4, name=c("p1", "p2", "p3", "p4"),
                                            x=c(-5,  0, 5, 0),
                                            y=c( 5, 10, 5, 5),
                                            M0=c(1, 0, 0, 0),
                                            stringsAsFactors=FALSE),
        arcs_df=data.frame(
            begin=c("t1", "t1", "p1", "t3", "t3", "p4", "p4", "p4", "p2", "p3" , "t2", "t2"),
            end  =c("p2", "p4", "t1", "p1", "p4", "t2", "t3", "t4", "t2", "t3", "p4", "p3"),
                           stringsAsFactors=FALSE))
plot.petrinet(page_14_pn)
replay.petrisim(pn=page_14_pn, replay.trans=c("t1", "t2"))
replay.petrisim(pn=page_14_pn, replay.trans=c("t1", "t2", "t3", "t1"))
replay.petrisim(pn=page_14_pn, replay.trans=c("t1", "t2", "t3", "t4"))
replay.petrisim(pn=page_14_pn, replay.trans=c("t1", "t2", "t4"))
replay.petrisim(pn=page_14_pn, replay.trans=c("t1", "t4"))

## page_16
page_16_pn <-
    petrinet(name="page_16",
             trans_df=data.frame(id=1:5, name=c("t1", "t2", "t3", "t4", "t5"),
                                 x=c(-10, 10, 0,  0, 10),
                                 y=c( 10, 10, 0, 10, 15),
                                 stringsAsFactors=FALSE),
             places_df=data.frame(id=1:4, name=c("p1", "p2", "p3", "p4"),
                                  x=c(-5,  0, 5, 0),
                                  y=c( 5, 15, 5, 5),
                                  M0=c(1, 0, 0, 1),
                                  stringsAsFactors=FALSE),
             arcs_df=data.frame(
                 begin=c("t1", "t1", "p1", "t3", "t3", "p4", "p4", "p4", "p2", "p3" , "t4", "t2", "p2"),
                 end  =c("p2", "p4", "t1", "p1", "p4", "t2", "t3", "t4", "t2", "t3", "p4", "p3", "t5"),
                 stringsAsFactors=FALSE))
plot.petrinet(page_16_pn)
replay.petrisim(pn=page_16_pn, replay.trans=c("t1"))
replay.petrisim(pn=page_16_pn, replay.trans=c("t1", "t2"))
replay.petrisim(pn=page_16_pn, replay.trans=c("t1", "t4"))
replay.petrisim(pn=page_16_pn, replay.trans=c("t1", "t5"))
replay.petrisim(pn=page_16_pn, replay.trans=c("t4"))

## page_25
page_25_pn <-
    petrinet(name="page_25",
        trans_df=data.frame(id=1:6, name=c("a", "b", "c", "d", "e", "f"),
                            x=c(5, 10, 10, 15, 15, 25),
                            y=c(5, 10,  0, 10,  0,  5),
                            stringsAsFactors=FALSE),
        places_df=data.frame(id=1:6, name=c("p1", "p2", "p3", "p4", "p5", "p6"),
                            x=c(0, 5, 5, 10, 20, 30),
                            y=c(5, 10, 0,  5,  5,  5),
                            M0=c(1, 0, 0, 0, 0, 0),
                            stringsAsFactors=FALSE),
        arcs_df=data.frame(
                 begin=c("p1", "a",  "a",  "p2", "p3", "b",  "c",  "p4", "p4", "d" , "e",  "p5", "p5", "f" ),
                 end  =c("a",  "p2", "p3", "b",  "c",  "p4", "p4", "d",  "e",  "p5", "p5", "f",  "f",  "p6"),
                 stringsAsFactors=FALSE))
plot.petrinet(x=page_25_pn)
ggplot.petrinet(pn=page_25_pn)

replay.petrisim(pn=page_25_pn, replay.trans=c("a", "b", "c", "d", "d", "f"))
replay.petrisim(pn=page_25_pn, replay.trans=c("a", "b", "c", "d", "e", "f"))
replay.petrisim(pn=page_25_pn, replay.trans=c("a", "b", "c", "e", "d", "f"))
replay.petrisim(pn=page_25_pn, replay.trans=c("a", "b", "c", "e", "e", "f"))
replay.petrisim(pn=page_25_pn, replay.trans=c("a", "b", "d", "c", "d", "f"))
replay.petrisim(pn=page_25_pn, replay.trans=c("a", "b", "d", "c", "e", "f"))
replay.petrisim(pn=page_25_pn, replay.trans=c("a", "b", "e", "c", "d", "f"))
replay.petrisim(pn=page_25_pn, replay.trans=c("a", "b", "e", "c", "e", "f"))

replay.petrisim(pn=page_25_pn, replay.trans=c("a", "c"))
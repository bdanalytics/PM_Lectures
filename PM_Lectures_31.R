rm(list=ls())
options(stringsAsFactors=FALSE)
setwd("~/Documents/Work/Courses/Coursera/process-mining/Projects/PM_Lectures")
source("~/Dropbox/datascience/R/mypetrinet.R")

#page18
page18_traces_df <- data.frame(trace=c(
    rep("a,c,d", 2),
    rep("b,c,e", 2)
), stringsAsFactors=FALSE)
print(page18_traces_df)
page18_pn <- alpha.petrinet(page18_traces_df)
#page4_pn$trans$name[7] <- "rej_r"
page18_pn$trans[page18_pn$trans$name == "a", c("x", "y")] <- c(5, 5)
page18_pn$places[page18_pn$places$name == "p1", c("x", "y")] <- c(5, 0)
page18_pn$trans[page18_pn$trans$name == "b", c("x", "y")] <- c(5, -5)
page18_pn$trans[page18_pn$trans$name == "c", c("x", "y")] <- c(10, 0)
page18_pn$trans[page18_pn$trans$name == "d", c("x", "y")] <- c(15, 5)
page18_pn$places[page18_pn$places$name == "p2", c("x", "y")] <- c(15, 0)
page18_pn$trans[page18_pn$trans$name == "e", c("x", "y")] <- c(15, -5)
page18_pn$places[page18_pn$places$name == "end", c("x", "y")] <- c(20, 0)
ggplot.petrinet(page18_pn)

page18_2_pn <- add.petrinet(pn=page18_pn,
                                 places_df=data.frame(
                                     name=c("p4","p5"),
                                     x=c(    10,  10),
                                     y=c(     5,  -5),
                                     M0=c(     0,   0),
                                     stringsAsFactors=FALSE),
                                 arcs_df=data.frame(
                                     begin=c("a", "p4","b", "p5"),
                                     end  =c("p4","d", "p5","e"),
                                     stringsAsFactors=FALSE))
ggplot.petrinet(page18_2_pn)
replay.petrisim(page18_2_pn, c("a","c"))
replay.petrisim(page18_2_pn, c("b","c"))




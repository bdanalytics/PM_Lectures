rm(list=ls())
setwd("~/Documents/Work/Courses/Coursera/process-mining/Projects/PM_Lectures")
source("~/Dropbox/datascience/R/mypetrinet.R")

#page12
page12_traces_df <- data.frame(trace=c(
    rep("a,b,c,d", 3),
    rep("a,c,b,d", 2),
    rep("a,e,d",   1)
    ), stringsAsFactors=FALSE)
print(page12_traces_df)
#page12_pn_save <- page12_pn
page12_pn <- alpha.petrinet(page12_traces_df); page12_pn$name <- "page12_pn"
#all.equal(page12_pn_save, page12_pn)
page12_pn$trans[page12_pn$trans$name == "a", c("x", "y")] <- c(5, 0)
page12_pn$places[page12_pn$places$name == "p1", c("x", "y")] <- c(10, 5)
page12_pn$places[page12_pn$places$name == "p2", c("x", "y")] <- c(10, -5)
page12_pn$trans[page12_pn$trans$name == "b", c("x", "y")] <- c(15, 10)
page12_pn$trans[page12_pn$trans$name == "e", c("x", "y")] <- c(15, 0)
page12_pn$trans[page12_pn$trans$name == "c", c("x", "y")] <- c(15, -10)
page12_pn$places[page12_pn$places$name == "p3", c("x", "y")] <- c(20, 5)
page12_pn$places[page12_pn$places$name == "p4", c("x", "y")] <- c(20, -5)
page12_pn$trans[page12_pn$trans$name == "d", c("x", "y")] <- c(25, 0)
page12_pn$places[page12_pn$places$name == "end", c("x", "y")] <- c(30, 0)
ggplot.petrinet(page12_pn)

#page19
page19_traces_df <- data.frame(trace=c(
    rep("a,b,c,d", 3),
    rep("a,c,b,d", 4),
    rep("a,b,c,e,f,b,c,d", 2),
    rep("a,b,c,e,f,c,b,d", 1),
    rep("a,c,b,e,f,b,c,d", 2),
    rep("a,c,b,e,f,b,c,e,f,c,b,d", 1)
), stringsAsFactors=FALSE)
#print(page19_traces_df)
page19_L_mtrx <- footprint.traces(page19_traces_df)
print(page19_L_mtrx)

page19_Tl <- unique.trans(page19_traces_df); print(page19_Tl)

page19_Ti <- first.trans(page19_traces_df);  print(page19_Ti)

page19_To <- last.trans(page19_traces_df);  print(page19_To)

page19_Xl_df <- Xl.pairs(page19_L_mtrx)


#page33
page33_traces_df <- data.frame(trace=c(
    rep("a,b,c,d,e,f,b,d,c,e,g", 1),
    rep("a,b,d,c,e,g", 2),
    rep("a,b,c,d,e,f,b,c,d,e,f,b,d,c,e,g", 1)
), stringsAsFactors=FALSE)
#print(page33_traces_df)
page33_L_mtrx <- footprint.traces(page33_traces_df)
print(page33_L_mtrx)
page33_pn <- alpha.petrinet(page33_traces_df)

page33_pn$trans[page33_pn$trans$name == "a", c("x", "y")] <- c(5, 0)
page33_pn$places[page33_pn$places$name == "p1", c("x", "y")] <- c(10, 0)
page33_pn$trans[page33_pn$trans$name == "b", c("x", "y")] <- c(15, 0)
page33_pn$places[page33_pn$places$name == "p2", c("x", "y")] <- c(20, 5)
page33_pn$places[page33_pn$places$name == "p3", c("x", "y")] <- c(20, -5)
page33_pn$trans[page33_pn$trans$name == "f", c("x", "y")] <- c(25, 10)
page33_pn$trans[page33_pn$trans$name == "c", c("x", "y")] <- c(25, 5)
page33_pn$trans[page33_pn$trans$name == "d", c("x", "y")] <- c(25, -5)
page33_pn$places[page33_pn$places$name == "p4", c("x", "y")] <- c(30, 5)
page33_pn$places[page33_pn$places$name == "p5", c("x", "y")] <- c(30, -5)
page33_pn$trans[page33_pn$trans$name == "e", c("x", "y")] <- c(35, 0)
page33_pn$places[page33_pn$places$name == "p6", c("x", "y")] <- c(40, 0)
page33_pn$trans[page33_pn$trans$name == "g", c("x", "y")] <- c(45, 0)
page33_pn$places[page33_pn$places$name == "end", c("x", "y")] <- c(50, 0)

ggplot.petrinet(page33_pn)

#page37
page37_traces_df <- data.frame(trace=c(
    rep("a,c,d", 45),
    rep("b,c,d", 42),
    rep("a,c,e", 38),
    rep("b,c,e", 22)
), stringsAsFactors=FALSE)
print(page37_traces_df)
page37_pn <- alpha.petrinet(page37_traces_df)

page37_pn$trans[page37_pn$trans$name == "b", c("x", "y")] <- c(5, -5)
page37_pn$trans[page37_pn$trans$name == "c", c("x", "y")] <- c(15, 0)
page37_pn$trans[page37_pn$trans$name == "d", c("x", "y")] <- c(25, 5)
page37_pn$trans[page37_pn$trans$name == "e", c("x", "y")] <- c(25, -5)
#page37_pn$places[page37_pn$places$name == "p1", c("x", "y")] <- c(10, 0)

ggplot.petrinet(page37_pn)

#page38
page38_traces_df <- data.frame(trace=c(
    rep("a,b,e,f", 2),
    rep("a,b,e,c,d,b,f", 3),
    rep("a,b,c,e,d,b,f", 2),
    rep("a,b,c,d,e,b,f", 4),
    rep("a,e,b,c,d,b,f", 3)
), stringsAsFactors=FALSE)
print(page38_traces_df)
page38_pn <- alpha.petrinet(page38_traces_df)

page38_pn$trans[page38_pn$trans$name == "a", c("x", "y")] <- c(5, 0)
page38_pn$trans[page38_pn$trans$name == "d", c("x", "y")] <- c(10, 10)
page38_pn$places[page38_pn$places$name == "p2", c("x", "y")] <- c(10, 5)
page38_pn$places[page38_pn$places$name == "p1", c("x", "y")] <- c(10, -5)
page38_pn$places[page38_pn$places$name == "p4", c("x", "y")] <- c(15, 10)
page38_pn$trans[page38_pn$trans$name == "b", c("x", "y")] <- c(15, 5)
page38_pn$trans[page38_pn$trans$name == "e", c("x", "y")] <- c(15, -5)
page38_pn$trans[page38_pn$trans$name == "c", c("x", "y")] <- c(20, 10)
page38_pn$places[page38_pn$places$name == "p3", c("x", "y")] <- c(20, 5)
page38_pn$places[page38_pn$places$name == "p5", c("x", "y")] <- c(20, -5)
page38_pn$trans[page38_pn$trans$name == "f", c("x", "y")] <- c(25, 0)
page38_pn$places[page38_pn$places$name == "end", c("x", "y")] <- c(30, 0)

ggplot.petrinet(page38_pn)


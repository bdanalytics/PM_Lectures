rm(list=ls())
options(stringsAsFactors=FALSE)
setwd("~/Documents/Work/Courses/Coursera/process-mining/Projects/PM_Lectures")
source("~/Dropbox/datascience/R/mypetrinet.R")

#page135
page135_traces_df <- data.frame(trace=c(
    rep("a,b,e,f", 2),
    rep("a,b,e,c,d,b,f", 2),
    rep("a,b,c,e,d,b,f", 2),
    rep("a,b,c,d,e,b,f", 2),
    rep("a,e,b,c,d,b,f", 2)
))
print(page135_traces_df)
page135_pn <- alpha.petrinet(page135_traces_df)
#page135_pn$trans$name[7] <- "rej_r"
page135_pn$trans[page135_pn$trans$name == "a", c("x", "y")] <- c(5, 0)
page135_pn$trans[page135_pn$trans$name == "d", c("x", "y")] <- c(10, 10)
page135_pn$places[page135_pn$places$name == "p1", c("x", "y")] <- c(10, 5)
page135_pn$places[page135_pn$places$name == "p3", c("x", "y")] <- c(10, -5)
page135_pn$places[page135_pn$places$name == "p2", c("x", "y")] <- c(15, 10)
page135_pn$trans[page135_pn$trans$name == "b", c("x", "y")] <- c(15, 5)
page135_pn$trans[page135_pn$trans$name == "e", c("x", "y")] <- c(15, -5)
page135_pn$trans[page135_pn$trans$name == "c", c("x", "y")] <- c(20, 10)
page135_pn$places[page135_pn$places$name == "p5", c("x", "y")] <- c(20, 5)
page135_pn$places[page135_pn$places$name == "p4", c("x", "y")] <- c(20, -5)
page135_pn$trans[page135_pn$trans$name == "f", c("x", "y")] <- c(25, 0)
page135_pn$places[page135_pn$places$name == "end", c("x", "y")] <- c(30, 0)
ggplot.petrinet(page135_pn)

traces_df <- page135_traces_df
L_mtrx <- footprint.traces(traces_df); print(L_mtrx)
Tl <- unique.trans(traces_df); print(Tl)
Ti <- first.trans(traces_df);  print(Ti)
To <- last.trans(traces_df);   print(To)
Xl_df <- Xl.pairs(L_mtrx); print(Xl_df)
Yl_df <- select_maximal_pairs(Xl_df, dups_col="A", sets_col="B"); print(Yl_df)
Yl_df <- select_maximal_pairs(Yl_df, dups_col="B", sets_col="A");
print(orderBy(~A+B, Yl_df[,c("A", "B")]))

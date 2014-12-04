rm(list=ls())
options(stringsAsFactors=FALSE)
setwd("~/Documents/Work/Courses/Coursera/process-mining/Projects/PM_Lectures")
source("~/Dropbox/datascience/R/mypetrinet.R")

#page4
page4_traces_df <- data.frame(trace=c(
    rep("a,e", 5),
    rep("a,b,c,e", 10),
    rep("a,c,b,e", 10),
    rep("a,b,e", 1),
    rep("a,c,e", 1),
    rep("a,d,e", 10),
    rep("a,d,d,e", 2),
    rep("a,d,d,d,e", 1)
), stringsAsFactors=FALSE)
print(page4_traces_df)
page4_pn <- alpha.petrinet(page4_traces_df)
#page4_pn$trans$name[7] <- "rej_r"
page4_pn$trans[page4_pn$trans$name == "a", c("x", "y")] <- c(5, 0)
page4_pn$places[page4_pn$places$name == "p3", c("x", "y")] <- c(5, 10)
page4_pn$places[page4_pn$places$name == "p1", c("x", "y")] <- c(10, 5)
page4_pn$places[page4_pn$places$name == "p2", c("x", "y")] <- c(10, -5)
page4_pn$trans[page4_pn$trans$name == "b", c("x", "y")] <- c(15, 5)
page4_pn$trans[page4_pn$trans$name == "d", c("x", "y")] <- c(15, 0)
page4_pn$trans[page4_pn$trans$name == "c", c("x", "y")] <- c(15, -5)
page4_pn$places[page4_pn$places$name == "p4", c("x", "y")] <- c(20, 5)
page4_pn$places[page4_pn$places$name == "p5", c("x", "y")] <- c(20, -5)
page4_pn$trans[page4_pn$trans$name == "e", c("x", "y")] <- c(25, 0)
page4_pn$places[page4_pn$places$name == "end", c("x", "y")] <- c(30, 0)
ggplot.petrinet(page4_pn)

traces_df <- page4_traces_df
L_mtrx <- footprint.traces(traces_df); print(L_mtrx)
Tl <- unique.trans(traces_df); print(Tl)
Ti <- first.trans(traces_df);  print(Ti)
To <- last.trans(traces_df);   print(To)
Xl_df <- Xl.pairs(L_mtrx); print(Xl_df)
Yl_df <- select_maximal_pairs(Xl_df, dups_col="A", sets_col="B"); print(Yl_df)
Yl_df <- select_maximal_pairs(Yl_df, dups_col="B", sets_col="A");
print(orderBy(~A+B, Yl_df[,c("A", "B")]))

#page23
page23_traces_df <- data.frame(trace=c(
    rep("a,e", 5),
    rep("a,b,c,e", 10),
    rep("a,c,b,e", 10),
    rep("a,b,e", 1),
    rep("a,c,e", 1),
    rep("a,d,e", 10),
    rep("a,d,d,e", 2),
    rep("a,d,d,d,e", 1)
), stringsAsFactors=FALSE)
#print(page23_traces_df)
print(footprint.traces(page23_traces_df))

freq.traces <- function(traces_df) {
    unique_trans <- unique.trans(traces_df)
    L_freq_mtrx <- matrix(rep(0, length(unique_trans) ^ 2),
                     nrow=length(unique_trans),
                     dimnames=list(unique_trans, unique_trans))

    # Count direct succession freq by trace
    for (trc in 1:nrow(traces_df)) {
        transs <- unlist(strsplit(traces_df$trace[trc], split=","))
        for (trs in 1:length(transs)) {
            if (trs != length(transs)) {
                L_freq_mtrx[transs[trs], transs[trs+1]] <-
                    L_freq_mtrx[transs[trs], transs[trs+1]] + 1
            }
        }
    }

    return(L_freq_mtrx)
}
page23_L_freq_mtrx <- freq.traces(traces_df=page23_traces_df)
print(page23_L_freq_mtrx)

dependency.traces <- function(L_freq_mtrx) {
    L_depnd_mtrx <- matrix(rep(0, nrow(L_freq_mtrx) ^ 2),
                          nrow=nrow(L_freq_mtrx),
                          dimnames=dimnames(L_freq_mtrx))

    # Compute dependencies for each element in matrix
    for (row in 1:nrow(L_freq_mtrx))
        for (col in 1:ncol(L_freq_mtrx))
            if (row == col) {
                L_depnd_mtrx[row, col] <-
                    (L_freq_mtrx[row, col] * 1.0) / (L_freq_mtrx[row, col] + 1)
            } else {
                L_depnd_mtrx[row, col] <-
                    ((L_freq_mtrx[row, col] * 1.0) -
                     (L_freq_mtrx[col, row] * 1.0)) /
                    (L_freq_mtrx[row, col] + L_freq_mtrx[col, row] + 1)
            }

    return(L_depnd_mtrx)
}
page23_L_depnd_mtrx <- dependency.traces(L_freq_mtrx=page23_L_freq_mtrx)
print(page23_L_depnd_mtrx)
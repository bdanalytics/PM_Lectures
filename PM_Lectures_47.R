rm(list=ls())
options(stringsAsFactors=FALSE)
setwd("~/Documents/Work/Courses/Coursera/process-mining/Projects/PM_Lectures")
source("~/Dropbox/datascience/R/mypetrinet.R")

#page15
page15_pn <- petrinet(name="page15",
                     trans_df=data.frame(id=1:8,
                                         name=c("a","b","c","d","e","f","g","h"),
                                         x=c(    5,  15, 15, 15, 25, 25, 35, 35),
                                         y=c(    0,  10, 0,  -5,  0,-10,  5, -5)),
                     places_df=data.frame(id=1:7,
                                          name=c("start", "p1", "p2", "p3", "p4", "p5", "end"),
                                          x=c(         0,   10,   10,   20,   20,   30,    40),
                                          y=c(         0,    5,   -5,    5,   -5,    0,     0),
                                          M0=c(        1,    0,    0,    0,    0,    0,     0)),
                     arcs_df=data.frame(
                         begin=c("start","a",  "a","p1","p1","p2", "b", "c", "d","p3","p4", "e",
                                 "f", "f","p5","p5","p5",  "g",  "h"),
                         end  =c("a",    "p1","p2", "b", "c", "d","p3","p3","p4", "e", "e","p5",
                                 "p1","p2", "f", "g", "h","end","end")))
ggplot.petrinet(page15_pn)
# log_filename <- "page15_pn.log"; dfs.petrisim_steps <- 0
# junk_pn <-
#     token.game(page15_pn, steps=0, animate=FALSE,
#                reset=TRUE, wait=100, file=log_filename)
# # junk_nxt_pn <- dfs.petrisim(page15_pn, indent=0, file=log_filename)
# junk_nxt_pn <-
#     token.game(page15_pn, steps=50, animate=FALSE,
#                reset=FALSE, wait=100, file=log_filename)
# page15_pn_markings_df <- read.table(paste(log_filename,sep=""),header=TRUE)
page15_pn_traces_df <- data.frame(trace=c(
    rep("a,b,d,e,f,b,d,e,g", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,b,d,e,f,b,d,e,h", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,b,d,e,f,c,d,e,g", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,b,d,e,f,c,d,e,h", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,b,d,e,f,d,b,e,g", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,b,d,e,f,d,b,e,h", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,b,d,e,f,d,c,e,g", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,b,d,e,f,d,c,e,h", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,b,d,e,g", 1),
    rep("a,b,d,e,h", 1),

    rep("a,c,d,e,f,b,d,e,g", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,c,d,e,f,b,d,e,h", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,c,d,e,f,c,d,e,g", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,c,d,e,f,c,d,e,h", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,c,d,e,f,d,b,e,g", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,c,d,e,f,d,b,e,h", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,c,d,e,f,d,c,e,g", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,c,d,e,f,d,c,e,h", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,c,d,e,g", 1),
    rep("a,c,d,e,h", 1),

    rep("a,d,b,e,f,b,d,e,g", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,d,b,e,f,b,d,e,h", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,d,b,e,f,c,d,e,g", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,d,b,e,f,c,d,e,h", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,d,b,e,f,d,b,e,g", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,d,b,e,f,d,b,e,h", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,d,b,e,f,d,c,e,g", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,d,b,e,f,d,c,e,h", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,d,b,e,g", 1),
    rep("a,d,b,e,h", 1),

    rep("a,d,c,e,f,b,d,e,g", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,d,c,e,f,b,d,e,h", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,d,c,e,f,c,d,e,g", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,d,c,e,f,c,d,e,h", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,d,c,e,f,d,b,e,g", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,d,c,e,f,d,b,e,h", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,d,c,e,f,d,c,e,g", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,d,c,e,f,d,c,e,h", 1),   # skipping f due to "b,d,e,f" repetition
    rep("a,d,c,e,g", 1),
    rep("a,d,c,e,h", 1)

))
print(fitness.traces(pn=page15_pn, traces_df=page15_pn_traces_df))
page15_pn_unique_traces_df <- ddply(page15_pn_traces_df,
                                    .(page15_pn_traces_df$trace), nrow)
names(page15_pn_unique_traces_df) <- c("trace", "freq")
require(doBy)
print(orderBy(~-freq+trace,page15_pn_unique_traces_df))

# cost.traces <- function(trace_log, trace_mdl) {
#     #### fix to check for sequence in addition to membership
#
#     trans_log <- unlist(strsplit(trace_log,"[,]"))
#     trans_mdl <- unlist(strsplit(trace_mdl,"[,]"))
#
#     mod_log <- trans_log
#     mod_mdl <- trans_mdl
#
#     # find first trans that is different from the end
#     #   "*" is treated as a wildcard
#     rev_mod_log <- rev(mod_log)
#     rev_mod_mdl <- rev(mod_mdl)
#     diff_pos <- 0
#     for (pos in 1:max(length(rev_mod_log), length(rev_mod_mdl)))
#         if ((rev_mod_log[pos] != rev_mod_mdl[pos]) &
#             (rev_mod_log[pos] != "*") &
#             (rev_mod_mdl[pos] != "*")) {
#             diff_pos <- pos; break
#         }
#
#     if (diff_pos > 0) {
#         mod_log <- c(head(mod_log, -diff_pos), "*", tail(mod_log, diff_pos))
#         mod_mdl <- c(head(mod_mdl, -diff_pos), tail(mod_mdl, diff_pos), "*")
#         recrsv_lst <- cost.traces(head(mod_log, -(diff_pos+1)),
#                                   head(mod_mdl, -(diff_pos+1)))
#         mod_log <- c(recrsv_lst$mod_log, tail(mod_log, diff_pos + 1))
#         mod_mdl <- c(recrsv_lst$mod_mdl, tail(mod_mdl, diff_pos + 1))
#     }
#
#     cost <- length(grep("*", mod_log, fixed=TRUE)) +
#             length(grep("*", mod_mdl, fixed=TRUE))
#     return(list(cost=cost,  mod_log=paste0(mod_log, collapse=","),
#                             mod_mdl=paste0(mod_mdl, collapse=",")))
# }
# print(cost.traces(trace_log="a,b,e,f,d,e,g",
#                   trace_mdl=page15_pn_unique_traces_df[02, "trace"]))
# print(stringdist("a,b,e,f,d,e,g",
#                  page15_pn_unique_traces_df[02, "trace"]))
page15_pn_unique_traces_df <- orderBy(~-freq-cost+trace,
                                      mutate(page15_pn_unique_traces_df,
                                     trace_log="a,b,e,f,d,e,g",
                                     cost=stringdist(trace_log, trace)))
print(page15_pn_unique_traces_df)
#trace_log first, trace_mdl, second
print("a,b,*,e,f,*,d,e,g"); print(subset(page15_pn_unique_traces_df, cost==4)[01, "trace"])
print("a,b,*,e,f,*,d,e,g"); print(subset(page15_pn_unique_traces_df, cost==4)[02, "trace"])
print("a,b,*,e,f,d,*,e,g"); print(subset(page15_pn_unique_traces_df, cost==4)[03, "trace"])
print("a,b,*,e,f,d,*,e,g"); print(subset(page15_pn_unique_traces_df, cost==4)[04, "trace"])
print("a,b,e,f,d,e,g"); print(gsub("b,d", "b,*,*,d", subset(page15_pn_unique_traces_df, cost==4)[05, "trace"]))
print("a,b,*,e,f,d,*,e,g"); print(subset(page15_pn_unique_traces_df, cost==4)[06, "trace"])
print("a,*,b,e,f,*,d,e,g"); print(subset(page15_pn_unique_traces_df, cost==4)[07, "trace"])
print("a,*,b,e,f,d,*,e,g"); print(subset(page15_pn_unique_traces_df, cost==4)[08, "trace"])
print("a,*,b,e,f,d,*,e,g"); print(subset(page15_pn_unique_traces_df, cost==4)[09, "trace"])

print(alignment.fitness.trace(trace="a,b,e,f,d,e,g",
                              shortest_path_trace="a,b,d,e,g", edit_distance=2))

#page24
print(alignment.fitness.trace(trace="a,c1,c2,e1,e2,e3",
                              shortest_path_trace="b,c1,c2,e1,e2,e3",
                              edit_distance=2))
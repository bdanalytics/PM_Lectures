rm(list=ls())
options(stringsAsFactors=FALSE)
setwd("~/Documents/Work/Courses/Coursera/process-mining/Projects/PM_Lectures")
source("~/Dropbox/datascience/R/mypetrinet.R")

page9_pn <- petrinet(name="page9",
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
ggplot.petrinet(page9_pn)
print(replay.petrisim(pn=page9_pn, replay.trans="a,b,e,g"))
print(replay.petrisim(pn=page9_pn, replay.trans="a,c,d,e,h"))

#page18
page18_pn <- petrinet(name="page18",
                     trans_df=data.frame(id=1:8,
                                         name=c("a","b","c","d","e","f","g","h"),
                                         x=c(    5,  15, 15, 25, 35, 25, 45, 45),
                                         y=c(    0,  5,   0,  0,  0, -5,  5, -5)),
                     places_df=data.frame(id=1:6,
                            name=c("start", "p1", "p2", "p3", "p4", "end"),
                            x=c(         0,   10,   20,   30,   40,    50),
                            y=c(         0,    0,    0,    0,    0,     0),
                            M0=c(        1,    0,    0,    0,    0,     0)),
                     arcs_df=data.frame(
    begin=c("start","a","p1","p1","b", "c", "p2","d","p3", "e",
 "f","p4","p4","p4",  "g",  "h"),
    end  =c("a",   "p1", "b", "c","p2","p2", "d","p3","e","p4",
"p1", "f", "g", "h","end","end")))
ggplot.petrinet(page18_pn)
print(replay.petrisim(pn=page18_pn, replay.trans="a,d"))
print(replay.petrisim(pn=page18_pn, replay.trans="a,d,c,e,h"))

#page29
page29_traces_df <- data.frame(trace=c(
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
page29_1_pn <- page9_pn; ggplot.petrinet(page29_1_pn)
print(fitness.traces(pn=page29_1_pn, traces_df=page29_traces_df))

page29_2_pn <- page18_pn; ggplot.petrinet(page29_2_pn)
print(fitness.traces(pn=page29_2_pn, traces_df=page29_traces_df))

page29_3_pn <- petrinet(name="page29_3",
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
ggplot.petrinet(page29_3_pn)
print(replay.petrisim(pn=page29_3_pn, replay.trans="a,c"))
print(replay.petrisim(pn=page29_3_pn, replay.trans="a,d,c"))
print(replay.petrisim(pn=page29_3_pn, replay.trans="a,d,c,e,f,c,d,e,h"))
print(replay.petrisim(pn=page29_3_pn, replay.trans="a,d,c,e,f,c"))
print(replay.petrisim(pn=page29_3_pn, replay.trans="a,d,c,e,f"))
print(replay.petrisim(pn=page29_3_pn, replay.trans="a,d,c,e"))

print(replay.petrisim(pn=page29_3_pn, replay.trans="a,b,d,e,g"))
print(fitness.traces(pn=page29_3_pn, traces_df=page29_traces_df))
# unique_traces_df <- transform(unique_traces_df,
#         trans_b=ifelse((length(grep("b", trace, fixed=FALSE)) == 0), 0, 1))
unique_traces_df$trans_b <- sapply(1:nrow(unique_traces_df), function(row)
    ifelse((length(grep("b", unlist(strsplit(unique_traces_df[row, "trace"], "[,]")), fixed=TRUE)) >  0),
           1, 0))
unique_traces_df$trans_c <- sapply(1:nrow(unique_traces_df), function(row)
    ifelse((length(grep("c", unlist(strsplit(unique_traces_df[row, "trace"], "[,]")), fixed=TRUE)) >= 1),
           1, 0))
unique_traces_df$trans_c2 <- sapply(1:nrow(unique_traces_df), function(row)
    ifelse((length(grep("c", unlist(strsplit(unique_traces_df[row, "trace"], "[,]")), fixed=TRUE)) >= 2),
           1, 0))

page29_4_pn <- petrinet(name="page29_4",
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
ggplot.petrinet(page29_4_pn)
print(fitness.traces(pn=page29_4_pn, traces_df=page29_traces_df))

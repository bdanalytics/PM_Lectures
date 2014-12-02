rm(list=ls())
setwd("~/Documents/Work/Courses/Coursera/process-mining/Projects/PM_Lectures")
source("~/Dropbox/datascience/R/mypetrinet.R")

#page4
page4_traces_df <- data.frame(trace=c(
    rep("a,b,d,e,h", 1),
    rep("a,d,c,e,g", 1),
    rep("a,c,d,e,f,b,d,e,g", 1),
    rep("a,d,b,e,h", 1),
    rep("a,c,d,e,f,d,c,e,f,c,d,e,h", 1),
    rep("a,c,d,e,g", 1)
), stringsAsFactors=FALSE)
print(page4_traces_df)
page4_pn <- alpha.petrinet(page4_traces_df)

page4_pn$trans$name[1] <- "reg_r"
page4_pn$trans[page4_pn$trans$name == "reg_r", c("x", "y")] <- c(5, 0)
page4_pn$places[page4_pn$places$name == "p1", c("x", "y")] <- c(10, 5)
page4_pn$places[page4_pn$places$name == "p2", c("x", "y")] <- c(10, -5)
page4_pn$trans$name[2] <- "e_t"
page4_pn$trans[page4_pn$trans$name == "e_t", c("x", "y")] <- c(15, 10)
page4_pn$trans$name[6] <- "e_c"
page4_pn$trans[page4_pn$trans$name == "e_c", c("x", "y")] <- c(15, 0)
page4_pn$trans$name[3] <- "c_t"
page4_pn$trans[page4_pn$trans$name == "c_t", c("x", "y")] <- c(15, -5)
page4_pn$places[page4_pn$places$name == "p3", c("x", "y")] <- c(20, 5)
page4_pn$places[page4_pn$places$name == "p4", c("x", "y")] <- c(20, -5)
page4_pn$trans$name[4] <- "dcd"
page4_pn$trans[page4_pn$trans$name == "dcd", c("x", "y")] <- c(25, 0)
page4_pn$trans$name[8] <- "rei_r"
page4_pn$trans[page4_pn$trans$name == "rei_r", c("x", "y")] <- c(25, -10)
page4_pn$places[page4_pn$places$name == "p5", c("x", "y")] <- c(30, 0)
page4_pn$trans$name[5] <- "p_c"
page4_pn$trans[page4_pn$trans$name == "p_c", c("x", "y")] <- c(35, 5)
page4_pn$trans$name[7] <- "rej_r"
page4_pn$trans[page4_pn$trans$name == "rej_r", c("x", "y")] <- c(35, -5)
page4_pn$places[page4_pn$places$name == "end", c("x", "y")] <- c(40, 0)
ggplot.petrinet(page4_pn)

#page5
page5_traces_df <- data.frame(trace=c(
    rep("a,c,e,g", 2),
    rep("a,e,c,g", 3),
    rep("b,d,f,g", 2),
    rep("b,f,d,g", 4)
), stringsAsFactors=FALSE)
print(page5_traces_df)
page5_pn <- alpha.petrinet(page5_traces_df)
#page4_pn$trans$name[7] <- "rej_r"
page5_pn$trans[page5_pn$trans$name == "a", c("x", "y")] <- c(5, 5)
page5_pn$trans[page5_pn$trans$name == "b", c("x", "y")] <- c(5, -5)
page5_pn$places[page5_pn$places$name == "p1", c("x", "y")] <- c(10, 10)
page5_pn$places[page5_pn$places$name == "p3", c("x", "y")] <- c(10, 5)
page5_pn$places[page5_pn$places$name == "p2", c("x", "y")] <- c(10, -5)
page5_pn$places[page5_pn$places$name == "p4", c("x", "y")] <- c(10, -10)
page5_pn$trans[page5_pn$trans$name == "c", c("x", "y")] <- c(15, 10)
page5_pn$trans[page5_pn$trans$name == "d", c("x", "y")] <- c(15, 5)
page5_pn$trans[page5_pn$trans$name == "e", c("x", "y")] <- c(15, -5)
page5_pn$trans[page5_pn$trans$name == "f", c("x", "y")] <- c(15, -10)
page5_pn$places[page5_pn$places$name == "p5", c("x", "y")] <- c(20, 10)
page5_pn$places[page5_pn$places$name == "p6", c("x", "y")] <- c(20, 0)
page5_pn$places[page5_pn$places$name == "p7", c("x", "y")] <- c(20, -10)
page5_pn$trans[page5_pn$trans$name == "g", c("x", "y")] <- c(25, 0)
page5_pn$places[page5_pn$places$name == "end", c("x", "y")] <- c(30, 0)
ggplot.petrinet(page5_pn)

traces_df <- page5_traces_df
L_mtrx <- footprint.traces(traces_df)
print(L_mtrx)
Tl <- unique.trans(traces_df); print(Tl)
Ti <- first.trans(traces_df);  print(Ti)
To <- last.trans(traces_df);   print(To)
Xl_df <- Xl.pairs(L_mtrx); print(Xl_df)
Yl_df <- select_maximal_pairs(Xl_df, dups_col="A", sets_col="B")
Yl_df <- select_maximal_pairs(Yl_df, dups_col="B", sets_col="A")
print(Yl_df)

#page6
page6_traces_df <- data.frame(trace=c(
    rep("a,c", 2),
    rep("a,b,c", 3),
    rep("a,b,b,c", 2),
    rep("a,b,b,b,b,c", 1)
), stringsAsFactors=FALSE)
print(page6_traces_df)
page6_pn <- alpha.petrinet(page6_traces_df)
#page4_pn$trans$name[7] <- "rej_r"
page6_pn$trans[page6_pn$trans$name == "a", c("x", "y")] <- c(5, 0)
page6_pn$trans[page6_pn$trans$name == "b", c("x", "y")] <- c(10, 5)
page6_pn$places[page6_pn$places$name == "p1", c("x", "y")] <- c(10, 0)
page6_pn$trans[page6_pn$trans$name == "c", c("x", "y")] <- c(15, 0)
page6_pn$places[page6_pn$places$name == "end", c("x", "y")] <- c(20, 0)
ggplot.petrinet(page6_pn)

#page7
page7_traces_df <- data.frame(trace=c(
    rep("a,b,d", 3),
    rep("a,b,c,b,d", 2),
    rep("a,b,c,b,c,b,d", 1)
), stringsAsFactors=FALSE)
print(page7_traces_df)
page7_pn <- alpha.petrinet(page7_traces_df)
#page4_pn$trans$name[7] <- "rej_r"
page7_pn$trans[page7_pn$trans$name == "a", c("x", "y")] <- c(5, 0)
page7_pn$places[page7_pn$places$name == "p1", c("x", "y")] <- c(10, 0)
page7_pn$trans[page7_pn$trans$name == "b", c("x", "y")] <- c(15, 0)
page7_pn$trans[page7_pn$trans$name == "c", c("x", "y")] <- c(15, 5)
page7_pn$places[page7_pn$places$name == "p2", c("x", "y")] <- c(20, 0)
page7_pn$trans[page7_pn$trans$name == "d", c("x", "y")] <- c(25, 0)
page7_pn$places[page7_pn$places$name == "end", c("x", "y")] <- c(30, 0)
ggplot.petrinet(page7_pn)

#page8
page8_traces_df <- data.frame(trace=c(
    rep("a,c,d", 3),
    rep("b,c,e", 3)
), stringsAsFactors=FALSE)
print(page8_traces_df)
page8_pn <- alpha.petrinet(page8_traces_df)
#page4_pn$trans$name[7] <- "rej_r"
page8_pn$trans[page8_pn$trans$name == "a", c("x", "y")] <- c(5, 5)
page8_pn$trans[page8_pn$trans$name == "b", c("x", "y")] <- c(5, -5)
page8_pn$places[page8_pn$places$name == "p1", c("x", "y")] <- c(10, 0)
page8_pn$trans[page8_pn$trans$name == "c", c("x", "y")] <- c(15, 0)
page8_pn$places[page8_pn$places$name == "p2", c("x", "y")] <- c(20, 0)
page8_pn$trans[page8_pn$trans$name == "d", c("x", "y")] <- c(25, 5)
page8_pn$trans[page8_pn$trans$name == "e", c("x", "y")] <- c(25, -5)
page8_pn$places[page8_pn$places$name == "end", c("x", "y")] <- c(30, 0)
ggplot.petrinet(page8_pn)

#page10
page10_traces_df <- data.frame(trace=c(
    rep("a,c,d", 3),
    rep("b,c,d", 3),
    rep("a,c,e", 3),
    rep("b,c,e", 3)
), stringsAsFactors=FALSE)
print(page10_traces_df)
page10_pn <- alpha.petrinet(page10_traces_df)
#page4_pn$trans$name[7] <- "rej_r"
page10_pn$trans[page10_pn$trans$name == "a", c("x", "y")] <- c(5, 5)
page10_pn$trans[page10_pn$trans$name == "b", c("x", "y")] <- c(5, -5)
page10_pn$places[page10_pn$places$name == "p1", c("x", "y")] <- c(10, 0)
page10_pn$trans[page10_pn$trans$name == "c", c("x", "y")] <- c(15, 0)
page10_pn$places[page10_pn$places$name == "p2", c("x", "y")] <- c(20, 0)
page10_pn$trans[page10_pn$trans$name == "d", c("x", "y")] <- c(25, 5)
page10_pn$trans[page10_pn$trans$name == "e", c("x", "y")] <- c(25, -5)
page10_pn$places[page10_pn$places$name == "end", c("x", "y")] <- c(30, 0)
ggplot.petrinet(page10_pn)

#page13
page13_traces_df <- data.frame(trace=c(
    rep("a,c,d", 3),
    rep("b,c,e", 3),
    rep("a,c,e", 3)
), stringsAsFactors=FALSE)
print(page13_traces_df)
page13_pn <- alpha.petrinet(page13_traces_df)
#page4_pn$trans$name[7] <- "rej_r"
page13_pn$trans[page13_pn$trans$name == "a", c("x", "y")] <- c(5, 5)
page13_pn$trans[page13_pn$trans$name == "b", c("x", "y")] <- c(5, -5)
page13_pn$places[page13_pn$places$name == "p1", c("x", "y")] <- c(10, 0)
page13_pn$trans[page13_pn$trans$name == "c", c("x", "y")] <- c(15, 0)
page13_pn$places[page13_pn$places$name == "p2", c("x", "y")] <- c(20, 0)
page13_pn$trans[page13_pn$trans$name == "d", c("x", "y")] <- c(25, 5)
page13_pn$trans[page13_pn$trans$name == "e", c("x", "y")] <- c(25, -5)
page13_pn$places[page13_pn$places$name == "end", c("x", "y")] <- c(30, 0)
ggplot.petrinet(page13_pn)

#page14
page14_pn <-
    petrinet(name="page14",
             trans_df=data.frame(id=1:6,
                                 name=c("a", "b", "c", "d", "e", "e"),
                                 x=c(    5,   5,   15,  25,  25,  25),
                                 y=c(    5,  -5,    0,   5,   0,  -5),
                                 stringsAsFactors=FALSE),
             places_df=data.frame(id=1:6,
                                  name=c("start", "p1", "p2", "p3", "p4", "end"),
                                  x=c(    0,       10,   15,   15,   20,   30),
                                  y=c(    0,        0,    5,   -5,    0,    0),
                                  M0=c(   1,        0,    0,    0,    0,    0),
                                  stringsAsFactors=FALSE),
             arcs_df=data.frame(
    begin=c("start","start","a", "a", "b", "b", "p1","p2","p2","c", "p3","p4","p4","p4","d",  "e",  "e"),
    end  =c("a",    "b",    "p1","p2","p1","p3","c", "d", "e", "p4","e", "d", "e"
,"e", "end","end","end"),
                 stringsAsFactors=FALSE))
ggplot.petrinet(page14_pn)

page14_2__pn <-
    petrinet(name="page14_2_",
             trans_df=data.frame(id=1:6,
                                 name=c("a", "b", "c", "d", "e1", "e2"),
                                 x=c(    5,   5,   15,  25,  25,  25),
                                 y=c(    5,  -5,    0,   5,   0,  -5),
                                 stringsAsFactors=FALSE),
             places_df=data.frame(id=1:6,
                                  name=c("start", "p1", "p2", "p3", "p4", "end"),
                                  x=c(    0,       10,   15,   15,   20,   30),
                                  y=c(    0,        0,    5,   -5,    0,    0),
                                  M0=c(   1,        0,    0,    0,    0,    0),
                                  stringsAsFactors=FALSE),
             arcs_df=data.frame(
                 begin=c("start","start","a", "a", "b", "b", "p1","p2","p2","c"
,"p3","p4","p4","p4","d",  "e1",  "e2"),
                 end  =c("a",    "b",    "p1","p2","p1","p3","c", "d", "e1","p4","e2","d", "e1","e2","end","end","end"),
                 stringsAsFactors=FALSE))
ggplot.petrinet(page14_2__pn)

#page15
page15_traces_df <- data.frame(trace=c(
    rep("a,a", 2)
), stringsAsFactors=FALSE)
print(page15_traces_df)
page15_pn <- alpha.petrinet(page15_traces_df)
#page4_pn$trans$name[7] <- "rej_r"
page15_pn$trans[page15_pn$trans$name == "a", c("x", "y")] <- c(5, 5)
page15_pn$trans[page15_pn$trans$name == "b", c("x", "y")] <- c(5, -5)
page15_pn$places[page15_pn$places$name == "p1", c("x", "y")] <- c(10, 0)
page15_pn$trans[page15_pn$trans$name == "c", c("x", "y")] <- c(15, 0)
page15_pn$places[page15_pn$places$name == "p2", c("x", "y")] <- c(20, 0)
page15_pn$trans[page15_pn$trans$name == "d", c("x", "y")] <- c(25, 5)
page15_pn$trans[page15_pn$trans$name == "e", c("x", "y")] <- c(25, -5)
page15_pn$places[page15_pn$places$name == "end", c("x", "y")] <- c(30, 0)
ggplot.petrinet(page15_pn)

traces_df <- page15_traces_df
L_mtrx <- footprint.traces(traces_df); print(L_mtrx)
Tl <- unique.trans(traces_df); print(Tl)
Ti <- first.trans(traces_df);  print(Ti)
To <- last.trans(traces_df);   print(To)
Xl_df <- Xl.pairs(L_mtrx); print(Xl_df)
Yl_df <- select_maximal_pairs(Xl_df, dups_col="A", sets_col="B")
Yl_df <- select_maximal_pairs(Yl_df, dups_col="B", sets_col="A")

#page16
page16_traces_df <- data.frame(trace=c(
    rep("a,b,c", 2),
    rep("a,c", 2)
), stringsAsFactors=FALSE)
print(page16_traces_df)
page16_pn <- alpha.petrinet(page16_traces_df)
#page4_pn$trans$name[7] <- "rej_r"
page16_pn$trans[page16_pn$trans$name == "a", c("x", "y")] <- c(5, 0)
page16_pn$trans[page16_pn$trans$name == "b", c("x", "y")] <- c(15, 0)
page16_pn$places[page16_pn$places$name == "p2", c("x", "y")] <- c(15, 5)
page16_pn$trans[page16_pn$trans$name == "c", c("x", "y")] <- c(25, 0)
page16_pn$places[page16_pn$places$name == "p3", c("x", "y")] <- c(20, 0)
page16_pn$places[page16_pn$places$name == "end", c("x", "y")] <- c(30, 0)
ggplot.petrinet(page16_pn)

#page18
page18_traces_df <- data.frame(trace=c(
    rep("a,b,d", 2),
    rep("a,c,d", 2),
    rep("a,b,c,d", 2),
    rep("a,c,b,d", 2)
), stringsAsFactors=FALSE)
print(page18_traces_df)
page18_pn <- alpha.petrinet(page18_traces_df)
#page4_pn$trans$name[7] <- "rej_r"
page18_pn$trans[page18_pn$trans$name == "a", c("x", "y")] <- c(5, 0)
page18_pn$places[page18_pn$places$name == "p1", c("x", "y")] <- c(10, 5)
page18_pn$places[page18_pn$places$name == "p2", c("x", "y")] <- c(10, -5)
page18_pn$trans[page18_pn$trans$name == "b", c("x", "y")] <- c(15, 5)
page18_pn$trans[page18_pn$trans$name == "c", c("x", "y")] <- c(15, -5)
page18_pn$places[page18_pn$places$name == "p3", c("x", "y")] <- c(20, 5)
page18_pn$places[page18_pn$places$name == "p4", c("x", "y")] <- c(20, -5)
page18_pn$trans[page18_pn$trans$name == "d", c("x", "y")] <- c(25, 0)
page18_pn$places[page18_pn$places$name == "end", c("x", "y")] <- c(30, 0)
ggplot.petrinet(page18_pn)

#page20
page20_traces_df <- data.frame(trace=c(
    rep("a,b,d,e,f", 2),
    rep("a,c,e,d,f", 2)
), stringsAsFactors=FALSE)
print(page20_traces_df)
page20_pn <- alpha.petrinet(page20_traces_df)
#page4_pn$trans$name[7] <- "rej_r"
page20_pn$trans[page20_pn$trans$name == "a", c("x", "y")] <- c(5, 0)
page20_pn$trans[page20_pn$trans$name == "b", c("x", "y")] <- c(15, 5)
page20_pn$trans[page20_pn$trans$name == "c", c("x", "y")] <- c(15, -5)
page20_pn$places[page20_pn$places$name == "p2", c("x", "y")] <- c(20, 5)
page20_pn$places[page20_pn$places$name == "p3", c("x", "y")] <- c(20, -5)
page20_pn$trans[page20_pn$trans$name == "d", c("x", "y")] <- c(25, 5)
page20_pn$trans[page20_pn$trans$name == "e", c("x", "y")] <- c(25, -5)
page20_pn$places[page20_pn$places$name == "p4", c("x", "y")] <- c(30, 5)
page20_pn$places[page20_pn$places$name == "p5", c("x", "y")] <- c(30, -5)
page20_pn$trans[page20_pn$trans$name == "f", c("x", "y")] <- c(35, 0)
page20_pn$places[page20_pn$places$name == "end", c("x", "y")] <- c(40, 0)
ggplot.petrinet(page20_pn)

replay.petrisim(pn=page20_pn, replay.trans=c("a","b","d"))
replay.petrisim(pn=page20_pn, replay.trans=c("a","c"))

#page22
page22_pn <-
    petrinet(name="page22",
             trans_df=data.frame(id=1:6,
                                 name=c("start", "end", "a", "b", "c", "d"),
                                 x=c(    5,       15,     5,  15,   5,  15),
                                 y=c(    0,        0,     5,   5,  -5,  -5),
                                 stringsAsFactors=FALSE),
             places_df=data.frame(id=1:3,
                                  name=c("p1", "p2", "p3"),
                                  x=c(    0,    10,   20),
                                  y=c(    0,     0,    0),
                                  M0=c(   1,     0,    0),
                                  stringsAsFactors=FALSE),
             arcs_df=data.frame(
    begin=c("p1",   "start","a", "c", "p2","p2","p2", "p2","p2","b", "d", "end"),
    end  =c("start","p2",   "p2","p2","a", "b", "end","c", "d", "p2","p2","p3"),
                 stringsAsFactors=FALSE))
ggplot.petrinet(page22_pn)

#page23
page23_traces_df <- data.frame(trace=c(
    rep("a,c,d", 2),
    rep("b,c,e", 2)
), stringsAsFactors=FALSE)
print(page23_traces_df)
page23_pn <- alpha.petrinet(page23_traces_df)
#page4_pn$trans$name[7] <- "rej_r"
page23_pn$trans[page23_pn$trans$name == "a", c("x", "y")] <- c(5, 5)
page23_pn$places[page23_pn$places$name == "p1", c("x", "y")] <- c(5, 0)
page23_pn$trans[page23_pn$trans$name == "b", c("x", "y")] <- c(5, -5)
page23_pn$trans[page23_pn$trans$name == "c", c("x", "y")] <- c(10, 0)
page23_pn$trans[page23_pn$trans$name == "d", c("x", "y")] <- c(15, 5)
page23_pn$places[page23_pn$places$name == "p2", c("x", "y")] <- c(15, 0)
page23_pn$trans[page23_pn$trans$name == "e", c("x", "y")] <- c(15, -5)
page23_pn$places[page23_pn$places$name == "end", c("x", "y")] <- c(20, 0)
ggplot.petrinet(page23_pn)

#page24
page24_traces_df <- data.frame(trace=c(
    rep("a,c,d", 2),
    rep("a,c,e", 2),
    rep("b,c,e", 2),
    rep("b,c,d", 2)
), stringsAsFactors=FALSE)
print(page24_traces_df)
page24_pn <- alpha.petrinet(page24_traces_df)
#page4_pn$trans$name[7] <- "rej_r"
page24_pn$trans[page24_pn$trans$name == "a", c("x", "y")] <- c(5, 5)
page24_pn$places[page24_pn$places$name == "p1", c("x", "y")] <- c(5, 0)
page24_pn$trans[page24_pn$trans$name == "b", c("x", "y")] <- c(5, -5)
page24_pn$trans[page24_pn$trans$name == "c", c("x", "y")] <- c(10, 0)
page24_pn$trans[page24_pn$trans$name == "d", c("x", "y")] <- c(15, 5)
page24_pn$places[page24_pn$places$name == "p2", c("x", "y")] <- c(15, 0)
page24_pn$trans[page24_pn$trans$name == "e", c("x", "y")] <- c(15, -5)
page24_pn$places[page24_pn$places$name == "end", c("x", "y")] <- c(20, 0)
ggplot.petrinet(page24_pn)
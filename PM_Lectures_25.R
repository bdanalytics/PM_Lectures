#rm(list=ls())
setwd("~/Documents/Work/Courses/Coursera/process-mining/Projects/PM_Lectures")
source("~/Dropbox/datascience/R/mypetrinet.R")

## page_9
page_9_pn <-
    petrinet(name="page_9",
             trans_df=data.frame(id=1:8,
        name=c("reg_r", "e_t", "e_c", "c_t", "d", "rei_r", "p_c", "rej_r"),
        x=c(   5,       15,    15,    15,    25,  25,      35,    35),
        y=c(   10,      20,    10,     5,    10,   0,      15,     5),
                                 stringsAsFactors=FALSE),
             places_df=data.frame(id=1:7,
        name=c("start", "c1", "c2", "c3", "c4", "c5", "end"),
        x=c(    0,      10,   10,   20,   20,   30,   40),
        y=c(   10,      15,    5,   15,    5,   10,   10),
        M0=c(   1,       0,    0,    0,    0,    0,    0),
                                  stringsAsFactors=FALSE),
             arcs_df=data.frame(
        begin=c("start", "reg_r", "reg_r", "c1",  "c1",  "c2",  "e_t", "e_c", "c_t"
, "c3", "c4", "d",  "rei_r", "rei_r", "c5",    "c5",  "c5",    "p_c", "rej_r"),
        end  =c("reg_r", "c1",    "c2",    "e_t", "e_c", "c_t", "c3",  "c3",  "c4",
  "d",  "d",  "c5", "c1",    "c2",    "rei_r", "p_c", "rej_r", "end", "end"),
                 stringsAsFactors=FALSE))
#plot.petrinet(x=page_9_pn)
ggplot.petrinet(pn=page_9_pn)
replay.petrisim(pn=page_9_pn, replay.trans=c("reg_r", "e_t", "c_t", "d", "p_c"))

## page_12
page_12_pn <-
    petrinet(name="page_12",
             trans_df=data.frame(id=1:8,
                name=c("reg_r", "e_t", "e_c", "c_t", "d", "rei_r", "p_c", "rej_r"),
                x=c(   5,       15,    15,    15,    25,  25,      35,    35),
                y=c(   10,      20,    10,     5,    10,   0,      15,     5),
                                 stringsAsFactors=FALSE),
             places_df=data.frame(id=1:7,
                            name=c("start", "c1", "c2", "c3", "c4", "c5", "end"),
                            x=c(    0,      10,   10,   20,   20,   30,   40),
                            y=c(   10,      15,    5,   15,    5,   10,   10),
                            M0=c(   1,       0,    0,    0,    0,    0,    0),
                                  stringsAsFactors=FALSE),
             arcs_df=data.frame(
        begin=c("start", "reg_r", "reg_r", "c1",  "c1",  "c2",  "e_t", "e_c", "c_t"
, "c3", "c4", "d",  "rei_r", "c5",    "c5",  "c5",    "p_c", "rej_r"),
        end  =c("reg_r", "c1",    "c2",    "e_t", "e_c", "c_t", "c3",  "c3",  "c4",
  "d",  "d",  "c5", "c1",    "rei_r", "p_c", "rej_r", "end", "end"),
                 stringsAsFactors=FALSE))
ggplot.petrinet(pn=page_12_pn)
replay.petrisim(pn=page_12_pn, replay.trans=c("reg_r", "e_t", "c_t", "d", "p_c"))
replay.petrisim(pn=page_12_pn, replay.trans=c("reg_r", "e_t", "c_t", "d", "rej_r"))

replay.petrisim(pn=page_12_pn, replay.trans=c("reg_r", "e_t", "c_t", "d", "rei_r", "e_t")) # deadlock
replay.petrisim(pn=page_12_pn, replay.trans=c("reg_r", "e_t", "c_t", "d", "rei_r", "e_c"))
replay.petrisim(pn=page_12_pn, replay.trans=c("reg_r", "e_c"))
replay.petrisim(pn=page_12_pn, replay.trans=c("reg_r", "c_t"))

## page_13
page_13_pn <-
    petrinet(name="page_13",
             trans_df=data.frame(id=1:8,
                name=c("reg_r", "e_t", "e_c", "c_t", "d", "rei_r", "p_c", "rej_r"),
                x=c(   5,       15,    15,    15,    25,  25,      35,    35),
                y=c(   10,      20,    10,     5,    10,   0,      15,     5),
                                 stringsAsFactors=FALSE),
             places_df=data.frame(id=1:6,
                        name=c("start", "c1", "c2", "c3", "c5", "end"),
                        x=c(    0,      10,   10,   20,   30,   40),
                        y=c(   10,      15,    5,   15,   10,   10),
                        M0=c(   1,       0,    0,    0,    0,    0),
                                  stringsAsFactors=FALSE),
             arcs_df=data.frame(
        begin=c("start", "reg_r", "reg_r", "c1",  "c1",  "c2",  "e_t", "e_c", "c_t"
, "c3", "d",  "rei_r", "rei_r", "c5",    "c5",  "c5",    "p_c", "rej_r"),
        end  =c("reg_r", "c1",    "c2",    "e_t", "e_c", "c_t", "c3",  "c3",  "c3",
  "d",  "c5", "c1",    "c2",    "rei_r", "p_c", "rej_r", "end", "end"),
                 stringsAsFactors=FALSE))
ggplot.petrinet(pn=page_13_pn)
replay.petrisim(pn=page_13_pn, replay.trans=c("reg_r", "e_t", "d", "p_c")) # c2 has a token
replay.petrisim(pn=page_13_pn, replay.trans=c("reg_r", "e_t", "d", "rej_r"))
replay.petrisim(pn=page_13_pn, replay.trans=c("reg_r", "e_t", "d", "rei_r"))
replay.petrisim(pn=page_13_pn, replay.trans=c("reg_r", "e_t", "d", "c_t"))
replay.petrisim(pn=page_13_pn, replay.trans=c("reg_r", "e_t", "c_t"))
replay.petrisim(pn=page_13_pn, replay.trans=c("reg_r", "e_c"))
replay.petrisim(pn=page_13_pn, replay.trans=c("reg_r", "c_t"))

## page_14
page_14_pn <-
    petrinet(name="page_14",
             trans_df=data.frame(id=1:8,
            name=c("reg_r", "e_t", "e_c", "c_t", "d", "rei_r", "p_c", "rej_r"),
            x=c(   5,       15,    15,    15,    25,  25,      35,    35),
            y=c(   10,      20,    10,     5,    10,   0,      15,     5),
                                 stringsAsFactors=FALSE),
             places_df=data.frame(id=1:7,
            name=c("start", "c1", "c2", "c3", "c4", "c5", "end"),
            x=c(    0,      10,   10,   20,   20,   30,   40),
            y=c(   10,      15,    5,   15,    5,   10,   10),
            M0=c(   1,       0,    0,    0,    0,    0,    0),
                                  stringsAsFactors=FALSE),
             arcs_df=data.frame(
    begin=c("start", "reg_r", "reg_r", "c1",  "c1",  "c2",  "e_t", "e_c", "c_t"
, "c3", "c4", "d",  "rei_r", "rei_r", "c5",    "c5",  "c5",    "p_c", "rej_r", "c3"),
    end  =c("reg_r", "c1",    "c2",    "e_t", "e_c", "c_t", "c3",  "c3",  "c4",
  "d",  "d",  "c5", "c1",    "c2",    "rei_r", "p_c", "rej_r", "end", "end",     "p_c"),
                 stringsAsFactors=FALSE))
ggplot.petrinet(pn=page_14_pn)
replay.petrisim(pn=page_14_pn, replay.trans=c("reg_r", "e_t", "c_t", "d", "rej_r"))
replay.petrisim(pn=page_14_pn, replay.trans=c("reg_r", "e_t", "c_t", "d", "rei_r")) # enabled trans are "e_t", "e_c", "c_t"; similar to after "reg_r"
replay.petrisim(pn=page_14_pn, replay.trans=c("reg_r", "e_c", "c_t", "d", "rej_r"))
replay.petrisim(pn=page_14_pn, replay.trans=c("reg_r", "e_c", "c_t", "d", "rei_r")) # enabled trans are "e_t", "e_c", "c_t"; similar to after "reg_r"
replay.petrisim(pn=page_14_pn, replay.trans=c("reg_r", "c_t", "e_t", "d", "rej_r"))
replay.petrisim(pn=page_14_pn, replay.trans=c("reg_r", "c_t", "e_t", "d", "rei_r")) # enabled trans are "e_t", "e_c", "c_t"; similar to after "reg_r"
replay.petrisim(pn=page_14_pn, replay.trans=c("reg_r", "c_t", "e_c", "d", "rej_r"))
replay.petrisim(pn=page_14_pn, replay.trans=c("reg_r", "c_t", "e_c", "d", "rei_r")) # enabled trans are "e_t", "e_c", "c_t"; similar to after "reg_r"
#### p_c is dead

## page_18
page_18_pn <-
    petrinet(name="page_18",
            trans_df=data.frame(id=1:6,
                name=c("t1", "t2", "t3", "t4", "t5", "t6"),
                x=c(     5,    5,    15,  15,   25,   25),
                y=c(    10,    0,    10,   0,   10,    0),
                                 stringsAsFactors=FALSE),
            places_df=data.frame(id=1:6,
                name=c("p1", "p2", "p3", "p4", "p5", "p6"),
                x=c(     0,   10,   10,   20,   20,   30),
                y=c(     5,   10,    0,   10,    0,    5),
                M0=c(    1,    0,    0,    0,    0,    0),
                                  stringsAsFactors=FALSE),
             arcs_df=data.frame(
    begin=c("p1", "p1", "t1", "t1", "t2", "p2", "p3", "t3", "t4", "p4", "p5", "p5", "t5", "t6"),
    end  =c("t1", "t2", "p2", "p3", "p3", "t3", "t4", "p4", "p5", "t5", "t5", "t6", "p6", "p6"),
                 stringsAsFactors=FALSE))
ggplot.petrinet(pn=page_18_pn)
replay.petrisim(pn=page_18_pn, replay.trans=c("t1", "t3", "t4", "t5"))
replay.petrisim(pn=page_18_pn, replay.trans=c("t1", "t3", "t4", "t6")) # improper completion; token left in p4
replay.petrisim(pn=page_18_pn, replay.trans=c("t1", "t4", "t3", "t5"))
replay.petrisim(pn=page_18_pn, replay.trans=c("t1", "t4", "t3", "t6")) # improper completion; token left in p4
replay.petrisim(pn=page_18_pn, replay.trans=c("t1", "t4", "t6", "t3")) # improper completion; token left in p4
replay.petrisim(pn=page_18_pn, replay.trans=c("t2", "t4", "t6"))

## page_19
page_19_pn <-
    petrinet(name="page_19",
             trans_df=data.frame(id=1:3,
                                 name=c("t1", "t2", "t3"),
                                 x=c(     5,   15,   10),
                                 y=c(     0,    0,    5),
                                 stringsAsFactors=FALSE),
             places_df=data.frame(id=1:5,
                                  name=c("p1", "p2", "p3", "p4", "p5"),
                                  x=c(     0,   10,    5,   15,   20),
                                  y=c(     0,    0,    5,    5,    0),
                                  M0=c(    1,    0,    0,    0,    0),
                                  stringsAsFactors=FALSE),
             arcs_df=data.frame(
                 begin=c("p1", "p3", "t1", "t1", "t3", "t3", "p2", "p4", "t2"),
                 end  =c("t1", "t3", "p3", "p2", "p3", "p4", "t2", "t2", "p5"),
                 stringsAsFactors=FALSE))
ggplot.petrinet(pn=page_19_pn)
replay.petrisim(pn=page_19_pn, replay.trans=c("t1", "t3", "t2")) # p3 has token
replay.petrisim(pn=page_19_pn, replay.trans=c("t1", "t3", "t3", "t2")) # p4 had 2 tokens; p3 & p4 has tokens;
replay.petrisim(pn=page_19_pn, replay.trans=c("t1", "t3", "t3", "t3"))

## page_20
page_20_pn <-
    petrinet(name="page_20",
             trans_df=data.frame(id=1:3,
                                 name=c("t1", "t2", "t3"),
                                 x=c(     5,   15,   10),
                                 y=c(     0,    0,    5),
                                 stringsAsFactors=FALSE),
             places_df=data.frame(id=1:3,
                                  name=c("p1", "p2", "p3"),
                                  x=c(     0,   10,   20),
                                  y=c(     0,    0,    0),
                                  M0=c(    1,    0,    0),
                                  stringsAsFactors=FALSE),
             arcs_df=data.frame(
                 begin=c("p1", "p1", "t1", "t3", "p2", "p2", "t2"),
                 end  =c("t1", "t3", "p2", "p3", "t3", "t2", "p3"),
                 stringsAsFactors=FALSE))
ggplot.petrinet(pn=page_20_pn)
replay.petrisim(pn=page_20_pn, replay.trans=c("t1", "t2")) # t3 is dead

## page_21
page_21_pn <-
    petrinet(name="page_21",
             trans_df=data.frame(id=1:8,
            name=c("reg_r", "e_t", "e_c", "c_t", "d", "rei_r", "p_c", "rej_r"),
            x=c(   5,       15,    15,    15,    25,  25,      35,    35),
            y=c(   10,      20,    10,     5,    10,   0,      15,     5),
                                 stringsAsFactors=FALSE),
             places_df=data.frame(id=1:7,
            name=c("start", "c1", "c2", "c3", "c4", "c5", "end"),
            x=c(    0,      10,   10,   20,   20,   30,   40),
            y=c(   10,      15,    5,   15,    5,   10,   10),
            M0=c(   1,       0,    0,    0,    0,    0,    0),
                                  stringsAsFactors=FALSE),
             arcs_df=data.frame(
    begin=c("start", "reg_r", "reg_r", "c1",  "c1",  "c2",  "e_t", "e_c", "c_t"
, "c3", "c4", "d",  "rei_r", "rei_r", "c5",    "c5",  "c5",    "p_c", "rej_r"),
    end  =c("reg_r", "c1",    "c2",    "e_t", "e_c", "c_t", "c3",  "c3",  "c4",
  "d",  "d",  "c5", "c1",    "c2",    "rei_r", "p_c", "rej_r", "end", "end"),
                 stringsAsFactors=FALSE))
ggplot.petrinet(pn=page_21_pn)
replay.petrisim(pn=page_21_pn, replay.trans=c("reg_r", "e_t", "c_t", "d", "p_c"))
replay.petrisim(pn=page_21_pn, replay.trans=c("reg_r", "e_t", "c_t", "d", "rej_r"))
replay.petrisim(pn=page_21_pn, replay.trans=c("reg_r", "e_t", "c_t", "d", "rei_r")) # enabled.trans "e_t", "e_c", "c_t"
replay.petrisim(pn=page_21_pn, replay.trans=c("reg_r", "e_c", "c_t", "d")) # enabled.trans "p_c", "rej_r", "rei_r"
replay.petrisim(pn=page_21_pn, replay.trans=c("reg_r", "c_t", "e_t", "d")) # enabled.trans "p_c", "rej_r", "rei_r"
replay.petrisim(pn=page_21_pn, replay.trans=c("reg_r", "c_t", "e_c", "d")) # enabled.trans "p_c", "rej_r", "rei_r"

## page_23
page_23_pn <-
    petrinet(name="page_23",
             trans_df=data.frame(id=1:8,
            name=c("reg_r", "e_t", "e_c", "c_t", "d", "rei_r", "p_c", "rej_r"),
            x=c(   5,       15,    15,    15,    25,  25,      35,    35),
            y=c(   10,      20,    10,     5,    10,   0,      15,     5),
                                 stringsAsFactors=FALSE),
             places_df=data.frame(id=1:7,
                            name=c("start", "c1", "c2", "c3", "c4", "c5", "end"),
                            x=c(    0,      10,   10,   20,   20,   30,   40),
                            y=c(   10,      15,    5,   15,    5,   10,   10),
                            M0=c(   1,       0,    0,    0,    0,    0,    0),
                                  stringsAsFactors=FALSE),
             arcs_df=data.frame(
    begin=c("start", "reg_r", "reg_r", "c1",  "c1",  "c2",  "e_t", "e_c", "c_t"
, "c3", "c4", "d",  "rei_r", "rei_r", "c5",    "c5",  "c5",    "p_c", "rej_r", "reg_r", "c5",  "c5"),
    end  =c("reg_r", "c1",    "c2",    "e_t", "e_c", "c_t", "c3",  "c3",  "c4",
  "d",  "d",  "c5", "c1",    "c2",    "rei_r", "p_c", "rej_r", "end", "end",   "c5",    "p_c", "rej_r"),
                 stringsAsFactors=FALSE))
ggplot.petrinet(pn=page_23_pn)
replay.petrisim(pn=page_23_pn, replay.trans=c("reg_r", "e_t", "c_t", "d")) #c5 unsafe
replay.petrisim(pn=page_23_pn, replay.trans=c("reg_r", "e_t", "c_t", "rei_r"))
replay.petrisim(pn=page_23_pn, replay.trans=c("reg_r", "e_t", "rei_r"))
replay.petrisim(pn=page_23_pn, replay.trans=c("reg_r", "e_c"))
replay.petrisim(pn=page_23_pn, replay.trans=c("reg_r", "c_t"))
replay.petrisim(pn=page_23_pn, replay.trans=c("reg_r", "rei_r"))

## page_24
page_24_pn <-
    petrinet(name="page_24",
             trans_df=data.frame(id=1:8,
            name=c("reg_r", "e_t", "e_c", "c_t", "d", "rei_r", "p_c", "rej_r"),
            x=c(   5,       15,    15,    15,    25,  25,      35,    35),
            y=c(   10,      20,    10,     5,    10,   0,      15,     5),
                                 stringsAsFactors=FALSE),
             places_df=data.frame(id=1:7,
                            name=c("start", "c1", "c2", "c3", "c4", "c5", "end"),
                            x=c(    0,      10,   10,   20,   20,   30,   40),
                            y=c(   10,      15,    5,   15,    5,   10,   10),
                            M0=c(   1,       0,    0,    0,    0,    0,    0),
                                  stringsAsFactors=FALSE),
             arcs_df=data.frame(
    begin=c("start", "reg_r", "reg_r", "c1",  "c1",  "c2",  "e_t", "e_c", "c_t"
, "c3", "c4", "d",  "rei_r", "rei_r", "c5",    "c5",  "c5",    "p_c", "rej_r", "c4"),
    end  =c("reg_r", "c1",    "c2",    "e_t", "e_c", "c_t", "c3",  "c3",  "c4",
  "d",  "d",  "c5", "c1",    "c2",    "rei_r", "p_c", "rej_r", "end", "end",
"e_c"),
                 stringsAsFactors=FALSE))
ggplot.petrinet(pn=page_24_pn)
replay.petrisim(pn=page_24_pn, replay.trans=c("reg_r", "e_t", "c_t", "d", "p_c"))
replay.petrisim(pn=page_24_pn, replay.trans=c("reg_r", "e_t", "c_t", "d", "rej_r"))
replay.petrisim(pn=page_24_pn, replay.trans=c("reg_r", "e_t", "c_t", "d", "rei_r")) #enabled.trans: e_t, c_t
replay.petrisim(pn=page_24_pn, replay.trans=c("reg_r", "c_t", "e_t", "d")) #enabled.trans: p_c, rej_r, rei_r

replay.petrisim(pn=page_24_pn, replay.trans=c("reg_r", "c_t", "e_c")) # no option to complete when c3 has 1 token

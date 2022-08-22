
1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
20
21
22
23
24
25
26
27
28
29
30
31
32
33
34
35
36
37
38
39
40
41
42
43
44
45
46
47
48
49
50
51
52
53
54
55
56
57
58
59
60
61
62
63
64
65
66
67
68
69
70
71
72
73
74
75
76
77
78
79
80
81
82
83
84
85
86
87
88
89
90
91
92
93
94
95
96
97
98
99
100
101
102
103
104
105
106
107
108
109
110
111
112
113
114
115
116
117
118
119
120
121
122
123
124
125
126
127
128
129
130
131
132
133
134
135
136
137
138
139
140
141
142
143
144
145
146
147
148
149
150
151
152
153
154
155
156
157
158
159
160
161
162
163
164
165
166
167
168
169
170
171
172
173
174
175
176
177
178
179
180
181
182
183
184
185
186
187
188
189
190
191
192
193
194
195
196
197
198
199
200
201
202
203
204
205
206
207
208
209
210
211
212
213
214
215
216
217
218
219
220
221
222
223
224
225
226
227
228
229
230
231
232
233
234
235
236
237
238
239
240
241
242
243
244
245
246
247
248
249
250
251
252
253
254
255
256
257
258
259
260
261
262
263
264
265
266
267
268
269
270
271
272
273
274
275
276
277
278
279
280
281
282
283
284
285
286
287
288
289
290
291
292
293
294
295
296
297
298
299
300
301
302
303
304
305
306
307
308
309
310
311
312
313
314
315
316
317
318
319
320
321
322
323
324
325
326
327
328
329
330
331
332
333
334
335
336
337
338
339
340
341
342
343
344
345
346
347
348
349
350
351
352
353
354
355
356
357
358
359
360
361
362
363
364
365
366
367
368
369
370
371
372
373
374
375
376
377
378
379
380
381
382
383
384
385
386
387
388
389
390
391
392
393
394
395
396
397
398
399
400
401
402
403
404
405
406
407
408
409
410
411
412
413
414
415
416
417
418
419
420
421
422
423
424
425
426
427
428
429
430
431
432
433
434
435
436
437
438
439
440
441
442
443
444
445
446
447
448
449
450
451
452
453
454
455
456
457
458
459
460
461
462
463
464
465
466
467
468
469
470
471
472
473
474
475
476
477
478
479
480
481
482
483
484
485
486
487
488
489
490
491
492
493
494
495
496
497
498
499
500
501
502
503
504
505
506
507
508
509
510
511
512
513
514
515
516
517
518
519
520
521
522
523
524
525
526
527
528
529
530
531
532
533
534
535
536
537
538
539
540
541
542
543
544
545
546
547
548
549
550
551
552
553
554
555
556
557
558
559
560
561
562
563
564
565
566
567
568
569
570
571
572
573
574
575
576
577
578
579
580
581
582
583
584
585
586
587
588
589
590
591
592
593
594
595
596
597
598
599
600
601
602
603
604
605
606
607
608
609
610
611
612
613
614
615
616
617
618
619
620
621
622
623
624
625
626
627
628
629
630
631
632
633
634
635
636
637
638
639
640
641
642
643
644
645
646
647
648
649
650
651
652
653
654
655
656
657
658
659
660
661
662
663
664
665
666
667
668
669
670
671
672
673
674
675
676
677
678
679
680
681
682
683
684
685
686
687
688
689
690
691
692
693
694
695
696
697
698
699
700
701
702
703
704
705
706
707
708
709
710
711
712
713
714
715
716
717
718
719
720
721
722
723
724
725
726
727
728
729
730
731
732
733
734
735
736
737
738
739
740
741
742
743
744
745
746
747
748
749
750
751
752
753
754
755
756
757
758
759
760
761
762
763
764
765
766
767
768
library(shiny)
library(ggthemes)
library(ggplot2)
library(dplyr)
#library( selfishBoids)
library(shinybusy)

############## FUNCTIONS! ############# 

flock.pred = function (  N = NULL ,
                         zones = NULL,
                         speeds = NULL,
                         avoidance.weight = NULL,
                         prop.d = NULL,
                         n.timesteps = NULL ,# how many timesteps in each model?
                         field.of.inperception = NULL,
                         max.turn.rate = NULL,
                         pred.speed = NULL,
                         eta.predate = NULL,
                         pred.view = NULL, 
                         max.pred.turn.rate = NULL,
                         plot =F,
                         pred.catch = 1, 
                         future.predict.multip = NULL ,# no future prediction if = 0
                         future.predict.multip.pred = NULL,
                         grid.size = 100,
                         starting.pos.sd = NULL,
                         eta = NULL,
                         pred.start = NULL,
                         pred.Ra = NULL){
  
  # N = N
  # zones =zones
  # speeds = rep ( input$v,N)
  # avoidance.weight = rep(input$w, N)
  # field.of.inperception = field.of.inperception
  # max.turn.rate = input$max.turn.rate
  # n.timesteps = 100
  # pred.view = input$pred.view
  # eta.predate = input$eta.predate
  # max.pred.turn.rate = input$max.pred.turn.rate
  # pred.speed =  input$pred.speed
  # grid.size = 100
  # starting.pos.sd = input$starting.pos.sd
  # future.predict.multip =  input$future.predict.multip
  # future.predict.multip.pred = input$future.predict.multip.pred
  # eta = input$eta
  # plot = F
  # pred.Ra = input$pred.Ra
  
  { # skip to loop
    
    # set up agents , params
    
    
    # set speed
    heading = suppressWarnings( as.numeric(  circular::rvonmises(N,pi,0)))-pi
    vel    =  matrix(c( speeds * sin(heading),
                        speeds * cos(heading)),N,2)
    
    # set positions
    pos = matrix(c( rnorm( N,grid.size/2,starting.pos.sd),
                    rnorm( N,grid.size/2,starting.pos.sd)), N,2)
    
    # set predator head and pos
    pred.pos = runif(2, min=0,max=grid.size)
    pred.head= suppressWarnings( as.numeric ( circular::rvonmises(1,1,0))-pi)
    pred.vel = c(pred.speed * sin( pred.head),
                 pred.speed * cos( pred.head))
    
    # for storing data
    pos.store = array(NA, c(N,2,n.timesteps))
    pred.store = array(NA, c(2,n.timesteps))
    
    # store initial position
    pos.store [,,1] = pos
    pred.store [,1] = pred.pos
    catch.store = NA
    pred.catch = F
    k = 1
  }
  
  #########################################
  ######### looooooooooooop ###############
  #########################################
  
  
  # FOR EACH TIMESTEP
  while ( k < n.timesteps){
    #k=1
    
    # Dist 2 predator
    pred.dists = vector()
    for ( m in 2:(nrow(pos)+1)){
      pred.dists = c( pred.dists , ( dis ( i = 1 , j = m, pos = rbind(pred.pos, pos))))
    }
    
    # move back onto grid if they have gone off
    for ( i in 1:N){
      pos[i,] = periodic.boundary(pos[i,], grid.size=100) 
    }
    pred.pos = periodic.boundary(pred.pos,grid.size=100)
    
    # store interaction dynamics
    vel.dt     = matrix(NA,N,2)
    vel.att.dt = matrix(NA,N,2)
    vel.ali.dt = matrix(NA,N,2)
    vel.rep.dt = matrix(NA,N,2)
    vel.pred   = matrix( 0,N,2)
    
    
    
    # FOR EACH INDIVIDUAL
    for ( i in 1:N){
      #i=2
      
      # shortest distance to neighbour
      dists.vec = rep ( NA, N)
      for ( x in 1:N){
        if ( i != x){
          dists.vec[x] = dis( i, x, pos )
        }
      }
      
      # FIELD OF INPERCEPTION (looks like a pizza slice)
      dists.vec = pizza( dists.vec, head.pizza = heading[i], pos,  i, zones,field.of.inperception[i])
      
      # REPULSION
      if ( any(na.omit(dists.vec) < zones[i,3])){
        nei = which(dists.vec < zones[i,3])
        vel.rep.dt[i,] = repel( pos , i , nei)
        vel.dt[i,] = vel.rep.dt[i,] 
        repulsion = T
      } else {
        repulsion = F
        att = F
        ali = F
        
        
        # ATTRACTION
        if ( length(which( dists.vec > zones[i,2] & dists.vec < zones[i,1])) > 0  ){
          nei =  which( dists.vec > zones[i,2] & dists.vec < zones[i,1])
          a = attract ( pos+vel*future.predict.multip, i, nei)
          vel.att.dt[i,] =  a/sqrt(a[1]^2 + a[2]^2)
          att = T
        }
        
        # ALIGNMENT
        if ( length(which( dists.vec > zones[i,3] & dists.vec < zones[i,2])) > 0 ){ # [2] is zone of alignment
          nei =  which( dists.vec > zones[i,3] & dists.vec < zones[i,2])
          a = alignment(vel, i , nei)
          vel.ali.dt[i,] = a/sqrt(a[1]^2 + a[2]^2)
          ali = T
        }
        
        # combine att/ali or not
        if ( att ==T & ali == T){
          vel.dt[i,] = ( vel.att.dt[i,] + vel.ali.dt[i,])*0.5
        }
        if ( att ==T & ali == F){
          vel.dt[i,] = vel.att.dt[i,]
        }
        if( att == F & ali == T){
          vel.dt[i,] = vel.ali.dt[i,]
        }
        if (att == F & ali == F){
          vel.dt[i,] = vel[i,]
        }
      }
      
      
      ## Avoid predator
      # FIELD OF INPERCEPTION (looks like a pizza slice)
      see.pred = pizza( pred.dists[i], head.pizza = heading[i], rbind( pos[i,], pred.pos),  1, zones,field.of.inperception[i])
      if ( !is.na ( see.pred)){
        if ( see.pred < zones[i,1]){
          vel.pred[i,] = repel( rbind( pos[i,] , pred.pos),1,2)
        }
      } 
    }
    
    ## GROUP LEVEL
    
    # turn angle restricted by max.turn.rate
    pref.head = atan2(vel.dt[,1]+(vel.pred[,1]*avoidance.weight),vel.dt[,2]+(vel.pred[,1]*avoidance.weight))
    pref.turn = atan2( sin(pref.head-heading),
                       cos(pref.head-heading))
    turn = sign( pref.turn) *ifelse ( abs(pref.turn) < max.turn.rate , abs( pref.turn) , max.turn.rate)
    heading = heading+turn
    heading = ifelse (  heading < -pi, 2*pi+heading, 
                        ifelse (heading >  pi,-2*pi+(heading), heading))
    # VELOCITY/PERTUBATION Save computer power by combining
    new.vel = matrix( rnorm( N*2,c(speeds * sin( heading),
                                   speeds * cos( heading)),eta),N,2)
    heading =  atan2(new.vel[,1],new.vel[,2])
    
    
    # PREDATOR
    if( k>= pred.start ){
      pred.dists.post.pizza = pizza( pred.dists, head.pizza = pred.head, rbind( pred.pos, pos),  1, zones,pred.view,pred=T)
      if ( any ( !is.na ( pred.dists.post.pizza))){
        prey = which.min(pred.dists.post.pizza)[1]
        if ( pred.dists.post.pizza[prey] < pred.Ra){
          chase = attract (  rbind ( pred.pos , pos[prey,]+vel[prey,]*future.predict.multip.pred),1,2) 
          vel.chase = chase/sqrt(chase[1]^2 + chase[2]^2)
          # turn angle restricted by max.turn.rate
          pref.head = atan2(vel.chase[1],vel.chase[2])
          pref.turn = atan2( sin(pref.head-pred.head),
                             cos(pref.head-pred.head))
          turn = sign( pref.turn) *ifelse ( abs(pref.turn) < max.pred.turn.rate , abs(pref.turn) , max.pred.turn.rate)
          pred.head = pred.head+turn
          pred.head = ifelse (  pred.head < -pi, 2*pi+pred.head, 
                                ifelse (pred.head >  pi,-2*pi+(pred.head), pred.head))
          # VELOCITY/PERTUBATION Save computer power by combining
          pred.vel = matrix( rnorm( 2,c(pred.speed * sin( pred.head),
                                        pred.speed * cos( pred.head)),eta.predate),1,2)
        }
      } else {
        pred.vel = matrix( rnorm( 2,c(pred.speed * sin( pred.head),
                                      pred.speed * cos( pred.head)),eta),1,2)
      }
      pred.pos = pred.pos + pred.vel
      pred.head = atan2(pred.vel[1],pred.vel[2])
    } else {
      pred.pos= pred.pos
    }
    # DID PREDATOR CATCH A PREY? 
    vel = new.vel # new prey vel
    pos = pos+new.vel         # new prey pos
    pred.dists = vector()
    for ( m in 2:(nrow(pos)+1)){
      pred.dists = c( pred.dists , ( dis ( i = 1 , j = m, pos = rbind(pred.pos, pos))))
    }
    if ( any(pred.dists < zones[1,3])){ # zone of repulsion
      if( k>= pred.start ){
        catch.store =which.min(pred.dists)
        end.store = k
        k = n.timesteps
      }
    } else {
      k = k+1
      end.store = k
    }
    
    # store position
    pos.store [,,k] = pos
    pred.store[,k] = pred.pos
    
    #Plot??
    if(plot){
      po = rbind( pos,pred.pos)
      xy =   xylims(po[,1],po[,2])
      # png ( file.path ( filePath,  paste0( stringr::str_pad ( k,3, pad="0") ,".png") ))
      
      colour = c(rep("red",(N)),rep("blue",1))
      plot ( po[,1],
             po[,2], xlim = xy[[1]]+c(-20,20),
             ylim = xy[[2]]+c(-20,20), pch=3, ylab = "y (units)", xlab = "x (units)",col = colour)
      abline(h=seq(0,1000,40),lty=2,col="grey")
      abline(v=seq(0,1000,40),lty=2,col="grey")
      
    }
    
    
  }
  
  pos.store.list  = list ( pos.store = pos.store ,  
                           pred.store = pred.store,
                           catch.store = catch.store,
                           end.store = end.store)
  return( pos.store.list )
}

make.relative = function(dir.rel.ali){
  dir.rel.ali = ifelse(dir.rel.ali < -(pi/2), dir.rel.ali+(2*pi),dir.rel.ali)
  dir.rel.ali = ifelse(dir.rel.ali >  (pi/2), dir.rel.ali-(2*pi),dir.rel.ali)
  return(dir.rel.ali)
}

alignment = function ( vel , i, nei) {
  Av.vel = c( x = mean( vel[nei,1]),
              y = mean( vel[nei,2])) # Average of neighbour's velocity
  return(Av.vel)
}

ang.diff = function(x,y){
  
  
  zfunc = function(z){
    if ( z< 0){
      z2=  c( z,pi+abs (  diff( c(z,-pi) ) ))
    } else {
      z2=  c( z,-pi-abs( diff( c(z,pi))))
    }
    return(z2)
  }
  x2 = zfunc(x)
  y2 = zfunc(y)
  
  return ( min ( abs( diff( c( x2[1], y2[1]))),
                 abs( diff( c( x2[1], y2[2])))))
}

attract = function ( pos, i, nei,grid.size = 100) {
  
  # loop to return true neighbour position
  nei.pos = matrix(NA, length(nei), 2)
  c = 1
  for( j in nei) {
    nei.pos[c,] = dis( i, j, pos , grid.size = grid.size, return.boundary = T)[1:2]
    c = c+1
  }
  
  # Centroid of neighbours
  CoM = colMeans(nei.pos) # What is the individual's percieved centre?
  
  # velocity
  vel.att.dt = (CoM - pos[i,]) 
  return(vel.att.dt)
  
  
}

centroid = function ( pos, group.max.rad = Inf, d2c.t1 = NULL,grid.size = 100){
  
  arr = array( 0, c(N,N,2) )
  for ( i in 1:N){
    for ( j in 1:N){
      if(i!=j){
        arr[i,j,] = dis(i, j, pos, grid.size=grid.size, return.boundary = T)[3:4]
      }
    }
  }
  lr = sum( as.vector(arr[,,1]))
  tb = sum( as.vector(arr[,,2]))
  
  pos.cent = pos
  
  if ( lr != 0){
    movers = which( rowSums( arr[,,1]) !=0)
    for ( l in movers){
      stayer = which(arr[l,,1] == 1)[1]
      pos.cent[l,1] = dis( stayer, l, pos, grid.size=grid.size, return.boundary = T)[1]
    }
  }
  if ( tb != 0){
    movers = which( rowSums( arr[,,2]) !=0)
    stayers= c(1:N)[-movers]
    for ( l in movers){
      pos.cent[l,2] = dis( stayers[1], l, pos, grid.size=grid.size, return.boundary = T)[2]
    }
  }
  
  #plotR(pos.cent, lim = "min-max")
  
  d2c = rep(NA,N)
  for ( i in 1:N){
    d2c[i] = dis( i, pos=pos.cent, dist.2.centroid = T)
  }
  d2c.init = d2c
  
  while( max (na.omit(d2c)) > group.max.rad){
    pos.cent[which.max(d2c),] = NA
    d2c = rep(NA,N)
    for ( i in 1:N){
      d2c[i] = dis( i, pos=pos.cent, dist.2.centroid = T)
    }
  }
  
  # return
  if ( !is.null(d2c.t1)){
    if( identical( is.na(d2c) , is.na(d2c.t1)) ){
      cent = periodic.boundary(colMeans(pos.cent,na.rm = T), grid.size)
      return( list(cent, d2c, F,d2c.init))
    } else{
      return( list(rep(NA,2), d2c, T, d2c.init) )
    }
  } else {
    cent = periodic.boundary(colMeans(pos.cent,na.rm = T), grid.size)
    return( list(cent, d2c, F, d2c.init))
  }
}

dis = function(i = NULL, j = NULL, pos = NULL, speed.mat = NULL, grid.size = 100,
               return.boundary = F, dist.2.centroid = F, speed=F){
  
  # SPEED 
  if( speed){
    #speed.mat = d
    t1 = speed.mat[1:(nrow(speed.mat)-1),]
    t2 = speed.mat[2: nrow(speed.mat),]
    
    speed = apply( cbind(t1,t2),1, function (x){
      X = x[1:2]
      Y = x[3:4]
      c =  X[1] - Y[1]
      d =  X[2] - Y[2]
      px = c(abs( (X[1] - Y[1])),
             grid.size - abs(X[1] - Y[1]))
      py = c(abs((X[2] - Y[2])),
             grid.size - abs(X[2] - Y[2]))
      a = min( px)
      b = min( py)
      return(sqrt( a^2 + b^2 ))})
    
    speed = ifelse ( speed > grid.size/2 , grid.size-speed, speed ) 
    return(speed)
  } else {
    
    # DISTANCE
    X = pos[i,]
    if ( !dist.2.centroid){
      Y = pos[j,]
    } else {
      Y = colMeans(pos, na.rm = T)
    }
    if ( length(X) == 1 | length(Y) == 1) {
      stop( "both inputs should be vectors!!!!")
    }
    c =  X[1] - Y[1]
    d =  X[2] - Y[2]
    px = c(abs( (X[1] - Y[1])),
           grid.size - abs(X[1] - Y[1]))
    py = c(abs((X[2] - Y[2])),
           grid.size - abs(X[2] - Y[2]))
    a = abs(min( px))
    b = abs(min( py))
    
    
    # BOUNDARY CONDITIONS 
    if ( return.boundary ) {
      
      x.right = which( abs(px) == a ) == 2 && c > 0
      x.left  = which( abs(px) == a ) == 2 && c < 0
      y.top   = which( abs(py) == b ) == 2 && d > 0
      y.bot   = which( abs(py) == b ) == 2 && d < 0
      
      x.j = ifelse( x.right == T, grid.size + pos[j,1], pos[j,1])
      x.j = ifelse( x.left  == T, x.j - grid.size  , x.j  )
      y.j = ifelse( y.top   == T, grid.size + pos[j,2], pos[j,2])
      y.j = ifelse( y.bot   == T, y.j - grid.size  , y.j )
      
      return(c ( x.j, y.j,x.right,y.top, x.left, y.bot))
    }
    
    else {
      
      # return distance
      return( sqrt( a^2 + b^2 ))
      
    }
  }
}

periodic.boundary = function( pos.i, grid.size=100) {
  
  if ( pos.i[1] > grid.size){
    pos.i[1] = pos.i[1] - grid.size
  }
  
  if ( pos.i[2] > grid.size){
    pos.i[2] = pos.i[2] - grid.size
  }
  
  if ( pos.i[1] < 0){
    pos.i[1] = pos.i[1] + grid.size
  }
  
  if ( pos.i[2] < 0){
    pos.i[2] = pos.i[2] + grid.size
  }
  
  return(pos.i)
}

pert = function(length.output){
  sign = ifelse (rbinom (length.output,1,0.5) == 0, -1,1)
  apply( t(runif(length.output,0,1)),2,function(x){
    eta.dist$turn[sample( which( eta.dist$prob > x))[1]]
  })*sign
  
}

pizza = function ( dists.vecr, head.pizza, posr, i,zones,field.of.inperceptionr,pred =F ){
  
  # dists.vecr = pred.dists
  # head.pizza = pred.head
  # posr = rbind( pred.pos, pos)
  # i = 1
  # zonesr = pred.dists
  # field.of.inperceptionr = pred.view
  # pred = T
  
  if( !pred){
    test.pizza = which(dists.vecr < zones[i,1])
    
    # angle which corresponds to middle of pizza
    eps = -head.pizza
  } else{
    test.pizza = 2:nrow(posr)
    eps = head.pizza
  }
  
  # neighbour's relative position
  rel.head = atan2(posr[test.pizza,1] - posr[i,1], 
                   posr[test.pizza,2] - posr[i,2])
  
  # difference between the angles
  diff = abs(atan2(sin(rel.head-eps), cos(rel.head-eps)))
  
  # is this difference bigger than half of the field of inperception? (Remember eps is in  the middle of the pizza slice)
  if ( !pred){
    dists.vecr[test.pizza[diff < (field.of.inperceptionr/2)]] = NA
  } else {
    dists.vecr[diff > (field.of.inperceptionr)] = NA
  }
  dists.vecr
  return(dists.vecr)
}

repel = function (pos , i ,  nei,grid.size = 100) {
  
  # loop to return true neighbour position
  nei.pos = matrix(NA, length(nei), 2)
  c = 1
  for( j in nei) {
    nei.pos[c,] = dis( i, j, pos , grid.size = grid.size , return.boundary = T)[1:2]
    c = c+1
  }
  
  # Centroid of neighbours
  CoM = colMeans(nei.pos) # What is the individual's percieved centre?
  
  # velocity
  vel.rep.dt = -(CoM - pos[i,])
  return(vel.rep.dt)
  
  
}

set.pos = function ( N,grid.size = 100){
  # initialise position
  pos = matrix(c( rnorm( N,grid.size/2,5),
                  rnorm( N,grid.size/2,5)), N,2)
  return(pos)
}

xylims = function( x, y){
  # x = dat$x
  # y = dat$y
  
  r.x = range ( na.omit(x))
  r.y = range ( na.omit(y))
  
  limmax = max( diff( r.x),
                diff( r.y))/2
  
  x.start = diff(r.x)/2 + r.x[1]
  y.start = diff(r.y)/2 + r.y[1]
  
  xlim = x.start + ( c ( - limmax ,  limmax))
  ylim = y.start + ( c ( - limmax ,  limmax))
  
  return( list( xlim, ylim ))
  
}


######################################
grid.size = 100

ui <- fixedPage(
  titlePanel(" 'Selfish herders' finish last in mobile groups" ),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("calculate", "Run simulations"),
      sliderInput(inputId = "time", 
                  label = "Run simulations then wait for loading before pressing play below (bottom right of this slider)",
                  min = 1,
                  max = 100,
                  value = 1, 
                  step = 1,
                  animate = animationOptions(interval = 300)),
      sliderInput ( inputId = "Niter"                     , label = "N simulations (for statistics)"            , min = 2    , max = 100, value = 10  ),
      sliderInput ( inputId = "N"                         , label = "N individuals in flock"                   , min = 10   , max = 40  , value = 20  ),
      sliderInput ( inputId = "prop.d"                    , label = "Proportion of 'selfish herders'"          , min = 0.1  , max = 0.9 , value = 0.5 ),
      sliderInput ( inputId = "ali.zone"                  , label = "Zone of alignment size (for aligners)"    , min = 1    , max = 20  , value = 10  ),
      sliderInput ( inputId = "w"                         , label = "Weight of predator avoidance"             , min = 0.5  , max = 1.5 , value = 1   ),
      sliderInput ( inputId = "max.turn.rate"             , label = "Maximum turn rate (boids)"                , min = 0.1  , max = 1   , value = 0.5 ),
      sliderInput ( inputId = "max.pred.turn.rate"        , label = "Maximum turn rate (predator)"             , min = 0.05 , max = 0.2 , value = 0.1 ),
      sliderInput ( inputId = "v"                         , label = "Speed (boids)"                            , min = 1    , max = 2   , value = 1.5 ),
      sliderInput ( inputId = "pred.speed"                , label = "Speed (predator)"                         , min = 1.5  , max = 2.5 , value = 2.0 ),
      sliderInput ( inputId = "pred.Ra"                   , label = "Predator sight range (distance)"          , min = 25   , max = 125 , value = 75  ),
      sliderInput ( inputId = "pred.view"                 , label = "Predator sight range (angle)"             , min = 1.5  , max = 4.5 , value = 3   ),
      sliderInput ( inputId = "boid.range"                , label = "Boid sight range (angle)"                 , min = 3    , max = 6   , value = 4.5 ),
      sliderInput ( inputId = "eta"                       , label = "Randomness in movement (boids)"           , min = 0.05 , max = 0.2 , value = 0.1 ),
      sliderInput ( inputId = "eta.predate"               , label = "Randomness in movement (predator)"        , min = 0.005, max = 0.02, value = 0.01),
      sliderInput ( inputId = "future.predict.multip"     , label = "Attraction to future centroid (boids)"    , min = 1    , max = 5   , value = 1   ),
      sliderInput ( inputId = "future.predict.multip.pred", label = "Attraction to future centroid (predator)" , min = 1    , max = 5   , value = 1   ),
      sliderInput ( inputId = "starting.pos.sd"           , label = "Starting position standard deviation"     , min = 1    , max = 5   , value = 3   )
      
      
    ) ,
    mainPanel(h3("<- Click Run simulations"),
              h4("Do you notice red (centroid attractors) falling to the back of the flock?"),
              h4( "Are red (centroid attractors) more or less predated than yellow (aligners)? (see stats plot below)"),
              textOutput(outputId = "text"),
              plotOutput(outputId = "vid", width="500px",height="500px"),
              add_busy_spinner(spin = "fading-circle"),
              plotOutput(outputId = "stats",width = "400px", height= "500px")
              
    )
  )
)


server <- function(input, output){
  # rm(list = ls())
  # input=list (
  # time                      = 1,
  # N                         = 100,
  # prop.d                    = 0.89,
  # ali.zone                  = 5,
  # w                         = 1,
  # max.turn.rate             = 0.5,
  # max.pred.turn.rate        = 0.1,
  # v                         = 1.5,
  # pred.speed                = 2.0,
  # pred.Ra                   = 50,
  # pred.view                 = 3,
  # boid.range                = 4.5,
  # eta                       = 0.1,
  # eta.predate               = 0.01,
  # future.predict.multip     = 1,
  # future.predict.multip.pred= 1,
  # Niter                     = 10,
  # starting.pos.sd           = 5)
  
  
  sam = eventReactive( input$calculate , {
    #
    #
    ######
    #
    #
    {
      field.of.inperception = 2*pi - input$boid.range
      N.att = floor(input$N*input$prop.d)
      N.ali = ceiling(input$N*(1-input$prop.d))
      N = N.att + N.ali
      self.zones = c( 25, 1, 1) # THIS ASSUMES BOID ATTRACT RANGE OF 25. NOT EDITABLE (this version) IN SHINY
      coop.zones = self.zones
      coop.zones[2] = 25-input$ali.zone
      zones = t ( matrix ( c( rep ( self.zones , N.att),rep ( coop.zones , N.ali)), nrow = 3))
      #grid.size = 100
      pos.store.lists = list()
      for ( l in 1:input$Niter){
        pos.store.list =
          flock.pred (
            N = N,
            zones =zones,
            speeds = rep ( input$v,N),
            avoidance.weight = rep(input$w, N),
            field.of.inperception = field.of.inperception,
            max.turn.rate = input$max.turn.rate,
            n.timesteps = 100,
            pred.view = input$pred.view,
            eta.predate = input$eta.predate,
            max.pred.turn.rate = input$max.pred.turn.rate,
            pred.speed =  input$pred.speed,
            grid.size = 100,
            starting.pos.sd = input$starting.pos.sd,
            future.predict.multip =  input$future.predict.multip,
            future.predict.multip.pred = input$future.predict.multip.pred,
            eta = input$eta,
            plot = F,
            pred.start = 10,
            pred.Ra = input$pred.Ra
          )
        pos.store.lists[[l]] = pos.store.list
      }
      pos.store.lists = pos.store.lists
      psl = pos.store.lists
      lens = unlist ( lapply ( psl, function(x)x$end.store))
      whi = max ( lens[ lens<100] )
      if(whi == -Inf ){
        whi =1
      } else { 
        whi = which ( lens == whi )[1]
      }
      dat = psl  [[whi]]
      dat2 = dat$pos.store[,,]
      pred = dat$pred.store[,]
      dims = dim(dat2)
      dims[1] = dims[1] +1
      dat3 = array ( NA , dims)
      for ( i in 1:dat$end.store){
        dat3[,,i] = rbind ( dat2[,,i],pred[,i])
      }
      for ( i in (dat$end.store+1):100){
        dat3[,,i] = dat3[,,dat$end.store]
      }
      caught = unlist ( lapply ( psl, function(x)x$catch.store))
      tb = table ( ifelse ( caught <= N.att , 1,2))
      foo=c(0,0)
      foo [ as.numeric ( names ( tb))] = tb
      
      chisQ = chisq.test(foo, p =   c (  input$prop.d, 1-input$prop.d))
      mult. = (1-input$prop.d) / input$prop.d
      tb2 = foo
      tb2[1] = tb2[1] * mult.
      statistic = ifelse ( diff (tb2) < 0 , chisQ$statistic, -chisQ$statistic)
      
      
      
      dat3 = list ( dat = dat3 , statistic= statistic)
      dat3
    }
  })
  
  ##### REACTIVE COLOUR
  colour = reactive ({ 
    cols = c(rep("red"   ,floor(input$N*input$prop.d)),
             rep("yellow3",ceiling(input$N*(1-input$prop.d))),
             rep("blue",1))
  })
  
  ##### FOR OUT OF APP WORK
  # sam = function() print(dat3)
  # colour = function() print ( cols)
  
  ########### STATS PLOT ###################
  output$stats = renderPlot ( {
    sig.effect = c(-1,1)* qchisq(0.975,1,0,T)
    ran = c(-1,1) * max (abs( c ( sig.effect ,sam()$statistic)))#
    ran = ran + c(-2,2)
    NI =     eventReactive( input$calculate, {
      input$Niter})
    plot ( sam()$statistic , ylim = ran , main =  expression ( paste ( chi, "-squared test")) ,xaxt = "n", xlab = "", ylab = "Aligners more predated ---- Null hypotheis ---- Attractors more predated")
    if (NI() < 100 ){
      legend ( "bottomright" , legend = "consider running 100 simulations to be 'sure'" )
    } else {
      legend ( "bottomright" , legend = "see paper for 1000 simulations \n over a range of conditions" )
    }
    abline ( h = sig.effect, lty = 2)
    abline ( h = 0)
  })
  
  ########### VIDEO PLOT ###################
  output$vid = renderPlot({
    
    
    plot ( sam()$dat[,1,input$time],
           sam()$dat[,2,input$time], 
           #xlim = xy[[1]]+c(-20,20),
           #ylim =  xy[[2]]+c(-20,20),
           xlim = c(0,100),#
           ylim = c(0,100),
           pch=3, ylab = "y (units)", xlab = "x (units)",col = colour())
    abline(h=seq(0,100,20),lty=2,col="grey")
    abline(v=seq(0,100,20),lty=2,col="grey")
    legend ( "topright", legend = c("centroid attractors" , "aligners", "predator"), col = c("red" , "yellow3", "blue"), pch = 3)
  })
  
}

shinyApp(ui = ui, server = server)

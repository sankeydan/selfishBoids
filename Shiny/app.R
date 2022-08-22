
{
  rm(list=ls())
library(shiny)
library(ggthemes)
library(ggplot2)
library(dplyr)
#library( selfishBoids)
#source( file.path (PROJHOME,"source" , "quickLibrary.r"))
library(shinybusy)
#radioButtons( inputId =   "start.from.pred.notice"  , label = "Start when predator is spotted?"          , choices = c(FALSE,TRUE))

######################################
grid.size = 100
pred.start = 200
time.total = 300
}

# do not run
ui <- fixedPage(
  titlePanel(" 'Selfish-herders' finish last in predator-responsive groups" ),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("calculate", "Run simulations"),
      sliderInput(inputId = "time", 
                  label = "....wait for loading then PRESS PLAY (underneath the number 300 below this bar)",
                  min = 1,
                  max = time.total,
                  value = 1, 
                  step = 1,
                  animate = animationOptions(interval = 40)),
      sliderInput ( inputId = "Niter"                     , label = "N simulations (for statistics)"           , min = 2    , max = 100 , value = 5  ),
      sliderInput ( inputId = "N"                         , label = "N individuals in flock"                   , min = 10   , max = 40  , value = 20  ),
      sliderInput ( inputId = "prop.d"                    , label = "Proportion of 'selfish herders'"          , min = 0.1  , max = 0.9 , value = 0.5 ),
      sliderInput ( inputId = "ali.zone"                  , label = "Zone of alignment size (for aligners)"    , min = 0.1  , max = 0.9 , value = 0.5 ),
      sliderInput ( inputId = "sense.range"               , label = "Sensory zone (maximum zone for attraction)",min = 20   , max = 45  , value = 25  ),
      sliderInput ( inputId = "w"                         , label = "Weight of predator avoidance"             , min = 0    , max = 1   , value = 0.5 ),
      sliderInput ( inputId = "max.turn.rate"             , label = "Maximum turn rate (boids)"                , min = 0.1  , max = 1   , value = 0.5 ),
      sliderInput ( inputId = "max.pred.turn.rate"        , labe= "Maximum turn rate (predator)"               , min = 0.05 , max = 0.2 , value = 0.1 ),
      sliderInput ( inputId = "pred.speed"                , label = "Speed (predator)"                         , min = 1    , max = 3   , value = 1.5 ,step=0.1),
      sliderInput ( inputId = "eta"                       , label = "Randomness in movement (boids)"           , min = 0.05 , max = 0.2 , value = 0.1 ),
      sliderInput ( inputId = "eta.predate"               , label = "Randomness in movement (predator)"        , min = 0.005, max = 0.02, value = 0.01),
      sliderInput ( inputId = "future.predict.multip"     , label = "Attraction to future centroid (boids)"    , min = 1    , max = 5   , value = 1   ),
      sliderInput ( inputId = "future.predict.multip.pred", label = "Attraction to future centroid (predator)" , min = 1    , max = 5   , value = 1   ),
      radioButtons( inputId =   "bordersTF"               , label = "Solid borders?"                           , choices = c(FALSE,TRUE)),
      sliderInput ( inputId = "border.dist.range"         , label = "If solid borders TRUE, start responding to borders at (distance)", min = 1    , max = 50   , value = 10 ),
      radioButtons( inputId = "continuous.method"         , label = "Continuous method?"                       , choices = c(FALSE,TRUE)),
      sliderInput ( inputId = "q.att.if.cont"             , label = "q val for attractors (if continuous method)", min = 0.5, max = 1, value =  0.8),
      sliderInput ( inputId = "q.ali.if.cont"             , label = "q val for aligners (if continuous method)"  , min = 0  , max = 0.5, value =  0.2),
     )
    ,
    mainPanel(h3("<- Click Run simulations"),
              plotOutput(outputId = "vid", width="500px",height="500px"),
              h5("- The 'selfish herd' hypothesis (W. D. Hamilton, 1971) suggests that animals who attract to the positions of their 
                 neighbours will be safer from predators."),
              h5("- The implication is that this could result in the evolution of socially cohesive behaviour, and possibly of stable group living
                 from previously solitary species."),
              h5("- However, observations from nature suggest that when a predator strikes, prey-groups often align in the same direction as one another, and do not 'jostle for position' in the centre of the group."), 
              h5("- One critical assumption of Hamilton's model was that prey do not respond to the position of their predator, only that of their neighbours. In this model this assumption is relaxed, and prey are able to respond to both predators and their neighbours"),
              h5("- This model poses a potential mechanistic account for why animals might not exhibit 'selfish herd' behaviour in moving flocks. 
                 But rather than tell you how, can you validate the model findings for yourself?"),
              h5("-----------------------------------------------------------"),
              h5("- Calling interested researchers: Sliders have been provided to modify anything within this model to test your own hypotheses. However, the first four may be the most useful to those of you previously unfamiliar with agent-based models"),
              h4("1. Are red (centroid attractors) falling to the back of the flock? (above)") ,
              h4( "2. Are red (centroid attractors) more or less predated than yellow (aligners)? (see stats plot below)"),
              add_busy_spinner(spin = "fading-circle"),
              plotOutput(outputId = "stats",width = "400px", height= "500px")
    )   
  )
)



server <- function(input, output){
  
  # run here 
  
  
  # input=list (
  # time                      = 1,
  # N                         = 20,
  # prop.d                    = 0.5,
  # ali.zone                  = 0.5,
  # sense.range               = 25,
  # w                         = 0.5,
  # max.turn.rate             = 0.5,
  # max.pred.turn.rate        = 0.1,
  # pred.speed                = 2.0,
  # eta                       = 0.1,
  # eta.predate               = 0.01,
  # future.predict.multip     = 1,
  # future.predict.multip.pred= 1,
  # Niter                     = 50,
  # bordersTF                 = F,
  # border.dist.range         = 10,
  # continuous.method         = T,
  # q.att.if.cont             = 0.9,
  # q.ali.if.cont             = 0.1
  # )
  
  
  sam = eventReactive( input$calculate , {
   #             #
   #              #
   #     ########### run
   #              #
   #             #
    {

      N.att = floor(input$N*input$prop.d)
      N.ali = ceiling(input$N*(1-input$prop.d))
      N = N.att + N.ali
      att.zones = c( input$sense.range, 1, 1)
      ali.zones = att.zones
      if ( input$continuous.method){
        ali.zones[2] = NA
      } else { 
      ali.zones[2] = ( (input$sense.range - 1) * input$ali.zone ) +1 
      }
      zones = t ( matrix ( c( rep ( att.zones , N.att),rep ( ali.zones , N.ali)), nrow = 3))
      pos.store.lists = list()
    }
    {
      for ( l in 1:input$Niter){
        pos.store.list =
          flock.pred ( 
            N = N,
            zones =zones,
            speeds = rep ( 1,N),
            avoidance.weight = rep(input$w, N),
            max.turn.rate = input$max.turn.rate,
            n.timesteps = time.total,
            eta.predate = input$eta.predate,
            max.pred.turn.rate = input$max.pred.turn.rate,
            pred.speed =  input$pred.speed,
            grid.size = grid.size,
            future.predict.multip =  input$future.predict.multip,
            future.predict.multip.pred = input$future.predict.multip.pred,
            eta = input$eta,
            plot = F,
            pred.start = pred.start,
            bordersTF = input$bordersTF,
            border.dist.range= input$border.dist.range,
            continuous.method = input$continuous.method,
            q.att.if.cont = input$q.att.if.cont,
            q.ali.if.cont = input$q.ali.if.cont,
          )
        pos.store.lists[[l]] = pos.store.list
      }
      psl = pos.store.lists
      lens = unlist ( lapply ( psl, function(x)x$end.store))
      whi = max ( lens [ lens<time.total] )
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
      for ( i in (dat$end.store+1):time.total){
        dat3[,,i] = dat3[,,dat$end.store]
      }
      caught = unlist ( lapply ( psl, function(x)x$catch.store))
      tb = table ( ifelse ( caught <= N.att , 1,2))
      foo=c(0,0)
      foo [ as.numeric ( names ( tb))] = tb
      
      chisQ = suppressWarnings( chisq.test(foo, p =   c (  input$prop.d, 1-input$prop.d)))
      mult. = (1-input$prop.d) / input$prop.d
      tb2 = foo
      tb2[1] = tb2[1] * mult.
      statistic = ifelse ( diff (tb2) < 0 , chisQ$statistic, -chisQ$statistic)
      
      dat3[N+1,,1:pred.start] = NA
      
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
  # sam = function() dat3
  # colour = function()  cols
  
  ########### STATS PLOT ###################
  output$stats = renderPlot ( {
    sig.effect = c(-1,1)* qchisq(0.975,1,0,T)
    ran = c(-1,1) * max (abs( c ( sig.effect ,sam()$statistic)))#
    ran = ran + c(-2,2)
    NI =     eventReactive( input$calculate, {
      input$Niter})
    plot ( sam()$statistic , ylim = ran , main =  expression ( paste ("Two-tailed ", chi, "-squared test")) ,xaxt = "n", xlab = "", ylab = "Aligners more predated ---- Null hypotheis ---- Attractors more predated")
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
           xlim = c(0,grid.size),#
           ylim = c(0,grid.size),
           pch=3, ylab = "y (units)", xlab = "x (units)",col = colour(), main = "One example from your N simulations")
    abline(h=seq(0,grid.size,20),lty=2,col="grey")
    abline(v=seq(0,grid.size,20),lty=2,col="grey")
    legend ( "topright", legend = c("centroid attractors" , "aligners", "predator"), col = c("red" , "yellow3", "blue"), pch = 3)
    
  })
  
}

shinyApp(ui = ui, server = server)
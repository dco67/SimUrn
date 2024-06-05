
library(shiny)
library(DT)
#library(kableExtra)

## Negative Binomial Distribution: Density
# x = Number of success/failures
# n = Number of success
# p = Probability of success
# Type = 1: Number of failures prior to n success
# Type = 2: Number of trials to reach n success
NegBinD <- function(x, n, p, type = 1){
  if(type == 1){
    pNB <- choose(x+n-1, n-1) * (1 - p)^x * p^n
  } else {
    pNB <- choose(x-1, n-1) * p^n * (1 - p)^(x - n)
  }
  return(pNB)
}

## Geometric Distribution: Density
# x = Number of success/failures
# p = Probability of success
# Type = 1: Number of failures prior to first success
# Type = 2: Number of trials to reach first success
GeomD <- function(x, p, type = 1){
  if(type == 1){
    pGEO <- (1 - p)^x * p
  } else {
    pGEO <- (1 - p)^(x - 1) * p 
  }
  return(pGEO)
}

# https://r-coder.com/poisson-distribution-r/ 
# https://www.r-bloggers.com/2017/10/a-minor-update-to-simstudy-provides-an-excuse-to-talk-a-bit-about-the-negative-binomial-and-poisson-distributions/

ui <- fluidPage(

    titlePanel("Distributions Binomiale Négative & Géométrique"),

    sidebarLayout(
        sidebarPanel(
          selectInput("type",
                      "Distribution",
                      choices = c("Binomiale Négative I"="bneg1",
                                  "Binomiale Négative II"="bneg2",
                                  "Géométrique I"="geo1", 
                                  "Géométrique II"="geo2"),
                      selected = "bneg1",
                      width = "200px"),
          sliderInput("p",
                      "Probabilité d'un succès",
                      min = 0.01,
                      max = 1,
                      step = 0.01,
                      value = 0.5),
          conditionalPanel(
            condition = "input.type == 'bneg1' || input.type == 'bneg2'",
            numericInput("n",
                         "Nombre de succès",
                         min = 1,
                         max = 200,
                         value = 5)
            ),

#          fluidPage(
#            column(6, offset = 3, 
#                   actionButton("go",
#                                "Exécuter", 
#                                icon("paper-plane"), 
#                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
#            ),
          p(),
          p(),
          wellPanel(style = "background: lightyellow",
                    helpText(
                      withMathJax(
                      h3("Introduction"),
                      "Cette application permet de visualiser les distributions binomiales négatives (I et II),  ",
                      "et les distributions géométriques (I et II), et de calculer ",
                      "les aires (probabilités) sous des zones définies de ces distributions.", p(),
                      "Les paramètres d'une distribution binomiale négative sont la probabilité d'un succès (\\(p\\)) et ",
                      "soit le nombre d'échecs survenant avant d'observer \\(n\\) succès (BinNeg I), soit le nombre d'essais requis pour atteindre \\(n\\) succès (BinNeg II). ", p(),
                      "Les distributions géométriques ne reposent que sur un unique paramètre, la probabilité d'un ",
                      "succès (\\(p\\)). La variable aléatoire est le nombre d'échecs survenant avant l'apparition du premier succès (Géom I), ", 
                      "ou le nombre d'essais requis pour obtenir le premier succès (Géom II).", p(),
                      "La distribution à étudier est sélectionnée à partir du menu déroulant, dans le tableau de bord. ",
                      "Les paramètres de la distribution choisie sont ensuite établis à l'aide d'un curseur et d'un ",
                      "champs numérique.", p(),
                      "La distribution est affichée sous forme d'un diagramme en barres, à droite du tableau de bord.  ",
                      "Les caractéristiques de cette distribution, de même que la liste des valeurs possibles et des ",
                      "probabilités qui y sont rattachées, sont affichées dans la portion inférieur de la fenêtre.  ", p(),
                      "Le curseur situé sous le graphique permet de sélectionner une zone particulière et de calculer ",
                      "la probabilité correspondante. ")
                      
                    )),
          
wellPanel(style = "background: lightblue",
          fluidRow(
            column(4,
                   a(h4("Par Daniel Coulombe, Ph.D.")),
                   p("2022")
            ),
            column(4,
                   tags$a(
                     href="https://isteah.org", 
                     tags$img(src="ISTEAH_LOGO.png", 
                              title="ISTEAH", 
                              width="160",
                              height="140")
                   )
            )
          )
)

        ),

        mainPanel(

          plotOutput("distPlot"),

        fluidPage(
            column(9,
                   uiOutput("probbtw")),
            column(3, 
                   textOutput("calc3"),
                   tags$head(tags$style("#calc3{color: red;
                                 font-size: 30px;
                                 font-style: bold;
                                 }"
                                        )
                             )
                   )
            ),
        fluidPage(
          column(7, 
                 uiOutput("details")),
          column(5,
                 dataTableOutput("tblshow"))
        )
        )
    )
)

server <- function(input, output) {
  
  x <-  reactive({
    if(input$type == "bneg1"){
      0:(qnbinom(0.9999, input$n, input$p)+10)
      
    } else if(input$type == "bneg2"){
      input$n:(qnbinom(0.9999, input$n, input$p)+10)
      
    } else if(input$type == "geo1"){
      0:(qgeom(0.9999, input$p)+10)
      
    } else if(input$type == "geo2"){
      1:(qgeom(0.9999, input$p)+10)
      
    }
  })
  
  
  DistDens <- reactive({
    if(input$type == "bneg1"){NegBinD(0:max(x()), input$n, input$p, type=1)}
      else if(input$type == "bneg2"){NegBinD(input$n:max(x()), input$n, input$p, type = 2)}
      else if(input$type == "geo1"){GeomD((0:max(x())), input$p, type = 1)} 
      else if(input$type == "geo2"){GeomD(1:max(x()), input$p, type = 2)} 
    })
  
  rg <- reactive(DistDens()[length(DistDens())])

    output$distPlot <- renderPlot({
      
      txt <- paste0("Distribution ", if(input$type == "bneg1"){"Binomiale Négative I"} 
                    else if(input$type == "bneg2"){"Binomiale Négative II"} 
                    else if(input$type == "geo1"){"Géométrique I"} 
                    else {"Géométrique II"})
      datcol <- x()[(x() >= input$ll[1]) & (x() <= input$ll[2])]
      cols <- ifelse(x() %in% datcol, "red","darkgray")
      espm <- if(input$type == "bneg1"){input$n * (1 - input$p) / input$p}
         else if(input$type == "bneg2"){input$n / input$p}
         else if(input$type == "geo1"){(1-input$p) / input$p} 
         else {1 / input$p}
      stdev <- if(input$type == "bneg1" | input$type == "bneg2"){sqrt(input$n * (1 - input$p) / input$p^2)} 
         else {sqrt((1 - input$p) / input$p^2)}

      txt1 <- paste0( 
                     if(input$type == "bneg1"){paste0("Nombre d'échecs antérieurs au ", input$n, "ième succès")} 
                        else if(input$type == "bneg2"){paste0("Nombre d'essais précédant le ", input$n, "ième succès (inclus)")}
                           else if(input$type == "geo1"){"Nombre d'échecs antérieurs au premier succès"} 
                              else {"Nombre d'essais antérieurs au premier succès (inclus)"})
      
     barplot(DistDens(),
           main=txt,
           names.arg = x(),
           ylab = "Probabilité",
           xlab = txt1,
           col = cols,
           lwd = 3,
           cex.main=2,
           cex.lab=1.5)
      
      legend("topright", 
             legend = c(paste0("E(x)   = ", round(espm, 3)),
                        paste0("\u03C3 = ", round(stdev, 3))),
             col = c("black", "black"),
             cex=1.2,
             title = "Paramètres",
             text.font = 2,
             bg = "lightblue",
             box.lty = 1,
             box.lwd = 2)
      
    })
    
    output$tblshow <- renderDataTable({

      tbl <- cbind(x(), round(DistDens(), 5), round(cumsum(DistDens()), 5))
      tbl <- subset(tbl, tbl[, 2] != 0)
      
      txt <- if(input$type == "bneg1"){"#Échecs"} 
              else if(input$type == "bneg2"){"#Essais"} 
              else if(input$type == "geo1"){"#Échecs"}
              else if(input$type == "geo2"){"#Essais"}

      datatable(tbl, 
                colnames=c(txt, "Prob", "Pcum"),
                class = "cell-border stripe",
                width = "25%",
                options = list(
                  dom = "tflipr",
                  scrollY = "500px",
                  pageLength = 20, 
                  lengthMenu = c(10, 20, 50),
                  info = TRUE,
                  paging = TRUE,
                  searching = FALSE,
                  autoWidth = TRUE,
                  columnDefs = list(list(width = '1%', targets = "_all"))
                )) %>% 
        formatStyle(columns = 2, target = "cell",  backgroundColor = "#F7080880")
      
    })
    
    output$probbtw <- renderUI({
      
      sliderInput("ll", 
                  label = h3("P( x \u2264 X \u2264 y )"),
                  value = c(min(x()[1]), max(x())),
                  min = x()[1],
                  max = max(x()),
                  step = 1,
                  width = "100%")
    })
    
    output$calc3 <- renderText({
      if(is.null(input$ll)){return()}
      
      pr <- if(input$type == "bneg1"){sum(DistDens()[(input$ll[1]+1):(input$ll[2]+1)])
              } else if(input$type == "bneg2"){sum(DistDens()[(input$ll[1]-input$n+1):(input$ll[2]-input$n+1)])
                } else if(input$type == "geo1"){sum(DistDens()[input$ll[1]:input$ll[2]])
                  } else {sum(DistDens()[(input$ll[1]):(input$ll[2])])} 
      
#      pr <- if(input$type == "bneg1" | input$type == "bneg2"){
#        pnbinom(input$ll[2], size=input$n, prob=input$p, lower.tail=TRUE) - pnbinom(input$ll[1], size=input$n, prob=input$p, lower.tail=TRUE)} 
#      else {pgeom(input$ll[2], input$p, lower.tail=TRUE) - pgeom(input$ll[1], input$p, lower.tail=TRUE)} 
      paste(" = ", round(pr, 4))

    })
    
    output$details <- renderUI({
      # https://en.wikipedia.org/wiki/Negative_binomial_distribution
      # https://en.wikipedia.org/wiki/Geometric_distribution
      p <- input$p
        if (input$type == "bneg1") {
        n <- input$n
          withMathJax(
            helpText(
              h4(strong("Fonction de probabilité de masse:"), "$$P(X = x) = \\binom{x+r-1}{r-1} (1-p)^x p^r $$"),
              "où \\( x = 0, 1, 2, \\dots \\qquad \\) et \\( \\qquad 0 < p \\leq 1 \\)"),
            br(),
            helpText("\\(\\mu = E(X) = \\dfrac{r(1-p)}{p} = \\)", round((n * (1 - p) / p), 3)),
            helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{r(1-p)}{p^2}} = \\)", round(sqrt((n * (1 - p) / (p^2))), 3)),
            helpText("\\(\\sigma^2 = Var(X) = \\dfrac{r(1-p)}{p^2} = \\)", round((n * (1 - p) / (p^2)), 3)),
            helpText("\\(Sk = Symétrie = \\dfrac{1+p}{\\sqrt{pr}} = \\)", round((1+p) / sqrt(p * n), 3)),
            helpText("\\(Ku = Voussure = \\dfrac{6}{r}+\\dfrac{(1-p)^2}{pr} = \\)", round((6/n)+(1-p)^2 / (p * n), 3))
 
          )

        } else if (input$type == "bneg2") {
          n <- input$n
          withMathJax(
            helpText(
              h4(strong("Fonction de probabilité de masse:"), "$$P(X = x) = \\binom{x-1}{r-1}p^r (1-p)^{x-r} $$"),
            helpText("où \\( x = r, r+1, \\dots \\qquad r = 1, 2, \\dots \\qquad 0 < p \\leq 1\\)"),
            br(),
            helpText("\\(\\mu = E(X) = \\dfrac{r}{p} = \\)", round((n / p), 3)),
            helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{r(1-p)}{p^2}} = \\)", round(sqrt((n * (1 - p) / (p^2))), 3)),
            helpText("\\(\\sigma^2 = Var(X) = \\dfrac{r(1-p)}{p^2} = \\)", round((n * (1 - p) / (p^2)), 3)),

            helpText("\\(Sk = Symétrie = \\dfrac{1+p}{\\sqrt{pr}} = \\)", round((1+p) / sqrt(p * n), 3)),
            helpText("\\(Ku = Voussure = \\dfrac{6}{r}+\\dfrac{(1-p)^2}{pr} = \\)", round((6/n)+(1-p)^2 / (p * n), 3))
)
          )

        } else if (input$type == "geo1") {
          withMathJax(
            helpText(
              h4(strong("Fonction de probabilité de masse:"), "$$ p(x) = P(X = x) = (1 - p)^{x-1} p $$")),
            helpText("où \\( x = 1, 2, \\dots \\) and \\( 0 < p \\leq 1 \\)"),
            br(),
            helpText("\\(\\mu = E(X) = \\dfrac{1-p}{p} = \\)", round((1-input$p) / input$p, 3)),
            helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{1-p}{p^2}} = \\)", round(sqrt((1 - input$p) / (input$p^2)), 3)),
            helpText("\\(\\sigma^2 = Var(X) = \\dfrac{1-p}{p^2} = \\)", round((1 - input$p) / (input$p^2), 3)),

            helpText("\\(Sk = Symétrie = \\dfrac{2-p}{\\sqrt{1-p}} = \\)", round((2-p) / sqrt(1-p), 3)),
            helpText("\\(Ku = Voussure = 6+\\dfrac{p^2}{1-p} = \\)", round((6+p^2 / (1-p)), 3)))

        } else if (input$type == "geo2") {
          withMathJax(
            helpText(
              h4(strong("Fonction de probabilité de masse:"), "$$ p(x) = P(X = x) = (1 - p)^x p $$")),
            helpText("où \\( x = 0, 1, 2, \\dots \\) and \\( 0 < p \\leq 1 \\)"),
            br(),
            helpText("\\(\\mu = E(X) = \\dfrac{1}{p} = \\)", round(1 / input$p, 3)),
            helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{1-p}{p^2}} = \\)", round(sqrt((1 - input$p) / (input$p^2)), 3)),
            helpText("\\(\\sigma^2 = Var(X) = \\dfrac{1-p}{p^2} = \\)", round((1 - input$p) / (input$p^2), 3)),

            helpText("\\(Sk = Symétrie = \\dfrac{2-p}{\\sqrt{1-p}} = \\)", round((2-p) / sqrt(1-p), 3)),
            helpText("\\(Ku = Voussure = 6+\\dfrac{p^2}{1-p} = \\)", round((6+p^2 / (1-p)), 3)))
          
        }
      
        })
}

shinyApp(ui = ui, server = server)

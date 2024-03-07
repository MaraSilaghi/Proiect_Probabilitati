#2 b)
library(shiny)
library(plotly)

#se afiseaza pagina cu titlul si variabilele x si y, care pot fi modificate
ui <- fluidPage(
  titlePanel("Vizualizarea integralei duble"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("range1", "Interval X:", min = -100, max = 100, value = c(-10, 10)),
      sliderInput("range2", "Interval Y:", min = -100, max = 100, value = c(-10, 10))
    ),
    mainPanel(
      plotlyOutput("plot3d")    #afisam graficul
    )
  )
)

server <- function(input, output) {
  #definim funcctia pe care vrem sa o reprezentam grafic
  f <- function(x, y) {
    x + y  
  }
  #cream graficul
  output$plot3d <- renderPlotly({
    #cream axele x si y care iau valori intre input-urile date in ui, iar length.out reprezinta numarul de puncte din secventa
    x <- seq(input$range1[1], input$range1[2], length.out = 50)  
    y <- seq(input$range2[1], input$range2[2], length.out = 50)
    
    #outer aplica functia f pe toate combinatiile de x si y si pastreaza valorile intr-o matrice
    z <- outer(x, y, f)  
    z_min <- min(z)  #pastram minimul din z pentru a desena si umbra planului functiei
    
    # cream suprafata functiei cu axele x y si z
    p <- plot_ly(x = ~x, y = ~y, z = ~z, type = "surface")
    
    
    # cream umbra suprafetei functiei
    z_floor <- matrix(z_min, nrow = 50, ncol = 50)  #toate valorile lui z iau valoarea z_min, pentru ca suprafata sa fie plana
    p <- add_trace(p, x = ~x, y = ~y, z = ~z_floor, type = "surface",   #add_trace adauga noua suprafata
                   showscale = FALSE, 
                   opacity = 0.5) 
    
    p
  })
}

shinyApp(ui = ui, server = server)



#2 c)
library(pracma) #folosim libraria pracma

functie <- function(x, y) #aici imi definesc f(x,y)
{
  x*y+x
}

verif_dens_prob <- function(x1, x2, y1, y2) #x1 si x2 capete pentru integrala dupa x, y1 si y2 capete pt a doua integrala
{
  semafor <- 1 #presupun ca este densitate de probabilitate
  rezultat <- integral2(functie, x1, x2, y1, y2) #aici calculez rezultatul integralei
  if(rezultat$Q!=1) #daca campul pentru rezultat nu este egal cu 1 pe interval
  {
    semafor <- 0 #functia nu este densitate de probabilitate
  }
  #verific daca functia este pozitiva pe intervale 
  #iau 100 de valori intre x1 si x2
  #iau 100 de valori intre y1 si y2
  #verific daca pentru aceste combinatii functia este positiva
  pozitiv <- all(outer(seq(x1, x2, length.out = 100), seq(y1, y2, length.out = 100), Vectorize(functie)) >= 0)
  if(pozitiv==FALSE) 
  {
    semafor <- 0 #daca nu este pozitiva, inseamna ca nu e densitate de probabilitate
  }
  if(semafor==0) #daca una sau mai multe conditii nu sunt indeplinite, nu e densitate
  {
    print("Functia nu este densitate de probabilitate")
  }
  else #e densitate
  {
    print("Functia este densitate de probabilitate")
  }
}

verif_dens_prob(0,1,0,1)
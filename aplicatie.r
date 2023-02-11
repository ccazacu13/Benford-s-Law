#Proiect realizat de Cazacu Cristian-Gabriel, Pasare Roxana-Francisca, Paun Andreea-Alexandra
#Legea lui Benford

#import-urile librariilor folosite pentru proiect
library(ggplot2)
library(shiny)
library(knitr)
library(shinythemes)

rmdfiles <- c("raport.Rmd")
sapply(rmdfiles, knit, quiet = T)


#Partea de UI
ui <- fluidPage(theme = shinytheme("united"),
  navbarPage("Legea lui Benford",
             tabPanel("Introducere",
             withMathJax(includeMarkdown("raport.Rmd"))),
             
             tabPanel("Raport Nume",
                      h4("Date incluse: 2020"),
                      h4("Date preluate din USA"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "numeComune",
                                      label = "Numarul de Nume care au fost prelucrate",
                                      min = 1,
                                      max = 39417,
                                      value = 20000
                          )
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Graficul legii lui Benford", plotOutput("dateName")),
                            tabPanel("Datele tabelului", DT::dataTableOutput("NumeTabel"))
                          )
                        )
                      )
             ),
             tabPanel("Raport Populatie a oraselor lumii",
                      h4("Date incluse: 2022"),
                      h4("Date preluate din lume"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "roCensus",
                                      label = "Numarul de orase care au fost prelucrate",
                                      min = 1,
                                      max = 42906,
                                      value = 5000
                          )
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Graficul legii lui Benford", plotOutput("dateCensus")),
                            tabPanel("Datele tabelului", DT::dataTableOutput("censusTabel"))
                          )
                        )
                      )
             ),
             tabPanel("Raport sirul lui Fibonacci",
                      h4("Sirul lui Fibonacci este un exemplul clasic al legii lui Benford."),
                      
                      sidebarLayout(
                        sidebarPanel(h4("Fibonacci"),
                                     sliderInput(inputId = "nrFibo",
                                                 label = "Cate numere din sir au fost generate",
                                                 min = 1,
                                                 max = 4000,
                                                 value = 200
                                     )
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Graficul legii lui Benford", plotOutput("dateFibo")),
                            tabPanel("Datele tabelului", DT::dataTableOutput("fiboTabel"))
                          )
                        )
                      )
             )
  ),
)


# Partea de server
server <- function(session, input, output) {
  calculBenford <- function(r, nrInregistrari) {
    
    percent <- c(0 ,0, 0, 0, 0, 0, 0, 0, 0)
    freq <- c(0 ,0, 0, 0, 0, 0, 0, 0, 0)
    benfordV <- c(0 ,0, 0, 0, 0, 0, 0, 0, 0)
    
    #extragem prima cifra dintr-un numar, chiar si dupa virgula
    firstDigit<-function(x) as.numeric(substr(gsub('[0.]', '', x), 1, 1)) 
    
    #Pe dataframe-ul transmis prelucram prima cifra din fiecare inregistrare si adunam la frecventa generala
    for(i in 1:nrInregistrari) {
      num<-firstDigit(r[i])
      freq[num] <- freq[num] + 1
    }
    
    
    for (i in 1:9) {
      percent[i] <- freq[i] / nrInregistrari
    }
    
    for( i in 1:9) {
      benfordV[i] <- log10(1 + 1 / i)
    }
    
    dataFrame <- data.frame (
      Numere = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
      benford = benfordV,
      Procentaj = percent
    )
    
    ggplot(data = dataFrame, aes(x = Numere, group = 1)) + geom_bar(aes(y = Procentaj), stat = "identity", color = "yellow", fill = "orange") + geom_line(aes(y = benford), stat = "identity", color = "green")
    
  }
  
  output$dateName <- renderPlot({
    censusFile <- (read.csv("Nume_comune_2000.csv", header = TRUE))
    
    valCensus <- censusFile$Count
    
    censUS <- data.frame(
      Nume = censusFile$Name,
      Population = censusFile$Count
    )
    
    output$NumeTabel = DT::renderDataTable({censUS})
    
    calculBenford(valCensus, input$numeComune)
  })
  
  output$dateFibo <- renderPlot({
    fiboFile <- (read.csv("fibo.csv", header = TRUE))
    
    valFibo <- fiboFile$Value
    
    
    fibo <- data.frame(
      Index = fiboFile$Iteration,
      Value = fiboFile$Value
    )
    
    output$fiboTabel = DT::renderDataTable({fibo})
    calculBenford(valFibo, input$nrFibo)
  })
  
  output$dateCensus <- renderPlot({
    censusFile <- (read.csv("worldcities.csv", header = TRUE))
    
    valCensus <- censusFile$Population
    
    censUS <- data.frame(
      City = censusFile$city_ascii,
      Population = censusFile$Population
      
    )
    
    output$censusTabel = DT::renderDataTable({censUS})
    
    calculBenford(valCensus, input$roCensus)
  })
  
}

shinyApp(ui = ui, server = server)

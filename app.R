library(shiny)
library(plotly)
library(ggplot2)
library(reshape2)
library(lattice)
library(corrplot)

radio_button_choices = list("Ggplot2" = 1, "Lattice" = 2)
panel_choices = list("Ggplot2" = 1, "Lattice" = 2)

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Wybierz plik CSV",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            ),
            tags$hr(),
            checkboxInput("header", "Header", TRUE),
            radioButtons(inputId = "radio_button", label = h5("Biblioteka graficzna:"), choices = radio_button_choices,selected = character(0)),
            sliderInput(inputId = "value", label = "Zakres zmiennych", min = 0, max = 100, value = c(1, 35)),
            downloadButton('downloadPlot1', 'Pobierz wykres ggplot2'),
            downloadButton('downloadPlot2', 'Pobierz wykres lattice'),
            img(src="image.png",height=150,width=150,style="display: block; margin-left: auto; margin-right: auto; margin-top: 10px;") 
        ),
        mainPanel(
            tabsetPanel(id = "tab",type = "tabs",
                        tabPanel("Tabela", div(DT::dataTableOutput("table.output"),style = "font-size:180%")),
                        tabPanel("Macierz korelacji", uiOutput("cor1")),
                        tabPanel("Wizualizacja korelacji", uiOutput("cor2")),
                        tabPanel(names(panel_choices)[1], value = panel_choices[[1]], plotOutput("plot1")),
                        tabPanel(names(panel_choices)[2], value = panel_choices[[2]], plotOutput("plot2"))
            )
        )
    )
)

server <- function(input, output, session) {
    mydata <- reactive({
        inFile <- input$file1
        if (is.null(inFile))
            return(NULL)
        tbl <- read.csv2(inFile$datapath, header = input$header,sep = "|")
        return(tbl)
    })
    
    output$table.output <- DT::renderDataTable({
        DT::datatable(mydata(), options = list(pageLength = 10))
    })
    
    observeEvent(input$radio_button, {
        updateTabsetPanel(session, "tab", selected = input$radio_button)
    })
    
    output$plot1 <- renderPlot({
        print(plotInput())
    })
    
    plotInput <- reactive({
        md <- mydata()
        md.long <- melt(md, id = "ID", measure = colnames(md)[-1])
        ggplot(md.long, aes(x=md.long[,1], y=value, colour=variable))+ggtitle("Wizualizacja danych z pliku csv")+
            geom_point(colour="white", fill="red", shape=21,size=3)+ scale_size_area(max_size = 15) + 
            scale_y_continuous(limits = c(input$value[1],input$value[2])) + theme_bw() + ylab("Wartosci zmiennych") + xlab("ID")
    })
    
    output$downloadPlot1 = downloadHandler(
        filename = 'test.png',
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
            }
            ggsave(file, plot = plotInput(), device = device)
        }
    )
    
    output$plot2 <- renderPlot({
        print(plotInput2())
    })
    
    plotInput2 <- reactive({
        md <- mydata()
        md.long <- melt(md, id = "ID", measure = colnames(md)[-1])
        xyplot(value~md.long[,1],data = md.long,main="Wizualizacja danych z pliku csv",ylim = c(input$value[1],input$value[2]),
               xlab = "ID",ylab = "Wartosci zmiennych")
    })
    
    output$downloadPlot2 <- downloadHandler(
        filename = '',
        content = function(file) {
            png(file)
            print(plotInput2())
            dev.off()
        })
    
    observeEvent(input$tab, {
        updateRadioButtons(session, "radio_button", selected = input$tab)
    })
    
    output$cor1 <- renderUI({
        output = tagList()
        output[[1]] = tagList()
        output[[1]][[1]] = checkboxGroupInput("chbox2", label = h4("Wybierz co najmniej dwie zmienne:"), 
                                              choices = colnames(mydata()),
                                              selected = colnames(mydata()))
        
        output$cor <- renderPrint({
            cp <- cor(mydata()[, input$chbox2])
            cp
        })
        output
    })
    
    output$cor2 <- renderUI({
        output = tagList()
        output[[1]] = tagList()
        output[[1]][[1]] = checkboxGroupInput("chbox1", label = h4("Wybierz co najmniej dwie zmienne:"), 
                                              choices = colnames(mydata()),
                                              selected = colnames(mydata()))

        output$cor_plot <- renderPlot({
            cp <- cor(mydata()[, input$chbox1])
            pl <- corrplot(cp,method = "number")
            pl
        })
        output
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

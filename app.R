#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plot3D)
library(TruncatedDistributions)
# Define UI for application that draws a 3D of model response surface
# using two independent variables of X1 and X2
# this code is developed to show the effect of prior choices on model response surface
ui <- fluidPage(

    # Application title
    titlePanel("On Global Sensitivity Analysis & Impact of Prior Choices"),

    # Sidebar with a slider input for location of distribution 
    sidebarLayout(
        sidebarPanel(width=3,
            sliderInput("mean",
                        "Location of distribution:",
                        min = -3,
                        max = 3,
                        value = 0,
                        step = 0.5),
            sliderInput("sd",
                        "Scale of distribution:",
                        min = 1,
                        max = 3,
                        value = 2,
                        step = 0.2),
            sliderInput("size",
                        "Sample size:",
                        min = 10,
                        max = 100,
                        value = 20,
                        step = 5)
        ),
        

        # Show a plot of the generated distribution
        mainPanel(width=9,
            plotOutput("distPlot",width=750,heigh=750),
            p(code("Model: Z = cos(X) * sin(Y))")),
            p(code("Y <- X <- r.trunc.norm(n,mean,sd,min,max)")),
            p(strong("By Iman Hosseini-Shakib",style = "font-size:10px;")),
            p(em("when still alive",style = "font-size:8px;"))
            
        )
    ),
   )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate model response from ui.R
       n=input$size
       mean=input$mean
       sd=input$sd
       a=-pi
       b=pi
        set.seed(1);x <- sort(rtnorm(n,mean,sd,a,b))
        set.seed(2);y <- sort(rtnorm(n,mean,sd,a,b))
        grid <- mesh(x, y)
        z    <- with(grid, cos(x) * sin(y))
        par(mfrow=c(2,2),mai=c(0.9,0.9,0.9,0.9))
        r=0.3 # ratio at which the plot area is divided
        par(fig=c(0,r,r,1),new=F)
        plot(density(x)$y,density(x)$x,main="",xlab="Density",
             ylab="X",type='l',lwd=10,col="#00ff7f",
             bty='l',xlim=c(0,0.75))
        par(fig=c(r,1,r,1),new=T)
        persp3D(z = z, x = x, y = y, colvar = z,
                xlim = c(-3, 3),ylim = c(-3, 3),
                zlim = c(-4, 1),colkey = F,contour = T,scale=F)
        par(fig=c(0,r,0,r),new=T)
        p("")
        par(fig=c(r,1,0,r),new=T)
        plot(density(y),main="",xlab="Y",lwd=10,col="#FF2400",bty='l',ylim=c(0,0.75))

    })
}

# Run the application 
shinyApp(ui = ui, server = server)

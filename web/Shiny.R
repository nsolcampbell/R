#RStudio是我最喜欢用的R语言IDE，其开发团队最近又推出了一个新的产品，即Shiny包。
#它的作用是快速搭建基于R的交互网页应用。使得那些对代码不熟悉的人士在工作中也可以应用统计模型。
#对于R和web的交互，之前已经有一些相关的包，例如：rApache, Rhttpd, Rack, Rook。
#不过这些工具都需要开发者不仅要熟悉R，还要熟悉网页编程语言（html,CSS,JS）。
#而Shiny包的特点在于不需要了解网页语言，可以用纯R来搭建。生成的网页应用是动态交互，而且是即时更新的。
#Shiny还提供了现成组件方便快速在网页上展示数据、图表和模型，的确是非常的炫。本例将用ggplot2包来绘制iris数据集的散点图，并将图形放到网页中。

#首先安装Shiny包：
options(repos=c(RStudio='http://rstudio.org/_packages', getOption('repos')))
install.packages('shiny')
install.packages("ggplot2")

#再写两个R代码文件：
#一个是负责前端的ui.R，另一个是负责后端的server.R。

#ui.R的代码如下：
library(shiny)
library(ggplot2)
 
dataset<-iris
 
shinyUI(pageWithSidebar(
 
  headerPanel("鸢尾花的数据展示"),
  sidebarPanel(
 
    selectInput('x', 'X', names(dataset)),
    selectInput('y', 'Y', names(dataset)[2]),
    selectInput('color', 'Color', c('None', names(dataset))),
 
    checkboxInput('smooth', 'Smooth')
    ),
 
  mainPanel(
    plotOutput('plot')
  )
))


#server.R的代码如下：
library(shiny)
library(ggplot2)
 
shinyServer(function(input, output) {
 
  output$plot <- reactivePlot(function() {
 
    p <- ggplot(dataset, aes_string(x=input$x, y=input$y)) + geom_point()
 
    if (input$color != 'None')
      p <- p + 
        aes_string(color=input$color) + 
        theme(legend.position="top")
 
    if (input$smooth)
      p <- p + 
        geom_smooth() + 
        theme(legend.position="top")
 
    print(p)
 
  }, height=400)
 
})

#将这两个代码文件存放到同一个文件夹下，例如我是放在在"d:/rspace/shinyapp"。

#最后在控制台下运行：

library(shiny)
runApp("D:/dropbox/Dropbox/Apps/R/wangye")


#之后R会自动打开系统默认的浏览器，并展示出如下的界面。

#Shiny包的缺点：
#在部署分享方面，Shiny包只能在本地浏览器展示应用。如果要分享给其它人的话，需要将R代码传到网盘或打包分发，而对方也需要使用runApp命令来进行本地展示。
#RStudio团队正在开发Shiny服务器构架，让使用者仅需要一个浏览器和网址就可以运行网页应用。不过这将是一个收费的服务。
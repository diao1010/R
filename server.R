library(shiny)
library(plyr)
library(ggplot2)


  #load data


  resume <- read.csv("resume1611utf.csv", stringsAsFactors = F) 
  colnames(resume) <-c("machineNum","happenTime","timing","condition","conditionG","生產狀態","工號","累積生產數","生產數","巡機員","error")
  
  #heat_map前處理
  abc <-ddply(resume,.(machineNum,error),transform, count = length(error))
  resumeHeat<- unique(abc[c("machineNum", "error","count")])
  
  #pie前處理
  resume1 <-subset(resume, conditionG != "關機" & conditionG != "開機準備" & conditionG != "關機準備")
  condiG<-count(resume1$conditionG)
  condiG$x=as.character(condiG$x)
  condiG$freq=as.numeric(condiG$freq)
  
    shinyServer(function(input, output) {
    countErr<- reactive({less<-subset(resume, timing < input$timing & condition =="異常" & conditionG =="異常")
                        countErr <-count(less$error)
                        countErr$rank <- NA
                        order.freq <- order(countErr$freq, countErr$x, decreasing = T)
                        countErr$rank[order.freq]<- 1:nrow(countErr)
                        countErr=as.data.frame(countErr)
                    })
    

    output$bar<-renderPlot({
      ggplot(countErr(), aes(x=reorder(x,-freq), y=freq))+geom_bar(stat="identity", data = subset(countErr(), rank <= input$num))+ xlab("Error") + ylab("count")
    })
    output$heat_map <-renderPlot({
      ggplot(resumeHeat,aes(x=machineNum,y=error,fill=count))+geom_raster()+scale_fill_gradient(limits=c(0,50))+theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1))
    
      })
    output$pie <- renderPlot({
      ggplot(data=condiG) +
        geom_bar(aes(x=factor(1), y=freq,fill=x), stat = "identity") +coord_polar("y", start=0)+scale_fill_discrete(labels=c("水電問題","未排程","生產","其它","待品保","待研發","待料","待模具","借機","異常","設備故障","換型號","調機")
        )
    })
    

})
  
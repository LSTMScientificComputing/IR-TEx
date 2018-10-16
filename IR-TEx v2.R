library(shiny)
library(dismo)
library(shinythemes)
library(shinycssloaders)
library(WGCNA)

enableWGCNAThreads()
allowWGCNAThreads()

#Read in the data from the same local folder as the app.R file
fold.change.data<-read.delim('Fold Changes.txt',header=T)
geography<-read.delim('geography.txt',header=T)


#Defines the layout of the page, the inputs and the outputs and their associated localisation. Scope for improving the inputs and making them more interactive.
ui <- fluidPage(
	theme = shinytheme("flatly"),

	navbarPage(
		"IR-TEx",
		tabPanel(
			"About",
			includeMarkdown("about.md"),
			a("IR-TEx_UserGuide.pdf",target="_blank",href="IR-TEx_UserGuide.pdf")
		),
		tabPanel(
			"Application",
			sidebarLayout(
				sidebarPanel(
					textInput('textInput','Transcript ID',value='AGAP008212-RA'),
					checkboxGroupInput('CountryInput','Select Relevant Countries',c('Burkina Faso','Cote D`Ivoire','Cameroon','Equatorial Guinea','Zambia','Tanzania','Sudan','Uganda','Togo'),selected=c('Burkina Faso','Cote D`Ivoire','Cameroon','Equatorial Guinea','Zambia','Tanzania','Sudan','Uganda','Togo')),
					checkboxGroupInput('ExposureInput','Select Exposure Status',c('Exposed','Unexposed'),selected=c('Exposed','Unexposed')),
					checkboxGroupInput('SpeciesInput','Select Relevant Species',c('Anopheles gambiae','Anopheles coluzzi','Anopheles arabiensis'),selected = c('Anopheles coluzzi')),
					checkboxGroupInput('InsecticideInput','Select Insecticide Class',c('Pyrethroid','Organochloride','Carbamate','None'),selected = c('Pyrethroid','None')),
					sliderInput('CorrNumberInput','Absolute Correlation Value',min=0.6,max=1,step=0.01,value=c(0.98,1)),
					submitButton("Update View", icon("refresh"))
				),

				mainPanel(
					tags$style(
						type="text/css",
						".shiny-output-error { visibility: hidden; }",
						".shiny-output-error:before { visibility: hidden; }"
					), 
              
					withSpinner(plotOutput("TranscriptExpression")),
					br(),br(),
					tableOutput('ArrayIndex'),
					tableOutput("TranscriptTable"),
					tableOutput('NumberSig'),
					downloadButton('downloadData', 'Download'),
					br(),br(),
					withSpinner(plotOutput("Geography")),
					textOutput('Geography_legend'),
					br(),br(),
					withSpinner(plotOutput("CorrelationNetwork")),
					br(), br(),
					tableOutput("CorrelationTable"),
					downloadButton('CorrData', 'Download')
				)
			)
		)
	)
)

server <- function(input, output) {
  
  expr2<-reactive({  
  fold.change.data<-as.matrix(fold.change.data)
  
  #Change selection for each possible option for selected country
  if(length(input$CountryInput)>1)
  {
    expr2.FC2<-c()
    expr2.Q2<-c()
    
    expr.2.FC2.names<-c()
    expr.2.Q2.names<-c()
    
    for(i in 1:length(input$CountryInput))
    {
      expr2.all<-fold.change.data[,which(fold.change.data[1,]==input$CountryInput[i]),drop=FALSE]
      
      if(ncol(expr2.all)==0)
      {
        next
      }
      
      expr2.Q<-expr2.all[,((ncol(expr2.all)/2)+1):ncol(expr2.all)]
      expr2.FC<-expr2.all[,1:((ncol(expr2.all)/2))]
      
      expr.2.FC2.names<-c(expr.2.FC2.names,colnames(expr2.all)[1:((ncol(expr2.all)/2))])
      expr.2.Q2.names<-c(expr.2.Q2.names,colnames(expr2.all)[((ncol(expr2.all)/2)+1):ncol(expr2.all)])
      
      expr2.FC2<-cbind(expr2.FC2,expr2.FC)
      expr2.Q2<-cbind(expr2.Q2,expr2.Q)
    }
    expr2<-cbind(fold.change.data[,1:3],expr2.FC2,expr2.Q2)
    colnames(expr2)<-c('Systematic Name','Detoxification Class','Transcript Type',expr.2.FC2.names,expr.2.Q2.names)
  }
  
  if(length(input$CountryInput)==1)
  {
    expr2<-fold.change.data[,which(fold.change.data[1,]==input$CountryInput[1])]
  }
  if(length(input$CountryInput)==0)
  {
    return()
  }
  
  
  #Change selection for each possible option for exposure status
  if(length(input$ExposureInput)==1)
  {
    if(input$ExposureInput == 'Exposed')
    {
      expr2<-expr2[,which(expr2[2,]=='Exposed')]
    }
    
    if(input$ExposureInput == 'Unexposed')
    {
      expr2<-expr2[,which(expr2[2,]=='Unexposed')]
    }
  }
  if(length(input$ExposureInput)==2)
  {
    expr2<-expr2
  }
  if(length(input$ExposureInput)==0)
  {
    expr2<-expr2
    return()
  }
  
  
  #Change selection for each possible option for species
  if(length(input$SpeciesInput)>1)
  {
    expr3.FC2<-c()
    expr3.Q2<-c()
    expr.3.FC2.names<-c()
    expr.3.Q2.names<-c()
    for(i in 1:length(input$SpeciesInput))
    {
      expr3.all<-expr2[,which(expr2[3,]==input$SpeciesInput[i]),drop=FALSE]
      if(ncol(expr3.all)==0)
      {
        next
      }
      
      expr3.Q<-expr3.all[,((ncol(expr3.all)/2)+1):ncol(expr3.all)]
      expr3.FC<-expr3.all[,1:((ncol(expr3.all)/2))]
      
      expr.3.FC2.names<-c(expr.3.FC2.names,colnames(expr3.all)[1:((ncol(expr3.all)/2))])
      expr.3.Q2.names<-c(expr.3.Q2.names,colnames(expr3.all)[((ncol(expr3.all)/2)+1):ncol(expr3.all)])
      
      expr3.FC2<-cbind(expr3.FC2,expr3.FC)
      expr3.Q2<-cbind(expr3.Q2,expr3.Q)
    }
    expr3<-cbind(expr3.FC2,expr3.Q2)
    colnames(expr3)<-c(expr.3.FC2.names,expr.3.Q2.names)
  }
  if(length(input$SpeciesInput)==1)
  {
    expr3<-expr2[,which(expr2[3,]==input$SpeciesInput[1])]
  }
  if(length(input$SpeciesInput)==0)
  {
    return()
  }
  
  expr3<-cbind(fold.change.data[,1:3],expr3)
  
  #Insecticide Input
  if(length(input$InsecticideInput)>1)
  {
    expr2.FC2<-c()
    expr2.Q2<-c()
    expr.2.FC2.names<-c()
    expr.2.Q2.names<-c()
    for(i in 1:length(input$InsecticideInput))
    {
      expr2.all<-expr3[,which(expr3[4,]==input$InsecticideInput[i]),drop=FALSE]
      
      if(ncol(expr2.all)==0)
      {
        next
      }
      
      expr2.Q<-expr2.all[,((ncol(expr2.all)/2)+1):ncol(expr2.all)]
      expr2.FC<-expr2.all[,1:((ncol(expr2.all)/2))]
      
      expr.2.FC2.names<-c(expr.2.FC2.names,colnames(expr2.all)[1:((ncol(expr2.all)/2))])
      expr.2.Q2.names<-c(expr.2.Q2.names,colnames(expr2.all)[((ncol(expr2.all)/2)+1):ncol(expr2.all)])
      
      expr2.FC2<-cbind(expr2.FC2,expr2.FC)
      expr2.Q2<-cbind(expr2.Q2,expr2.Q)
    }
    expr2<-cbind(expr3[,1:3],expr2.FC2,expr2.Q2)
    colnames(expr2)<-c('Systematic Name','Detoxification Class','Transcript Type',expr.2.FC2.names,expr.2.Q2.names)
  }
  if(length(input$InsecticideInput)==1)
  {
    expr2<-expr3[,which(expr3[4,]==input$InsecticideInput)]
  }
  if(length(input$InsecticideInput)==0)
  {
    return()
  }
  
  #Plot the graph for the selected data
  expr2<-cbind(fold.change.data[,1:3],expr2)
  
})


  output.cor2<-reactive({
    fold.change.data<-as.matrix(fold.change.data)
    
    #Change selection for each possible option for selected country
    if(length(input$CountryInput)>1)
    {
      expr2.FC2<-c()
      expr2.Q2<-c()
      
      expr.2.FC2.names<-c()
      expr.2.Q2.names<-c()
      
      for(i in 1:length(input$CountryInput))
      {
        expr2.all<-fold.change.data[,which(fold.change.data[1,]==input$CountryInput[i]),drop=FALSE]
        
        if(ncol(expr2.all)==0)
        {
          next
        }
        
        expr2.Q<-expr2.all[,((ncol(expr2.all)/2)+1):ncol(expr2.all)]
        expr2.FC<-expr2.all[,1:((ncol(expr2.all)/2))]
        
        expr.2.FC2.names<-c(expr.2.FC2.names,colnames(expr2.all)[1:((ncol(expr2.all)/2))])
        expr.2.Q2.names<-c(expr.2.Q2.names,colnames(expr2.all)[((ncol(expr2.all)/2)+1):ncol(expr2.all)])
        
        expr2.FC2<-cbind(expr2.FC2,expr2.FC)
        expr2.Q2<-cbind(expr2.Q2,expr2.Q)
      }
      expr2<-cbind(fold.change.data[,1:3],expr2.FC2,expr2.Q2)
      colnames(expr2)<-c('Systematic Name','Detoxification Class','Transcript Type',expr.2.FC2.names,expr.2.Q2.names)
    }
    
    
    if(length(input$CountryInput)==1)
    {
      expr2<-fold.change.data[,which(fold.change.data[1,]==input$CountryInput[1])]
    }
    if(length(input$CountryInput)==0)
    {
      return()
    }
    
    
    #Change selection for each possible option for exposure status
    if(length(input$ExposureInput)==1)
    {
      if(input$ExposureInput == 'Exposed')
      {
        expr2<-expr2[,which(expr2[2,]=='Exposed')]
      }
      
      if(input$ExposureInput == 'Unexposed')
      {
        expr2<-expr2[,which(expr2[2,]=='Unexposed')]
      }
    }
    if(length(input$ExposureInput)==2)
    {
      expr2<-expr2
    }
    if(length(input$ExposureInput)==0)
    {
      expr2<-expr2
      return()
    }
    
    
    #Change selection for each possible option for species
    if(length(input$SpeciesInput)>1)
    {
      expr3.FC2<-c()
      expr3.Q2<-c()
      expr.3.FC2.names<-c()
      expr.3.Q2.names<-c()
      for(i in 1:length(input$SpeciesInput))
      {
        expr3.all<-expr2[,which(expr2[3,]==input$SpeciesInput[i]),drop=FALSE]
        if(ncol(expr3.all)==0)
        {
          next
        }
        
        expr3.Q<-expr3.all[,((ncol(expr3.all)/2)+1):ncol(expr3.all)]
        expr3.FC<-expr3.all[,1:((ncol(expr3.all)/2))]
        
        expr.3.FC2.names<-c(expr.3.FC2.names,colnames(expr3.all)[1:((ncol(expr3.all)/2))])
        expr.3.Q2.names<-c(expr.3.Q2.names,colnames(expr3.all)[((ncol(expr3.all)/2)+1):ncol(expr3.all)])
        
        expr3.FC2<-cbind(expr3.FC2,expr3.FC)
        expr3.Q2<-cbind(expr3.Q2,expr3.Q)
      }
      expr3<-cbind(expr3.FC2,expr3.Q2)
      colnames(expr3)<-c(expr.3.FC2.names,expr.3.Q2.names)
    }
    if(length(input$SpeciesInput)==1)
    {
      expr3<-expr2[,which(expr2[3,]==input$SpeciesInput[1])]
    }
    if(length(input$SpeciesInput)==0)
    {
      return()
    }
    
    
    expr3<-cbind(fold.change.data[,1:3],expr3)
    
    #Insecticide Input
    if(length(input$InsecticideInput)>1)
    {
      expr2.FC2<-c()
      expr2.Q2<-c()
      expr.2.FC2.names<-c()
      expr.2.Q2.names<-c()
      for(i in 1:length(input$InsecticideInput))
      {
        expr2.all<-expr3[,which(expr3[4,]==input$InsecticideInput[i]),drop=FALSE]
        
        if(ncol(expr2.all)==0)
        {
          next
        }
        
        expr2.Q<-expr2.all[,((ncol(expr2.all)/2)+1):ncol(expr2.all)]
        expr2.FC<-expr2.all[,1:((ncol(expr2.all)/2))]
        
        expr.2.FC2.names<-c(expr.2.FC2.names,colnames(expr2.all)[1:((ncol(expr2.all)/2))])
        expr.2.Q2.names<-c(expr.2.Q2.names,colnames(expr2.all)[((ncol(expr2.all)/2)+1):ncol(expr2.all)])
        
        expr2.FC2<-cbind(expr2.FC2,expr2.FC)
        expr2.Q2<-cbind(expr2.Q2,expr2.Q)
      }
      expr2<-cbind(expr3[,1:3],expr2.FC2,expr2.Q2)
      colnames(expr2)<-c('Systematic Name','Detoxification Class','Transcript Type',expr.2.FC2.names,expr.2.Q2.names)
    }
    if(length(input$InsecticideInput)==1)
    {
      expr2<-expr3[,which(expr3[4,]==input$InsecticideInput)]
    }
    if(length(input$InsecticideInput)==0)
    {
      return()
    }
    
    
    #expr2<-cbind(fold.change.data[,1:3],expr2)
    expr4<-expr3[-c(1:4),-c(1:3)]
    gene.names<-expr3[-c(1:4),1]
    expression<-expr4[,1:((ncol(expr2)/2))]
    mode(expression)<-'numeric'
    cor.matrix<-WGCNA::cor(t(expression), nThreads= 15)
    
    
  })
  

  
  output.cor<-reactive({
    expr3<-expr2()
    expr4<-expr3[-c(1:4),-c(1:3)]
    gene.names<-expr3[-c(1:4),1]
    #Find the input transcript in the overall table
    gene.position<-which(gene.names==input$textInput[1])
    
    #Find the transcript in the correlation matrix, and extract that row
    cor.matrix<-output.cor2()
    cor.matrix.gene<-cor.matrix[gene.position[1],]
    cor.matrix.gene<-t(cor.matrix.gene)
    
    colnames(cor.matrix.gene)<-gene.names
    
    cor.matrix.gene<-as.matrix(cor.matrix.gene)
    
    output<-as.matrix(cor.matrix.gene[,c(which(as.numeric(abs(cor.matrix.gene))>=as.numeric(input$CorrNumberInput)))])
  })
  


  output$TranscriptExpression <- renderPlot({
    
      expr3<-expr2()
      expr3<-expr3[-c(1:4),]
      expr.for.plot<-as.matrix(expr3)
      col.names<-colnames(expr3)
      positions<-grep('FC',col.names)
      
      array.index<-as.matrix(col.names[positions],length(col.names[positions]),1)
      array.index<-t(array.index)
      names<-array.index
      number<-length(names)
      
      validate(need(!is.na(((expr.for.plot[which(expr.for.plot[,1]==input$textInput[1])[1],grep('FC',colnames(expr.for.plot))]))),'Please Check Inputs - (1) Ensure transcript ID is entered (e.g ending in -RA) (2) Transcript ID may not be available on the microarray (3) No microarray datasets match selection criteria. Please Try Again.'))
      
      mp<-plot(log2(as.numeric(expr.for.plot[which(expr.for.plot[,1]==input$textInput[1])[1],grep('FC',colnames(expr.for.plot))])),ylab='Log2 Fold Change',xlab='',type='b',axes=T,xaxt='n')
    
      axis(1,at=1:number,labels=FALSE)
      text(x=seq(1,length(names),by=1), par("usr")[3],labels = names, srt = 35, pos = 1,offset = 2.25, xpd = TRUE,cex=1)
      #text(x=seq(1,length(names),by=1), par("usr")[3]-0.25,labels = names, srt = 35, adj = 1, xpd = TRUE,cex=1)
      #text(x=seq(1,length(names),by=1), par("usr")[3]-0.25,adj = c(1,1.5), labels = names, srt = 35, xpd = TRUE,cex=1)
      abline(h=0,lty=2,col='cornsilk4')
      abline(h=0.25,lty=2,col='cornsilk3')
      abline(h=-0.25,lty=2,col='cornsilk3')
      axis(1,at=seq(1,30,1),labels=F)
})
  

  
  output$TranscriptTable <- renderTable({
    
    expr3<-expr2()
    expr3<-expr3[-c(1:4),]
    
    if(length(which(expr3[,1]==input$textInput[1]))==1)
    {
      filtered.table<-data.frame(as.list(expr3[which(expr3[,1]==input$textInput[1]),]))
    }
    else
    {
      filtered.table<-expr3[which(expr3[,1]==input$textInput[1]),]
    }

      if(length(input$InsecticideInput)==1)
      {
        filtered.table<-filtered.table
      }
    else
    {
      filtered.table<-filtered.table[,-c(1:3)]
    }

  })
  
  output$NumberSig <- renderTable({
    
    expr3<-expr2()
    expr3<-expr3[-c(1:4),]
    
    filtered.table<-data.frame(as.list(expr3[which(expr3[,1]==input$textInput[1])[1],]))
    filtered.table<-filtered.table[,-c(1:3)]
    #filtered.table.Q<-filtered.table[,((ncol(filtered.table)/2)+1):ncol(filtered.table)]
    filtered.table.Q<-filtered.table[,grep('Q',colnames(filtered.table))]
    filtered.table.Q<-as.matrix(filtered.table.Q)
    filtered.table.Q<-as.numeric(filtered.table.Q)
    
    Significant<-length(which(filtered.table.Q<0.05))
    Total<-length(filtered.table.Q)
    
    result<-cbind(Significant,Total)
    
  })

  output$downloadData <- downloadHandler(
    filename = function() { paste(input$textInput[1], '.tsv', sep='') },
    content = function(file) {
      
      expr3<-expr2()
      
      expr3<-expr3[-c(1:4),]
      
      if(length(which(expr3[,1]==input$textInput[1]))==1)
      {
        filtered.table<-data.frame(as.list(expr3[which(expr3[,1]==input$textInput[1]),]))
      }
      else
      {
        filtered.table<-expr3[which(expr3[,1]==input$textInput[1]),]
      }
      if(length(input$InsecticideInput)==1)
      {
        filtered.table<-filtered.table
      }
      else
      {
        filtered.table<-filtered.table[,-c(1:3)]
      }
      write.table(filtered.table, file,sep='\t',row.names=F)
      })
  
  output$Geography <- renderPlot({

    
    expr3<-expr2()
    expr3<-expr3[-c(1:4),]
    
    filtered.table<-data.frame(as.list(expr3[which(expr3[,1]==input$textInput[1])[1],]))
    filtered.table<-filtered.table[,-c(1:3)]
    #filtered.table.Q<-filtered.table[,((ncol(filtered.table)/2)+1):ncol(filtered.table)]
    filtered.table.Q<-filtered.table[,grep('Q',colnames(filtered.table))]
    names<-colnames(filtered.table.Q)
    filtered.table.Q<-as.matrix(filtered.table.Q)
    filtered.table.Q<-as.numeric(filtered.table.Q)
    Significant<-which(filtered.table.Q<0.05)
    
    names<-names[Significant]
    
    
    
    if(length(names)>1)
    {
      new.geography<-c()
      for(i in 1:length(names))
      {
        positions<-which(geography[,1]==names[i])
        
        new.geography<-rbind(new.geography,geography[positions,2:3])
      }
    }
    if(length(names)==1)
    {
      new.geography<-c(geography[which(geography[,1]==names[i]),2:3])
    }
    
    
    if(length(names)>1)
    {
      colours<-c()
      for(i in 1:length(names))
      {
        new.name<-substr(names[i],1,nchar(names[i])-1)
        positions<-which(colnames(filtered.table)==paste(new.name,'FC',sep=''))
        new.FCs<-unlist(filtered.table[,positions])
        new.FCs<-as.numeric(levels(new.FCs))[new.FCs]

        
        if(new.FCs >= 5)
        {
          colours<-c(colours,'red')
        }
        
        if(new.FCs>1 && new.FCs < 5)
        {
          colours<-c(colours,'orange')
        }
        
        if(new.FCs < 1 )
        {
          colours<-c(colours,'green')
        }
      }
    }
    if(length(names)==1)
    {
      colours<-c()
      new.name<-substr(names[1],1,nchar(names[1])-1)
      new.FCs<-filtered.table[,colnames(filtered.table)==paste(new.name,'FC',sep='')]
      new.FCs<-as.numeric(levels(new.FCs))[new.FCs]
      
      
      if(new.FCs >= 5)
      {
        colours<-c(colours,'red')
      }
      
      if(new.FCs>1 && new.FCs < 5)
      {
        colours<-c(colours,'orange')
      }
      
      if(new.FCs < 1)
      {
        colours<-c(colours,'green')
      }
    }
    

    all.data<-cbind(new.geography,colours,names)
    
    new.geography<-cbind(new.geography,rep('Africa',length(names)))
    colnames(new.geography)<-c('Longitude','Latitude','Country')
    
    
    
    coordinates(new.geography)<-c('Longitude','Latitude')
    
    gbmap<-gmap(new.geography,type='roadmap')
    new.geography.merc <- Mercator(new.geography)
    plot(gbmap)
    points(new.geography.merc,pch=21,bg=colours,cex=2)

  })
  
  output$Geography_legend <- renderText({ 
    paste("Significant Transcripts Only (p", as.expression("<="),"0.05): FC > 5 = Red, FC > 1 = Amber, FC < 1 = Green",sep="")
  })
  
  output$CorrelationNetwork <- renderPlot({
    

    
    
    output.cor2<-output.cor()
    expr3<-expr2()
    if(length(input$InsecticideInput)==1)
    {
      expr3<-expr3[-c(1:4),]
    }
    else
    {
      expr3<-expr3[-c(1:4),-c(1:3)]
    }
    
    
    hubs<-c(row.names(output.cor2))
    hubs<-unique(hubs)
    
    col.names<-colnames(expr3)
    positions<-grep('FC',col.names)
    
    array.index<-as.matrix(col.names[positions],length(col.names[positions]),1)
    array.index<-t(array.index)
    names<-array.index
    number<-length(names)
    
    
    #Define colours for the graph.
    colours.vector<-colors(distinct = TRUE)
    colours.vector<-colours.vector[-c(137:234)]
    colours.vector<-colours.vector[-21]
    colours.for.graph<-colours.vector[sample(1:403,length(hubs), replace=T)]
    colours.for.graph<-c(colours.for.graph)
    
    #Plot output.
    plot(1:ncol(expr3),1:ncol(expr3),col=colours.for.graph[1],axes=T,ylim=c(-10,10),xlim=c(1,(ncol(expr3)/2)),type='n',ylab='Log2 Fold Change',xlab='',xaxt='n')
    
    box()
    for(i in 1:length(hubs))
    {
      positions<-grep(hubs[i],expr3[,1])
      plot.points<-expr3[positions[1],4:(3+((ncol(expr3)-3)/2))]
      plot.points<-unname(plot.points)
      mode(plot.points)<-'numeric'
      points(1:length(plot.points),log2(plot.points),col=colours.for.graph[i])
      lines(1:length(plot.points),log2(plot.points),col=colours.for.graph[i])
      par(new = TRUE)
    }
    abline(h=0,col='gray',lty=2)
    #axis(1,at=1:number,labels=names,las=2,cex.axis=0.8)
    axis(1,at=1:number,labels=FALSE)
    text(x=seq(1,length(names),by=1), par("usr")[3]-0.25,adj = c(1,1.5), labels = names, srt = 35, xpd = TRUE,cex=1)
    
    if(length(hubs)<50)
    {
      legend('topright',hubs,cex=.6,col=colours.for.graph,lty=1, bty='n',ncol=6)
    }
    
  })
  
  output$CorrelationTable <- renderTable({
    output.cor2<-output.cor()
    expr3<-expr2()
    
    hubs<-c(row.names(output.cor2))
    hubs<-unique(hubs)
    
    if(length(input$InsecticideInput)==1)
    {
      vector.out<-c()
      for(i in 1:length(hubs))
      {
        positions<-grep(hubs[i],expr3[,1])
        vector.out<-rbind(vector.out,c(output.cor2[grep(hubs[i],row.names(output.cor2))[1],1],expr3[positions[1],]))
      }
    }
    else
    {
      vector.out<-c()
      for(i in 1:length(hubs))
      {
        positions<-grep(hubs[i],expr3[,1])
        vector.out<-rbind(vector.out,c(output.cor2[grep(hubs[i],row.names(output.cor2))[1],1],expr3[positions[1],-c(1:3)]))
      }
    }
    
    colnames(vector.out)[1]<-'Correlation'
    return(vector.out)
    
  })
  
  output$CorrData <- downloadHandler(
    
    filename = function() { paste(input$textInput[1],' Correlation Network', '.tsv', sep='') },
    content = function(file){
      output.cor2<-output.cor()
      expr3<-expr2()
      
      hubs<-c(row.names(output.cor2))
      hubs<-unique(hubs)
      
      if(length(input$InsecticideInput)==1)
      {
        vector.out<-c()
        for(i in 1:length(hubs))
        {
          positions<-grep(hubs[i],expr3[,1])
          vector.out<-rbind(vector.out,c(output.cor2[grep(hubs[i],row.names(output.cor2))[1],1],expr3[positions[1],]))
        }
      }
      else
      {
        vector.out<-c()
        for(i in 1:length(hubs))
        {
          positions<-grep(hubs[i],expr3[,1])
          vector.out<-rbind(vector.out,c(output.cor2[grep(hubs[i],row.names(output.cor2))[1],1],expr3[positions[1],-c(1:3)]))
        }
      }
      colnames(vector.out)[1]<-'Correlation'
      
      write.table(vector.out, file,sep='\t',row.names=F)
    })
}

shinyApp(ui = ui, server = server)

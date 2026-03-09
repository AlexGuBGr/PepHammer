

subsetModal <- function(session) {
    ns <- session$ns
    showModal(modalDialog(
      title = "Upload own data here",
      {
        div( 
		fluidRow(column(12, strong("Upload TXT files with each peptide on a separate line."))),
		br(),
		fluidRow(column(12, fileInput("fileid", "Upload file",multiple = FALSE, accept = c(".txt")) ))
		)
      }, size = "l",
	  
      footer = tagList(
        actionButton("cancelmodal", "Cancel", class = "btn btn-danger"),
        actionButton("accepto", "Confirm", class = "btn btn-success")
      )))
  }



shinyServer(function(input, output, session) {
	
	dat <- reactiveVal()
	starter <- reactiveVal()

	  observe({
		for ( i in 1:2 ) {gc()}
		shiny::invalidateLater(10000)
	  })


	observeEvent( input$search, {
		req(input$inppeps)
		get_data(starter, dat, input$inppeps, compat, aa_levels, allbionames, input$searchtype, grantm, input$inclpred)
	})
	
	
	observeEvent(starter(), {
	
		if (input$searchtype == "Hamming") {
			cnter <- 1000 - ((1 - input$inclpred ) * 500)
		} else {
			cnter <- 1500 - ((1 - input$inclpred ) * 850)
		}
		
		shinyjs::runjs('document.getElementById("tmploader").innerHTML = "Loading..."')
		shinyjs::runjs(paste0("startCounter(",starter(),",",cnter,")"))
	})
	
	
	observeEvent(input$upld, {
		subsetModal(session)
	})
	

	output$foundpeps <- DT::renderDataTable(
		DT::datatable({ dat() },
					options = list(ScrollX=TRUE)
	  ), server = TRUE)
	  # %>% formatRound(purrr::map_lgl(.$x$data, is.numeric), digits = 3)

	observeEvent(input$clear, {
		shiny::updateTextAreaInput(
			inputId="inppeps",
			value=""
		)
	})


	observeEvent(input$cancelmodal, {
		removeModal()
	})

	observeEvent(input$accepto, {
		shiny::updateTextAreaInput(
			inputId= "inppeps",
			value=paste(readLines(input$fileid$datapat), collapse="\n")
		)
		removeModal()
	})


	output$dld <- downloadHandler(
		filename = function() {
		  paste("data-", Sys.Date(), ".xlsx", sep="")
		},
		content = function(file) {
		  writexl::write_xlsx(dat(), file)
		}
	  )

	
	output$clicked <- renderPlotly({
		req(dat())
		req(input$foundpeps_rows_selected)
		
		bioactivity_of_rows(dat()[ na.omit(input$foundpeps_rows_selected[1:6]),], allbionames)
	})
	
	
	
	output$lenvsbio <- renderPlotly({
	
		make_length_vs_biofunction(input$biofunction, dbpath)
	})

	output$biovslen <- renderPlotly({
	
		make_biofunction_vs_length(input$pepsize, allbionames, dbpath)
	
	})


})
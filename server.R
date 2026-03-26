

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


arrange_db_tbls <- function(inputchoosedb, inputinclpred, dbx, tablx, allbionamesx) {

		dbx( dbselector[[inputchoosedb]] )
		tablx( tableselector[[inputchoosedb]])
		allbionamesx( colnameselector[[inputchoosedb]] )


		#if (inputchoosedb == "Peptipedia" ) {
		#	dbx( dbselector[[inputchoosedb]] )
		#	tablx( "peptipedia" )
		#	allbionamesx(allbionames_pep)
			
		#} else if (inputchoosedb == "Peptipedia|Tissue" ) { 
			
		#	dbx( dbselector[[inputchoosedb]] )
		#	#tablx( "peptipedia_tissue" )
		#	tablx( tableselector[[inputchoosedb]])
		#	allbionamesx( colnameselector[inputchoosedb]] )
			#allbionamesx( c( allbionames_pep, tissues ) )
			
			#if (inputinclpred == T) {
			#	allbionamesx( c( allbionamesx(), tissues ) )
			#} else {
			#	allbionamesx( c( tissues ) )
			#}
			
		#} else if (inputchoosedb == "NeuroPep_v2") {
			
		#	dbx( dbselector[[inputchoosedb]] )
		#	tablx( "neuropepv2" )
		#	allbionamesx(familynames)
			
			
		#} else if (inputchoosedb == "MultiPep") {
		#	dbx( dbselector[[inputchoosedb]] )
		#	tablx( "pepti_multipep" )
		#	allbionamesx(mulpclass)

			
		#} else if (inputchoosedb == "MultiPep|Tissue") {
		
		#	dbx( dbselector[[inputchoosedb]] )
		#	tablx( "multipep_tissue" )
		#	allbionamesx( c( mulpclass, tissues) )
		
			#if (inputinclpred == T) {
			#	allbionamesx( c( allbionamesx(), tissues ) )
			#} else {
			#	allbionamesx( c( tissues ) )
			#}

		#}
}


# TODO:
# the filtering based on score and  miss-match interval is OFF !!!


shinyServer(function(input, output, session) {
	
	biodistplot <- reactiveVal()
	dat <- reactiveVal()
	dat_init <- reactiveVal()
	starter <- reactiveVal()
	searchtypeval <- reactiveVal()
	
	allbionames_stat <- reactiveVal()
	statdb <- reactiveVal()
	stattabl <- reactiveVal()
	stattablfig <- reactiveVal()
	keybio <- reactiveVal(NULL)
	keybio2 <- reactiveVal(NULL)
	keylen <- reactiveVal(NULL)
	
	allbionames <- reactiveVal()
	tabl <- reactiveVal()
	db <- reactiveVal()

	  observe({
		for ( i in 1:2 ) {gc()}
		shiny::invalidateLater(10000)
	  })


	observeEvent(c(input$choosedb, input$inclpred ), {
	
		if (input$choosedb == "NeuroPep_v2") {
			updateCheckboxInput(session, "inclpred", value = T)
			shinyjs::disable("inclpred")
			updateCheckboxInput(session, "tissue", value = F)
		} else {
			shinyjs::enable("inclpred")
		}
	
		arrange_db_tbls(input$choosedb, input$inclpred, db, tabl, allbionames)
		
		
	})

	observeEvent(input$choosedbstat,{
		#keybio(NULL)
		#keylen(NULL)
		updateSelectizeInput(session, "pepsize", choices = c("-"))
		updateSelectizeInput(session, "biofunction", choices = c("-"))
		updateSelectizeInput(session, "biofunction2", choices = c("-"))
	}, priority=1)
	
	
	observeEvent( c(input$choosedbstat, input$inclpredstat), {
		
		arrange_db_tbls(input$choosedbstat, input$inclpredstat, statdb, stattabl, allbionames_stat)
		
		if ( grepl("Tissue", input$choosedbstat)) {
			stattablfig( c("length_bio_dist_tissue", "bioact_vs_bioact_tissue") )
		} else {
			stattablfig( c("length_bio_dist", "bioact_vs_bioact") )
		}
		
		#if ( grepl("multipep", statdb() ) ) {
		#	shinyjs::show("statthr")
		#	updateSelectizeInput(session, "pepsize", choices = c("All", load_columns_from_table("Pep_length", paste0( stattablfig(), stringr::str_replace( stringr::str_replace(input$statthr, ">", ""), "[.]", "") ) , statdb() )[[1]] ) )
		#} else if ( grepl("neuropepv2", statdb()) ) {
		#	shinyjs::hide("statthr")
		#	updateSelectizeInput(session, "pepsize", choices = c("All", load_columns_from_table("Pep_length", paste0( stattablfig(), "05" ), statdb() )[[1]] ) )
		#} else {
		#	shinyjs::hide("statthr")
		#	updateSelectizeInput(session, "pepsize", choices = c("All", load_columns_from_table("Pep_length", stattablfig(), statdb() )[[1]] ) )
		#}
		updateSelectizeInput(session, "pepsize", choices = c("All", load_columns_from_table("Pep_length", stattablfig()[[1]], statdb() )[[1]] ) )
		updateSelectizeInput(session, "biofunction", choices = c("All", allbionames_stat()) )
		updateSelectizeInput(session, "biofunction2", choices = allbionames_stat() )
		
	}, priority=0)



	observeEvent( input$search, {
		req(input$inppeps)
		dat_init(NULL)
		searchtypeval(input$searchtype)
		get_data(starter, dat, input$inppeps, compat, aa_levels, allbionames(), input$searchtype, grantm, input$inclpred, db(), tabl() )
		
		if ( grepl("multipep", db()) ) {
			shinyjs::show( "predscore" )
		} else {
			shinyjs::hide( "predscore" )
		}
		
		if ( grepl("tissue", tabl()) ) {
			shinyjs::show( "onlytis" )
		} else {
			shinyjs::hide( "onlytis" )
		}
		
	})
	
	
	observeEvent(starter(), {
	
		if (searchtypeval() == "Hamming") {
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

	foundpepsproxy <- DT::dataTableProxy("foundpeps")

	observeEvent(input$clearselectedrows, {
		DT::selectRows(foundpepsproxy, NULL)
	})


	observeEvent(input$filtering, {
		if ( input$filtering == T) {
			shinyjs::show("filteringopts")
		} else {
			shinyjs::hide("filteringopts")
		}
	})
	
	observeEvent(dat(), {
		#req(input$choosedb == "Peptipedia")
	
		if (is.null(dat())) {
			biodistplot(NULL)
			shinyjs::hide("pepdistfound")
			
			if ( is.null( dat_init() ) ) {
				updateCheckboxInput(session, "filtering", value=F)
				shinyjs::disable("filtering")
			}

		} else {
			biodistplot(make_found_biofunction_dist(dat(), allbionames(), db(), input$predscore ))
			#req( is.null(dat_init()) )
			
			shinyjs::enable("filtering")
			
			if (searchtypeval() %in% c("Exact match", "Smaller matching Peptides", "Larger containing peptides")) {
				shinyjs::hide("divboth")
			} else {
				shinyjs::show("divboth")
				if ( searchtypeval() == "Hamming") {
					shinyjs::hide("distanceval")
					shinyjs::hide("prcent_of_worst")
					shinyjs::show("score")
				} else {
					shinyjs::show("distanceval")
					shinyjs::show("prcent_of_worst")
					shinyjs::hide("score")
				}
			}

		update_filtering_options(session, searchtypeval(), dat(), allbionames(), input$predscore, dat_init())
		
		}
	
	}, ignoreNULL = F)


	observeEvent(input$clearfilter, {
		if ( !is.null(dat_init()) ) {
			dat(dat_init())
			dat_init(NULL)
		}
	})


	observeEvent(input$apply_filter, {
		if ( is.null(dat_init()) ) {
			dat_init(dat())
		}
		req( dat_init() )
		dat( filtering_function(dat(), searchtypeval(), allbionames(), input, db() ) )
	})


	output$pepdistfoundplot <- renderPlotly({
		req(biodistplot())
		biodistplot()
	})

	observeEvent(input$showbiodist, {
		req(biodistplot())
		shinyjs::toggle("pepdistfound")
	})
	
	
	observeEvent(input$foundpeps_rows_selected, {
		if (is.null(input$foundpeps_rows_selected)) {
			shinyjs::hide("pepholder")
		} else {
			shinyjs::show("pepholder")
		}
	}, ignoreNULL = F)
	
	output$clicked <- renderPlotly({
		req(dat())
		req(input$foundpeps_rows_selected)
		
		bioactivity_of_rows(dat()[ head(input$foundpeps_rows_selected, 6),], allbionames(), db(), input$predscore)
	})
	
	
	#### Statistics tab ###################

	observeEvent(input$biofunction,{
		keybio(input$biofunction)
	}, ignoreNULL = F)
	
	observeEvent(input$biofunction2,{
		keybio2(input$biofunction2)
	}, ignoreNULL = F)
	
	
	observeEvent(input$pepsize,{
		keylen(input$pepsize)
	}, ignoreNULL = F)
	
	output$lenvsbio <- renderPlotly({
		req(keybio())
		req(keybio() != "-")
		make_length_vs_biofunction(keybio(), statdb(), stattablfig()[[1]], input$statthr  )
	})

	output$biovslen <- renderPlotly({
		req(keylen())
		req(keylen() != "-")
		make_biofunction_vs_length(keylen(), allbionames_stat(), statdb(), stattabl(), stattablfig()[[1]], table_lengths, input$stattisorpred, tissues )
	
	})

	output$bioacrossbio <- renderPlotly({
		req(keybio2())
		req(keybio2() != "-")
		make_overlay_bar_bio_vs_bio(statdb(), stattablfig()[[2]], keybio2(), tissues, input$stattisorpred)
		#make_length_vs_biofunction(keybio(), statdb(), stattablfig()[[1]], input$statthr  )
	})

})
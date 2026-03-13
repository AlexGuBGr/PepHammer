
theme <- bs_theme(
  version = 5,
  bootswatch = NULL,  
  primary = "#1f2937", 
  secondary = "#6b7280",
  
  bg = "#f1f5f9",
  fg = "#0f172a",
  
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  code_font = font_google("JetBrains Mono")
)


ui <- page_fluid(
	shinyjs::useShinyjs(),
	
	
	tags$head(
	  tags$script(HTML("
		let myTimer = null;
	  
		function startCounter(totalCount, cnter) {

		  if (myTimer) clearInterval(myTimer);

		  let counter = 0;
		  const loader = document.getElementById('tmploader');

		  if (!loader) {
			  clearInterval(myTimer);
			  return;
			}

		  myTimer = setInterval(function () {

			counter++;

			let prgs = '='.repeat(Math.round(Math.min(counter / totalCount, 1) * 42));

			loader.textContent = prgs;

			if (counter > totalCount) {
			  counter = Math.round(totalCount * ((Math.random() * 0.15) + 0.5));
			  loader.textContent = ' - almost there!';
			  //loader.textContent = '='.repeat(Math.round(Math.min(counter / totalCount, 1) * 42));
			}
		  }, cnter);
		}
	  ")),
	
	  tags$style(HTML("
	  
		body {
		  background-color: #f8fafc;
		}
		
		.app-header {
		  margin-bottom: 2rem;
		}
		
		.card {
		  border: none;
		  border-radius: 12px;
		  box-shadow: 0 4px 12px rgba(0,0,0,0.05);
		}
		
		.card-header {
		  font-weight: 600;
		  background: white;
		  border-bottom: 1px solid #e5e7eb;
		}
		
		.form-control {
		  border-radius: 8px;
		}
		
		.loader {
        transform: rotateZ(45deg);
        perspective: 1000px;
        border-radius: 50%;
        width: 48px;
        height: 48px;
        color: #3b82f6;
      }
        .loader:before,
        .loader:after {
          content: '';
          display: block;
          position: absolute;
          top: 0;
          left: 0;
          width: inherit;
          height: inherit;
          border-radius: 50%;
          transform: rotateX(70deg);
          animation: 1s spin linear infinite;
        }
        .loader:after {
          color: #FF3D00;
          transform: rotateY(70deg);
          animation-delay: .4s;
        }

      @keyframes rotate {
        0% {
          transform: translate(-50%, -50%) rotateZ(0deg);
        }
        100% {
          transform: translate(-50%, -50%) rotateZ(360deg);
        }
      }

      @keyframes rotateccw {
        0% {
          transform: translate(-50%, -50%) rotate(0deg);
        }
        100% {
          transform: translate(-50%, -50%) rotate(-360deg);
        }
      }

      @keyframes spin {
        0%,
        100% {
          box-shadow: .2em 0px 0 0px currentcolor;
        }
        12% {
          box-shadow: .2em .2em 0 0 currentcolor;
        }
        25% {
          box-shadow: 0 .2em 0 0px currentcolor;
        }
        37% {
          box-shadow: -.2em .2em 0 0 currentcolor;
        }
        50% {
          box-shadow: -.2em 0 0 0 currentcolor;
        }
        62% {
          box-shadow: -.2em -.2em 0 0 currentcolor;
        }
        75% {
          box-shadow: 0px -.2em 0 0 currentcolor;
        }
        87% {
          box-shadow: .2em -.2em 0 0 currentcolor;
        }
      }
   
		
	  "))
	),


  theme = theme,
  title = "BioPepper",
  
  navset_card_tab(
  nav_panel(title = "Pep_Search", 
		layout_sidebar(
		sidebar = sidebar(style="gap:0px",
		  width = 380,
		  h6(strong("Choose database"), ),
		  radioButtons( "choosedb", choices = c("Peptipedia", "NeuroPep_v2"), inline = T, label= NULL),
		  hr(),
		  h5(strong("Insert up to 2000 peptides")),
		  radioButtons("searchtype", choices = c("Hamming", "Grantham", "Exact match", "Smaller matching Peptides", "Larger containing peptides"), inline = T, label = "Search type" ),
		  checkboxInput("inclpred", label= "Include predicted", value = T),
		  br(),
		  textAreaInput("inppeps", "Insert petides", height="300px"),
		  p(style="padding:2px;"),
			  layout_columns(
				col_widths = c(4, 4, 4),
				actionButton("search", "Search", class="btn btn-success btn-sm btn-block"),
				actionButton("clear", "Clear", class="btn btn-primary btn-sm btn-block"),
				actionButton("upld", "Upload", class="btn btn-info btn-sm btn-block")
			  ),
		  p(style="padding:2px;"),
			  layout_columns(
				col_widths = c(12),
				actionButton("showbiodist", "Show distribution of Found peptides", class="btn btn-warning btn-sm btn-block"),
			  )
		),
		
		
		
		card(
		  style = "resize:vertical;",
		  id="filteringopts",
		  style="display:none;", 
		  card_header("Filtering options"),
		  card_body(
			layout_columns(
				col_widths = c(1, 4, 1, 4, 2),
				#all
				checkboxInput("onlyselbio", label= "Only selected", value = F),
				selectizeInput(inputId="biofunctionfilter",label = "Biofunctions", multiple = T, choices = NULL),
				p(""),
				sliderInput("pepsizefilter", "Peptide length", min = 2, max = 150, value = c(2,150), step = 1),
				checkboxInput("allowpred", label= "Allow predicted", value = T),
			),
			div(id="divboth",
				layout_columns(
					
					col_widths = c(4, 4, 4),
					# both
					sliderInput("match", "Miss interval", min = 0, max = 150, value = c(0,150), step = 1),
					#grantham
					sliderInput("distanceval", "Distance interval", min = 0, max = 100, value = c(0,100), step = 1),
					sliderInput("prcent_of_worst", "Percent_of_worst interval", min = 0, max = 1, value = c(0,1), step = 0.01),
					#hamming
					sliderInput("score", "Score interval", min = 0, max = 1, value = c(0,1), step = 0.01),
				)
			), 
			layout_columns(
					col_widths = c(4, 4, 4),
					actionButton("apply_filter", "Apply filter", class="btn btn-info btn-sm btn-block"),
					actionButton("clearfilter", "Clear filter", class="btn btn-primary btn-sm btn-block"),
					p("")
			)
		  )
		),
		
		card(
		  height = 730,
		  style = "resize:vertical;",
		  full_screen = TRUE,
		  card_header("Bioactive peptides", HTML("&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;&nbsp;"), checkboxInput("filtering", label= "Show filtering options", value = F)),
		  card_body(
			#div(
			DT::dataTableOutput("foundpeps")#,style = "overflow-x: auto;")#,# width: 95%;")
			#downloadButton("dld", "Download", class = "btn-danger")
		  )
		),
		card(
		  max_height = "80px",
		  layout_columns(
				col_widths = c(8, 4),
				downloadButton("dld", "Download", class = "btn-danger btn-block"),
				actionButton("clearselectedrows", "Clear selected rows", class="btn btn-block")
		  ),
		  
		),
		card(
		  id="pepdistfound", 
		  style="width=100%;height=400px;display:none;", 
		  card_header("Biofunction Distribution"),
			plotlyOutput("pepdistfoundplot")
		),
		card(
		  id="pepholder", 
		  style="width=100%;height=250px;",
		  card_header("Specific peptides - select up to six"),
		 #div(id="protholder", style="width=100%;height=400px;",
			plotlyOutput("clicked")
		)
	  )
  ),
  
  nav_panel(title = "Statistics", 
	
	layout_sidebar(
		sidebar = sidebar(
		  width = 300,
		  h6(strong("Choose database"), ),
		  radioButtons( "choosedbstat", choices = c("Peptipedia", "NeuroPep_v2"), inline = T, label= NULL),
		  hr(),
		  
		  h5(strong("See the data")),
			  layout_columns(
				col_widths = c(12),
				selectizeInput(inputId="biofunction",label = "Biofunction", multiple = F, choices = NULL), #allbionames
			  ),
			  layout_columns(
				col_widths = c(12),
				selectizeInput(inputId="pepsize",label = "Peptide length", multiple = F, choices = NULL) #peplen
			  ),
		),
		
		card(
		  style = "resize:vertical;",
		  full_screen = TRUE,
		  card_header("Plotly Figures"),
		  card_body(
			layout_columns(
			col_widths = c(4,8),
			plotlyOutput("lenvsbio"),
			plotlyOutput("biovslen")
			)
		  )
		),
	  )
	
	),
	
  nav_panel(title = "Cite", p("stuff")),
  
  nav_spacer(),
  
  nav_menu(
    title = "Links",
    nav_item(a("Peptipedia", href="https://app.peptipedia.cl/", target="_blank"))
  )
)
  

)
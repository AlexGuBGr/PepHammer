topnr <- 3


extend_grantham <- function(G) {
    

    #d(X,B) = (d(X,D)+d(X,N)) / 2
	#d(B,B) = (d(D,D)+d(D,N)+d(N,D)+d(N,N)) / 4
    #d(B,B) = d(D,N) / 2
	
	#d(X,Ai)=(1/20)*∑d(Aj,Ai), where j=1 and ∑^20

    
    add_binary <- function(mat, new_aa, aa1, aa2) {
        new_col <- (mat[, aa1] + mat[, aa2]) / 2
        self_dist <- mat[aa1, aa2] / 2
        mat <- cbind(mat, new_col)
        mat <- rbind(mat, c(new_col, self_dist))
        colnames(mat)[ncol(mat)] <- new_aa
        rownames(mat)[nrow(mat)] <- new_aa
        mat
    }
    
    # Add X
    x_col <- colMeans(G)
    x_self <- mean(G)
    G <- cbind(G, X = x_col)
    G <- rbind(G, X = c(x_col, x_self))
    
    G <- add_binary(G, "B", "D", "N")
    G <- add_binary(G, "Z", "E", "Q")
    G <- add_binary(G, "J", "I", "L")
    
    G
}


grantham_distance_matrix <- function() {
    
    mat <- matrix(c(
        0,110,145, 74, 58, 99,124, 56,142,155,144,112, 89, 68, 46,121, 65, 80,135,177,
        110,  0,102,103, 71,112, 96,125, 97, 97, 77,180, 29, 43, 86, 26, 96, 54, 91,101,
        145,102,  0, 98, 92, 96, 32,138,  5, 22, 36,198, 99,113,153,107,172,138, 15, 61,
        74,103, 98,  0, 38, 27, 68, 42, 95,114,110,169, 77, 76, 91,103,108, 93, 87,147,
        58, 71, 92, 38,  0, 58, 69, 59, 89,103, 92,149, 47, 42, 65, 78, 85, 65, 81,128,
        99,112, 96, 27, 58,  0, 64, 60, 94,113,112,195, 86, 91,111,106,126,107, 84,148,
        124, 96, 32, 68, 69, 64,  0,109, 29, 50, 55,192, 84, 96,133, 97,152,121, 21, 88,
        56,125,138, 42, 59, 60,109,  0,135,153,147,159, 98, 87, 80,127, 94, 98,127,184,
        142, 97,  5, 95, 89, 94, 29,135,  0, 21, 33,198, 94,109,149,102,168,134, 10, 61,
        155, 97, 22,114,103,113, 50,153, 21,  0, 22,205,100,116,158,102,177,140, 28, 40,
        144, 77, 36,110, 92,112, 55,147, 33, 22,  0,194, 83, 99,143, 85,160,122, 36, 37,
        112,180,198,169,149,195,192,159,198,205,194,  0,174,154,139,202,154,170,196,215,
        89, 29, 99, 77, 47, 86, 84, 98, 94,100, 83,174,  0, 24, 68, 32, 81, 40, 87,115,
        68, 43,113, 76, 42, 91, 96, 87,109,116, 99,154, 24,  0, 46, 53, 61, 29,101,130,
        46, 86,153, 91, 65,111,133, 80,149,158,143,139, 68, 46,  0, 94, 23, 42,142,174,
        121, 26,107,103, 78,106, 97,127,102,102, 85,202, 32, 53, 94,  0,101, 56, 95,110,
        65, 96,172,108, 85,126,152, 94,168,177,160,154, 81, 61, 23,101,  0, 45,160,181,
        80, 54,138, 93, 65,107,121, 98,134,140,122,170, 40, 29, 42, 56, 45,  0,126,152,
        135, 91, 15, 87, 81, 84, 21,127, 10, 28, 36,196, 87,101,142, 95,160,126,  0, 67,
        177,101, 61,147,128,148, 88,184, 61, 40, 37,215,115,130,174,110,181,152, 67,  0
    ), nrow = 20, byrow = TRUE)
    
    aa <- c( "S","R","L","P","T","A","V","G","I", "F","Y","C","H","Q","N","K","D","E","M","W")
    rownames(mat) <- aa
    colnames(mat) <- aa
    
    mat
}


best_matches_fast_grantham <- function(query_vec, nr, grantm, inclpred, dbpath, tabl) {
    
    query_vec <- query_vec[  !(grepl("[OU]", query_vec)) ]
    B <- stringr::str_split_fixed(query_vec, "", nchar(query_vec[[1]]) )
    A <- encode_matrix(dbpath, nr, aa_levels, gett = tabl, str_only = T, rmOU = T , preds=inclpred)
    
    
    N <- nrow(A)
    M <- nrow(B)
    L <- ncol(A)
    
	if (N == 0) {
		return( data.frame() )
	}
	
    best_index <- vector("list", M)
    names(best_index) <- query_vec
    best_score <- integer(M)
    names(best_score) <- query_vec
    
    for (j in seq_len(M)) {
        
        tmp <- B[j, ]
        scores <- integer(N)
        
        for (k in seq_len(L)) {
            scores <- scores + unname(grantm[ A[, k], tmp[k] ])
        }
        
        min_score <- min(scores)
        best_index[[ query_vec[[j]] ]] <- head( which(scores == min_score), topnr )
        best_score[[ query_vec[[j]] ]] <- min_score
    }
    #return(list(best_index, best_score))
    num_nr <- as.numeric(nr)
    outdf <- do.call(rbind, lapply(1:length(best_index), function(x) { data.frame(Query = names(best_index[x]), Found=best_index[[x]], Distance=best_score[[x]], Percent_of_worst = NA, Length=num_nr)}))
    outdf[["Found"]] <- apply( A[ outdf[["Found"]], ,drop = F], 1, paste0, collapse = "")
    
	outdf <- add_extra(outdf, nr)
	
    return(outdf)
    
}


add_extra <- function(df, nr) {
	num_nr <- as.numeric(nr)
	query_split <- stringr::str_split_fixed(df[["Query"]], "", num_nr)
	found_split <- stringr::str_split_fixed(df[["Found"]], "", num_nr)
	df[["Miss_count_exact"]] <- df[["Length"]] - rowSums(query_split  == found_split)
	worst <- vapply(
					seq_len(nrow(query_split)),
					function(i) {
						x <- query_split[i, ]
						sum(matrixStats::colMaxs(grantm[, x, drop = FALSE]))
					},
					numeric(1)
				)
	df[["Percent_of_worst"]] <- df[["Distance"]] / worst
	return(df)

}



best_matches_chunked <- function(A, B, chunk_size = 100000) {
    
    N <- nrow(A)
    M <- nrow(B)
    
    best_index <- vector('list', M)
    best_score <- rep(-1L, M)
    
    # Iterate over chunks of A
    for (start in seq(1, N, by = chunk_size)) {
        
        end <- min(start + chunk_size - 1L, N)
        A_chunk <- A[start:end, , drop = FALSE]
        
        # For each query peptide
        for (j in seq_len(M)) {
            
            scores <- rowSums(A_chunk == matrix(B[j, ],
                                                nrow = nrow(A_chunk),
                                                ncol = ncol(A_chunk),
                                                byrow = TRUE))
            chunk_max <- max(scores)
            
            # Case 1: new better maximum
            if (chunk_max > best_score[j]) {
                
                idx_local <- which(scores == chunk_max)
                
                best_score[j] <- chunk_max
                best_index[[j]] <- idx_local + start - 1L
                
                # Case 2: equal maximum → append
            } else if (chunk_max == best_score[j]) {
                
                idx_local <- which(scores == chunk_max)
                
                best_index[[j]] <- c(
                    best_index[[j]],
                    idx_local + start - 1L
                )
            }
        }
    }
    
    list(best_index = best_index, best_score = best_score)
}








encode_matrix <- function(dbpath, mat, aa_levels, gett=F, str_only = F, rmOU = F, preds=T) {

	if ( class(gett) == "character" ) {
		if ( rmOU == T ) {
			if (preds == T) {
				mat <- stringr::str_split_fixed( load_columns_from_table_where_NOT_OU("peptide", gett, as.numeric(mat),dbpath = dbpath)[["peptide"]], "", as.numeric(mat) )
			} else {
				mat <- stringr::str_split_fixed( load_columns_from_table_where_NOT_OU_and_not_pred("peptide", gett, as.numeric(mat), dbpath = dbpath)[["peptide"]], "", as.numeric(mat) )
			}
		} else {
			if (preds == T) { # cols, tablename, col2, cond,
				mat <- stringr::str_split_fixed( load_columns_from_table_where("peptide", gett, "Length", as.numeric(mat), dbpath = dbpath )[["peptide"]], "", as.numeric(mat) )
			} else {
				mat <- stringr::str_split_fixed( load_columns_from_table_where_not_pred("peptide", gett, as.numeric(mat), dbpath = dbpath )[["peptide"]], "", as.numeric(mat) )
			}
		}
		if ( str_only == T ) {
			return(mat)
		}
	}
	# the actual encoding
    matrix(match(mat, aa_levels),nrow = nrow(mat))
}


decode_matrix <- function(encoded_mat, aa_levels) {
  matrix(
    aa_levels[encoded_mat],
    nrow = nrow(encoded_mat)
  )
}



build_compat_matrix <- function(aa_levels) {
    
    K <- length(aa_levels)
    compat <- matrix(FALSE, K, K)
    
    diag(compat) <- TRUE
    
    idx <- setNames(seq_along(aa_levels), aa_levels)
    
    # B = D or N
    if ("B" %in% aa_levels) {
        b <- idx["B"]
        d <- idx["D"]
        n <- idx["N"]
        compat[b, c(d,n)] <- TRUE
        compat[c(d,n), b] <- TRUE
    }
    
    # Z = E or Q
    if ("Z" %in% aa_levels) {
        z <- idx["Z"]
        e <- idx["E"]
        q <- idx["Q"]
        compat[z, c(e,q)] <- TRUE
        compat[c(e,q), z] <- TRUE
    }
    
    # J = I or L
    if ("J" %in% aa_levels) {
        j <- idx["J"]
        i <- idx["I"]
        l <- idx["L"]
        compat[j, c(i,l)] <- TRUE
        compat[c(i,l), j] <- TRUE
    }
    # for X
    x <- which(aa_levels == "X")
    compat[x,] <- compat[,x] <- T
    
    compat
}


# it need to be known: db and table name



best_matches_fast <- function(query_vec, nr, compat, aa_levels, inclpred, dbpath, tabl) {
    
	B <- encode_matrix(dbpath, stringr::str_split_fixed(query_vec, "", nchar(query_vec[[1]]) ), aa_levels )
	A <- encode_matrix(dbpath, nr, aa_levels, gett = tabl, preds=inclpred)


    N <- nrow(A)
    M <- nrow(B)
    L <- ncol(A)

	if (N == 0) {
		return( data.frame() )
	}
	
    best_index <- vector("list", M)
	names(best_index) <- query_vec
    best_score <- integer(M)
	names(best_score) <- query_vec

    for (j in seq_len(M)) {
        
        tmp <- B[j, ]
        
        scores <- integer(N)
        
        for (k in seq_len(L)) {
            scores <- scores + compat[A[, k], tmp[k]]
        }
        
        max_score <- max(scores)
        best_index[[ query_vec[[j]] ]] <- head( which(scores == max_score), topnr )
        best_score[[ query_vec[[j]] ]] <- max_score
    }
    
	num_nr <- as.numeric(nr)
	outdf <- do.call(rbind, lapply(1:length(best_index), function(x) { data.frame(Query = names(best_index[x]), Found=best_index[[x]], Score=best_score[[x]]/num_nr,  Miss_count = num_nr - best_score[[x]], Length=num_nr)}))
	outdf[["Found"]] <- apply( decode_matrix(A[ outdf[["Found"]], ,drop = F], aa_levels), 1, paste0, collapse = "")

	return(outdf)

}


exapnd_to_table <- function(df, allbionames_pep) {
    bioa <- stringr::str_split( df[["bioactivities"]], ";")
    #unq <- unique(unlist(bioa))
    #allbion <- allbionames_pep[ allbionames_pep %in% unq ]
    df1 <- do.call(rbind, lapply(bioa, function(x){ (allbionames_pep %in% x) + 0 }))
    colnames(df1) <- allbionames_pep
    cbind( df[ setdiff(colnames(df), "bioactivities")], df1 )
}


get_bio_data <- function(df, dbpath, dfname, bionames, col="Found", colname="peptide") {

	if (!is.null(df) && nrow(df) > 0 ) {
	
		if (grepl("peptipedia", dfname)) {
			dftmp <- exapnd_to_table( load_columns_from_table_where("*", dfname, colname, df[[col]], dbpath), bionames )
		} else {
			dftmp <- load_columns_from_table_where("*", dfname, colname, df[[col]], dbpath)
		}
		
		dftmp <- dftmp[colnames(dftmp) != "Length"]
		return( merge(df, dftmp, by.x = "Found", by.y = "peptide") )
	} else {
		return(NULL)
	}
}



go_throug_lst <- function(lst, compat, aa_levels, allbionames, searchtype, grantm, inclpred, dbpath, tabl) {

	final <- list() #vector("list", length(lst))
	iter <- 1
	total_steps <- length(lst)
	if (searchtype == "Hamming") {
		
		for ( i in names(lst) ) {
			final[[iter]] <- best_matches_fast(lst[[i]], i, compat, aa_levels, inclpred, dbpath, tabl)
			#final[[iter]] <- get_bio_data(final[[iter]], dbpath, tabl, allbionames)
			iter <- iter + 1
		}
	} else if (searchtype == "Grantham") {
		#print(searchtype)
		for ( i in names(lst) ) {
			final[[iter]] <- best_matches_fast_grantham(lst[[i]], i, grantm, inclpred, dbpath, tabl)
			#final[[iter]] <- add_extra(final[[iter]], i)
			#final[[iter]] <- get_bio_data(final[[iter]], dbpath, tabl, allbionames)
			iter <- iter + 1
		}
	} else if (searchtype == "Exact match") { 
		#print(searchtype)
		final[[iter]] <- find_exact_matches( unname(unlist(lst)), "peptide", tabl, "peptide", dbpath, batch_size = 500, preds=inclpred)
		#for ( i in names(lst) ) {
		#	final[[iter]] <- find_exact_matches( lst[[i]], "peptide", i, "peptide", dbpath, tabl, batch_size = 500, preds=inclpred)
			#if (length(final[[iter]]) > 0) {
			#	final[[iter]] <- get_bio_data(final[[iter]], dbpath, tabl, allbionames)
			#}
		#	iter <- iter + 1
		#}	
	} else if (searchtype == "Smaller matching Peptides") { 
		#print(searchtype)
		for ( i in names(lst) ) {
			if ( as.numeric(i) > 2 ) {
				tmpi <- as.character(max(c(as.numeric(i)-1 , 2)))
				final[[iter]] <- find_smaller_matching_peps( lst[[i]], "peptide", tabl, tmpi, "peptide", dbpath, batch_size = 500, preds=inclpred)
				#if (length(final[[iter]]) > 0) {
				#	final[[iter]] <- get_bio_data(final[[iter]], dbpath, tabl, allbionames)
				#}
				iter <- iter + 1
			}
		}
	} else if (searchtype == "Larger containing peptides") {
		#print(searchtype)
		for ( i in names(lst) ) {
			if ( as.numeric(i) < 150 ) {
				tmpi <- as.character(as.numeric(i)+1)
				final[[iter]] <- find_larger_containing_peptides(lst[[i]], "peptide", tabl, tmpi, "peptide", dbpath, batch_size = 500, preds=inclpred)
				#if (length(final[[iter]]) > 0) {
				#	final[[iter]] <- get_bio_data(final[[iter]], dbpath, tmpi, allbionames)
				#}
				iter <- iter + 1
			}
		}
	}
	
	final <- do.call(rbind, final) 
	
	if ( is.null(final) || nrow(final) == 0 ) {
		print("is null")
		return(final) 
	}
	
	final <- get_bio_data(final, dbpath, tabl, allbionames)
	
	clnams <- colnames(final)
	if ( !("Length" %in% clnams) ) {
		final[["Length_found"]] <- nchar(final[["Found"]])
		clnams <- c( clnams, "Length_found" )
	}
	
	
	#if (grepl("biofuncs", dbpath)) {
	final <- cbind(final[ c( "Query", clnams[!(clnams %in% c( allbionames, "Query" ))]) ], round( final[allbionames][ colSums(final[allbionames]) > 0 ], 2 ) )
	#}
	final[["Proteins"]] <- sapply(final[["Proteins"]], function(i) { paste( head(stringr::str_split_1(i, ";"), 5 ), collapse = ";" ) }, USE.NAMES = F )
	return(final)
}



update_filtering_options <- function(session, typo, dat, allbionames, predscore, dat_init) {

	clnames <- colnames(dat)
	updateSelectizeInput(session, "biofunctionfilter", choices = allbionames[allbionames %in% clnames])
	lncol <- "Length"
	if ("Length_found" %in% clnames) {
		lncol <- "Length_found"
	}
	ma <- max(dat[[lncol]])
	mi <- min(dat[[lncol]])
	updateSliderInput(session, "pepsizefilter", value=c(mi,ma), min=mi,max=ma, step=1)
	
	updateCheckboxInput(session, "onlyselbio", value=F)
	updateCheckboxInput(session, "allowpred", value=T)
	updateCheckboxInput(session, "onlytis", value=F)
	
	if (typo == "Grantham" ) {
		ma <- max(dat[["Miss_count_exact"]])
		mi <- min(dat[["Miss_count_exact"]])
		updateSliderInput(session, "match", value=c(mi,ma), min=mi,max=ma, step=1)
		ma <- max(dat[["Distance"]])
		mi <- min(dat[["Distance"]])
		updateSliderInput(session, "distanceval", value=c(mi,ma), min=mi,max=ma, step=1)
		ma <- max(dat[["Percent_of_worst"]])
		mi <- min(dat[["Percent_of_worst"]])
		ma <- ceiling(ma * 100) / 100
		mi <- floor(mi * 100) / 100
		updateSliderInput(session, "prcent_of_worst", value=c(mi,ma), min=mi,max=ma, step=0.01)
		
	} else if (typo == "Hamming") {
		ma <- max(dat[["Miss_count"]])
		mi <- min(dat[["Miss_count"]])
		updateSliderInput(session, "match", value=c(mi,ma), min=mi,max=ma, step=1)
		ma <- max(dat[["Score"]])
		mi <- min(dat[["Score"]])
		ma <- ceiling(ma * 100) / 100
		mi <- floor(mi * 100) / 100
		updateSliderInput(session, "score", value=c(mi,ma), min=mi,max=ma, step=0.01)
	}
	
	if ( is.null(dat_init) ) {
		updateSliderInput(session, "predscore", value=c(0,1), min=0, max=1, step=0.01)
	} else {
		updateSliderInput(session, "predscore", value=c(predscore[[1]],predscore[[2]]), min=predscore[[1]], max=predscore[[2]], step=0.01)
	}
	
}


filtering_function <- function(dat, typo, allbionames, input, dbpath) {
	
	clnames <- colnames(dat)
	allbionames <- allbionames[allbionames %in% clnames]
	othernames <- clnames[ !(clnames %in% allbionames) ]
	
	if (input$allowpred == F) {
		dat <- dat[ !grepl( "predicted", dat[["ID"]]), ]
	}
	# length:
	lncol <- "Length"
	if ("Length_found" %in% clnames) {
		lncol <- "Length_found"
	}
	dat <- dat[ dat[[lncol]] >= input$pepsizefilter[[1]] & dat[[lncol]] <= input$pepsizefilter[[2]], ]
	
	# Grantham & Hamming:
	if (typo == "Grantham" ) {
		dat <- dat[ dat[["Distance"]] >= input$distanceval[[1]] & dat[["Distance"]] <= input$distanceval[[2]], ]
		dat <- dat[ dat[["Percent_of_worst"]] >= input$prcent_of_worst[[1]] & dat[["Percent_of_worst"]] <= input$prcent_of_worst[[2]], ]
		dat <- dat[ dat[["Miss_count_exact"]] >= input$match[[1]] & dat[["Miss_count_exact"]] <= input$match[[2]], ]
		
	} else if (typo == "Hamming") {
		dat <- dat[ dat[["Miss_count"]] >= input$match[[1]] & dat[["Miss_count"]] <= input$match[[2]], ]
		dat <- dat[ dat[["Score"]] >= input$score[[1]] & dat[["Score"]] <= input$score[[2]], ]
	}
	
	if ( grepl("multipep", dbpath) ) { 
		tmp_bio <- allbionames[!grepl("_PXD", allbionames)]
		if ( length(tmp_bio) > 0 ) {
			dat <- dat[ rowSums(dat[tmp_bio] >= input$predscore[[1]] & dat[tmp_bio] <= input$predscore[[2]]) > 0, ]
			#dat <- dat[ matrixStats::rowProds(dat[tmp_bio] > input$predscore[[1]] & dat[tmp_bio] <= input$predscore[[2]]) > 0, ]
		}
	}
	
	if (input$onlytis == T) {
		tmp_bio <- allbionames[grepl("_PXD", allbionames)]
		if ( length(tmp_bio) > 0 ) {
			allbionames <- tmp_bio
			cbind( dat[othernames], dat[allbionames])
		}
	}
	
	# biofunctions:
	if ( !is.null( input$biofunctionfilter ) & input$onlyselbio == T ) {
		dat <- cbind( dat[othernames], dat[input$biofunctionfilter])
		dat <- dat[ rowSums(dat[input$biofunctionfilter]) > 0, ]
		
	} else if ( !is.null( input$biofunctionfilter ) & input$onlyselbio == F ) {
		dat <- dat[ rowSums(dat[input$biofunctionfilter]) > 0, ]
		dat <- cbind( dat[othernames], dat[allbionames][colSums(dat[allbionames]) > 0])
		
	} else if ( is.null( input$biofunctionfilter ) ) {
		dat <- cbind( dat[othernames], dat[allbionames][colSums(dat[allbionames]) > 0])
	}
	
	return( dat )

}





check_length <- function(strings, dbpath) {
    nc <- nchar(strings)
	
    cx <- stringr::str_count(strings, "X")
    strings[ (nc > 1 & nc < 151) & (cx / nc) <= 0.2]
}

return_message <- function(strings) {

	ln <- length(strings)
	if ( ln == 0 ) {
		showNotification("Invalid peptides - try again", duration = 6, type = "error")
		removeModal()
		req(F)
	}
}


get_data <- function(starter, dat, strings, compat, aa_levels, allbionames, searchtype, grantm, inclpred, dbpath, tabl ) {


	showModal(modalDialog(
			title = "Analyzing...",
			size = "m",
			HTML("<strong id='tmploader'>Loading...</strong><br>
				<div style='padding-left:47%;'>
                  <div class='loader'></div>
                </div>"),
			easyClose = F,
			footer = NULL
	))

	starter(  min( c( max( c(stringr::str_count(strings, "\n")/10, 6) ), 300 ) ) )
	future_promise({
      source("R/dbfunctions.R")
      source("R/functions.R")
	  	
		strings <- stringr::str_split_1(strings, "\n")
		if ( length(strings) > 10000) {return(NULL)}
		strings <- strings[duplicated(strings) == F]
		#return_message(strings)
		if ( length(strings) == 0) {return(NULL)}
		strings <- check_length(strings, dbpath)
		#return_message(strings)
		if ( length(strings) == 0) {return(NULL)}
		strings <- strings[sapply(strings, function(x){ all(stringr::str_split_fixed(x, "", nchar(x)) %in% aa_levels) }, USE.NAMES = F)]
		#return_message(strings)
		if ( length(strings) == 0) {return(NULL)}
		
		if (length(strings) > 0) {
			##strings <- sapply(strings, function(x){ tmp <- stringr::str_split_fixed(x, "", nchar(x)); tmp[!(tmp %in% aa_levels)] <- "_"; paste(tmp,collapse = "") }, USE.NAMES = F)
			strings <- split(strings, nchar(strings))
			return( go_throug_lst(strings, compat, aa_levels, allbionames, searchtype, grantm, inclpred, dbpath, tabl))
		} else {
			return(NULL)
		}
	  
      },seed = T, 
		globals = list(strings=strings, compat=compat, 
						aa_levels=aa_levels, allbionames=allbionames, 
						searchtype=searchtype, grantm=grantm,
						inclpred=inclpred, dbpath=dbpath, tabl=tabl)
	  
	  ) %...>% (function(result) {
		shinyjs::runjs('document.getElementById("tmploader").innerHTML = "==========================================="')
		removeModal()
		starter(NULL)
		if (is.null(result)) {
			showNotification("No valid peptides.", duration = 6, type = "warning" )
		}
		dat(result)
		
    }) %...!% (function(err) {
		removeModal()
		starter(NULL)
		print(err)
        showNotification("Data could no be processed - Try again!", 
                       duration = 6, type = "error"
      )
      NULL    
    })




}


make_length_vs_biofunction <- function(biof, dbname, stattablfig, thr) {
    
	biox <- load_entire_table(stattablfig, dbname)
	req(biox)
	if ( "pred" %in% colnames(biox) && length(unique(biox[["pred"]])) > 1) {
			return( make_overlay_bar_plot_length(biox, biof) )
	}
	
	if (biof == "All") {
	
		biox <- cbind( biox["Pep_length"], "All" = rowSums(biox[ setdiff( colnames(biox), c("pred", "Pep_length") ) ] ))
		biof <- "All"
		
	} else {
		biox <- biox[c("Pep_length", biof)]
		#biox <- load_columns_from_table(c("Pep_length", biof), stattablfig, dbname)
	}

    biox[["Pep_length"]] <- factor(biox[["Pep_length"]], levels = biox[["Pep_length"]])
    fig <- plot_ly(x = biox[["Pep_length"]], y = biox[[biof]], type = 'bar', #text = text,
                   marker = list(color = "#487A46"))
    fig <- fig %>% layout(title = paste0("Lengths distribution for ", biof, "\n", "Total bioactivities: (",sum(biox[[biof]]), ")"),
                          xaxis = list(title = "Peptide length"),
                          yaxis = list(title = "Count"),
						  margin = list(l = 5, r = 5, b = 5, t = 60)
    )
    
    fig <- fig %>% plotly::config(toImageButtonOptions = list(format= 'svg', # one of png, svg, jpeg, webp
                                                 filename= 'len_vs_bio',
                                                 height= NULL,#400, # = NULL to download img as is
                                                 width= NULL,#600,  # = NULL to download img as is
                                                 scale= 1 ),
                     displaylogo = FALSE,
                     modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"))
	fig
}

make_overlay_bar_plot_length <- function(df, len) {

    unq_preds <- unique(df[["pred"]])
    cats <- setdiff( colnames(df), c("pred", "Pep_length") )
    lens <- df[["Pep_length"]][df[["pred"]] == unq_preds[[1]] ]
    lens <- factor(lens, levels = lens)
    if (len == "All") {
        scs <- lapply(unq_preds, function(x) { unname(rowSums(df[df[["pred"]] == x, ][cats])) })
    } else {
        scs <- lapply(unq_preds, function(x) { unname(unlist(df[df[["pred"]] == x, ][len])) })
    }
    
    opa <- seq(0.4, 1, length.out = length(unq_preds))
    allbio <- 0
    fig <- plot_ly()
    for ( i in 1:length(unq_preds) ) {
        fig <- add_bars(fig, x = lens, y = scs[[i]],  name = unq_preds[[i]],  opacity = opa[[i]]) 
        allbio <- allbio + sum(scs[[i]])
    }
    fig <- layout(fig, barmode = "overlay", title = paste0("Lengths distribution for ", len, "\n", "Total bioactivities: (",sum(allbio), ")"),
                          xaxis = list(title = "Peptide length"),
                          yaxis = list(title = "Count"),
                          margin = list(l = 5, r = 5, b = 5, t = 60))
    fig <- plotly::config(fig, toImageButtonOptions = list(format= 'svg', # one of png, svg, jpeg, webp
                                                                            filename= 'len_vs_bio',
                                                                            height= NULL,#400, # = NULL to download img as is
                                                                            width= NULL,#600,  # = NULL to download img as is
                                                                            scale= 1 ),
                                                displaylogo = FALSE,
                                                modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"                           ))
    fig
}





make_biofunction_vs_length <- function(len, allbionames, dbname, tabl, stattablfig, table_lengths, tisorpred, tissues) {

	biox <- load_entire_table(stattablfig, dbname)	
	if (tisorpred == "Tissue only" && all(tissues %in% allbionames) ) {
		allbionames <- tissues
	} else if (tisorpred == "Pred only") {
		allbionames <- setdiff( allbionames, tissues )
	} 

	if ( "pred" %in% colnames(biox) && length(unique(biox[["pred"]])) > 1) {
		if (len == "All") {
			cnt <- table_lengths[[tabl]]
		} else {
			cnt <- get_countdb(dbname, tabl, len)[[1]]
		}
		return( make_overlay_bar_plot_biolength(biox, len, allbionames, cnt) )
	}

	if (len == "All") {
		biox <- biox[ setdiff( colnames(biox), "Pep_length" ) ]
		dat <- unname( colSums(biox[allbionames]) )
		cnt <- table_lengths[[tabl]]
	} else {
		biox <- biox[ biox[["Pep_length"]] == len, ]
		cnt <- get_countdb(dbname, tabl, len)[[1]]
		dat <- unname(unlist(biox[allbionames][1,]))
	}

	datbool <- dat > 0
	usex <- allbionames[datbool]
	usex <- factor(usex, levels = usex)
    fig <- plot_ly(x = usex, y = dat[datbool], type = 'bar',
                   marker = list(color = "#41608A"))
    fig <- fig %>% layout(title = paste0("Bioactivity distribution and peptide length '", len,"'", "\nAll bioactivies: ",sum(dat), " and peptides: ", cnt ), 
                          xaxis = list(title = "Bioactivities"
						  ),
                          yaxis = list(title = "Count"),
						  margin = list(l = 5, r = 5, b = 5, t = 60)

    )
    fig <- fig %>% plotly::config(toImageButtonOptions = list(format= 'svg', # one of png, svg, jpeg, webp
                                                 filename= 'bio_vs_len',
                                                 height= NULL,#400, # = NULL to download img as is
                                                 width= NULL,#600,  # = NULL to download img as is
                                                 scale= 1 ),
                     displaylogo = FALSE,
                     modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"))
    fig
}


make_overlay_bar_plot_biolength <- function(df, len, cats, cnt) {
    
    unq_preds <- unique(df[["pred"]])
    
    if (len == "All") {
        scs <- lapply(unq_preds, function(x) { unname(colSums(df[df[["pred"]] == x, ][cats])) })
    } else {
        scs <- lapply(unq_preds, function(x) { unname(unlist(df[df[["Pep_length"]] == len & df[["pred"]] == x, ][cats])) })
    }
    
	
	datbool <- scs[[1]] > 0
	cats <- cats[datbool]
	cats <- factor(cats, levels = cats)
    
    opa <- seq(0.4, 1, length.out = length(unq_preds))
    allbio <- 0
    
    fig <- plot_ly()
    for ( i in 1:length(unq_preds) ) {
        
        fig <- add_bars(fig, x = cats, y = scs[[i]][datbool],  name = unq_preds[[i]],  opacity = opa[[i]]) 
        allbio <- allbio + sum(scs[[i]])
    }
    fig <- fig %>% layout(barmode = "overlay", title = paste0("Bioactivity distribution and peptide length '", len,"'", "\nAll bioactivies: ",allbio, " and peptides: ", cnt ), 
                          xaxis = list(title = "Bioactivities"),
                          yaxis = list(title = "Count"),
                          margin = list(l = 5, r = 5, b = 5, t = 60))
    fig <- plotly::config(fig, toImageButtonOptions = list(format= 'svg', # one of png, svg, jpeg, webp
                                                                            filename= 'len_vs_bio',
                                                                            height= NULL,#400, # = NULL to download img as is
                                                                            width= NULL,#600,  # = NULL to download img as is
                                                                            scale= 1 ),
                                                displaylogo = FALSE,
                                                modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"                           ))
    fig
}



make_overlay_bar_bio_vs_bio <- function(dbpath, tablename, biof, tissues, tisorpred) {
    
    df <- load_entire_table(tablename, dbpath)
    cats <- setdiff( colnames(df), c("pred", "rownames") )
    
    if (tisorpred == "Tissue only" && all(tissues %in% cats) ) {
        cats <- tissues
    } else if (tisorpred == "Pred only") {
        cats <- setdiff( cats, tissues )
    } 
    
    if ("pred" %in% colnames(df) && length( unique(df[["pred"]]) ) > 1 ) {
        unq_preds <- unique(df[["pred"]])
		cnt <- df[df[["rownames"]] == biof & df[["pred"]] == unq_preds[[1]], ][[biof]]
		cats <- setdiff( cats, biof )
        scs <- lapply(unq_preds, function(x) { unname(unlist(df[df[["rownames"]] == biof & df[["pred"]] == x, ][cats])) })
    } else {
		cnt <- df[ df[["rownames"]] == biof, ][[biof]]
		cats <- setdiff( cats, biof )
        scs <- list( unname(unlist(df[ df[["rownames"]] == biof, ][cats])) )
        unq_preds <- c(">0.5")
    }
    
	
	datbool <- scs[[1]] > 0
	cats <- cats[datbool]
	cats <- factor(cats, levels = cats)
    
	if (length(unq_preds) == 1) {
		opa <- 1
	} else {
		opa <- seq(0.4, 1, length.out = length(unq_preds))
	}
    
	if ( sum(datbool) == 0 ) {
		cats <- biof
		scs <- list( cnt )
		datbool <- T
	}
	
    allbio <- 0
    req(cats)
	req(scs[[1]])
    fig <- plot_ly()
    for ( i in 1:length(unq_preds) ) {
        
        fig <- add_bars(fig, x = cats, y = scs[[i]][datbool],  name = unq_preds[[i]],  opacity = opa[[i]]) 
        allbio <- allbio + sum(scs[[i]])
    }
    fig <- fig %>% layout(barmode = "overlay", title = paste0("Bioactivity distribution across '", biof,"' (", "peptides with ",unq_preds[[1]],": ", cnt , ")\nAll bioactivies: ",allbio ), 
                          xaxis = list(title = "Bioactivities"),
                          yaxis = list(title = "Count"),
                          margin = list(l = 5, r = 5, b = 5, t = 60))
    fig <- plotly::config(fig, toImageButtonOptions = list(format= 'svg', # one of png, svg, jpeg, webp
                                                                            filename= 'bio_vs_bio',
                                                                            height= NULL,#400, # = NULL to download img as is
                                                                            width= NULL,#600,  # = NULL to download img as is
                                                                            scale= 1 ),
                                                displaylogo = FALSE,
                                                modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"                           ))
    fig
}











make_found_biofunction_dist <- function(dat, allbionames, dbname, predscore) {

	nr <- nrow(dat)
    allbionames <- allbionames[ allbionames %in% colnames(dat)]
	if ( grepl("multipep", dbname) ) {
		dat <- colSums( dat[allbionames] >= max(c( 0.5, predscore[[1]] )) & dat[allbionames] <= predscore[[2]]) 
		allbionames <- names(dat)[dat > 0 ]
		dat <- unname( dat[dat > 0] )
	} else {
		dat <- unname(colSums(dat[allbionames]))
	}
	
	allbionames <- factor(allbionames, levels = allbionames)
    fig <- plot_ly(x = allbionames, y = dat, type = 'bar',
                   marker = list(color = "#339988"))
    fig <- fig %>% layout(title = paste0("Bioactivity distribution of 'Found' peptides", "\nAll bioactivies: ",sum(dat), " and 'Found' peptides: ", nr ), 
                          xaxis = list(title = "Bioactivities"
						  ),
                          yaxis = list(title = "Count"),
						  margin = list(l = 5, r = 5, b = 5, t = 60)

    )
    fig <- fig %>% plotly::config(toImageButtonOptions = list(format= 'svg', # one of png, svg, jpeg, webp
                                                 filename= 'bio_dist_found',
                                                 height= NULL,#400, # = NULL to download img as is
                                                 width= NULL,#600,  # = NULL to download img as is
                                                 scale= 1 ),
                     displaylogo = FALSE,
                     modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"))
    fig
}



bioactivity_of_single <- function(dfrow, allbionames, pep, dbname, predscore, subb) {


	if ( grepl("multipep", dbname) ) {
		datbool <- dfrow >= max(c( 0.5, predscore[[1]] )) & dfrow <= predscore[[2]]
		dfrow[datbool] <- 1
	} else {
		datbool <- dfrow > 0
	}

    
    usex <- allbionames[datbool]
	usex <- factor(usex, levels = usex)
    fig <- plot_ly(x = usex, y = dfrow[datbool], type = 'bar',
                   marker = list(color = "#4C78A8", 
                                 line = list(color = '#000000', width = 1.5)),
                   textposition = "inside",
                   text = allbionames[datbool]
    )
    fig <- fig %>% layout(
        #title = pep, 
        showlegend=FALSE,
        margin = list(l = 0, r = 0, b = 5, t = 20),
        annotations = list(
            list(
                text = pep,
                x = 0.5,
				y = 1.10-subb,
				xref = "paper",
				yref = "paper",
                showarrow = FALSE,
                xanchor = "center"
            )
        ),
        xaxis = list(title = pep, showticklabels = FALSE),#, side = "top"),
        yaxis = list(title = "Count", showticklabels = FALSE) 
        
    )
    fig
}


bioactivity_of_rows <- function(df, allbionames, dbname, predscore) {

	allbionames <- allbionames[ allbionames %in% colnames(df)]
	nrw <- nrow(df)
	nr <- ceiling(nrw/2)
	addjust <- c(0, 0, 0.05, 0.05, 0.1, 0.1)
	
	csize <- x <- ifelse(nr == 1, 300, 210*nr+70)
	shinyjs::runjs(paste0("document.getElementById('pepholder').style.height = '", csize ,"px';"))
	shinyjs::runjs(paste0("document.getElementById('pepholder2').style.height = '",205 * nr ,"px';"))
	shinyjs::runjs(paste0("document.getElementById('clicked').style.height = '",200 * nr ,"px';"))
	
	subplot( lapply( 1:nrw, function(x) {bioactivity_of_single(unname(unlist(df[x,][allbionames])), allbionames, df[["Found"]][[x]], dbname, predscore, addjust[[x]] ) } ) ,
           nrows = nr, 
           titleX = F,
           titleY = F,
           margin = c(0.035,0.035,0.05,0.05)# * 1/nr
           ) %>% plotly::config(toImageButtonOptions = list(format= 'svg', # one of png, svg, jpeg, webp
                                               filename= 'pepbiofigs',
                                               height= 200*nr, # = NULL to download img as is
                                               #width= W,  # = NULL to download img as is
                                               scale= 1 ),
                   displaylogo = FALSE,
                   modeBarButtonsToRemove = c( "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"))

}




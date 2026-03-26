

get_row_count <- function(table, dbpath) {
    
    con <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
    
    stopifnot(DBI::dbIsValid(con))
    stopifnot(is.character(table), length(table) == 1)
    
    tryCatch({
        table_quoted <- DBI::dbQuoteIdentifier(con, table)
        query <- paste0(
            "SELECT COUNT(*) AS n FROM ",
            table_quoted
        )
        res <- DBI::dbGetQuery(con, query)
        res
        #res$n[[1]]
    }, error = function(e) {
        print(e)
    }, finally = DBI::dbDisconnect(con)
    )
}



get_column_sums <- function(table, columns, dbpath) {
    
    con <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
    set_busytimeout(con, time=10000)
    
    stopifnot(DBI::dbIsValid(con))
    stopifnot(is.character(table), length(table) == 1)
    stopifnot(is.character(columns), length(columns) > 0)
    
    table_quoted <- DBI::dbQuoteIdentifier(con, table)
    cols_quoted  <- DBI::dbQuoteIdentifier(con, columns)
    
    # Build SUM(col) AS col
    sum_expr <- paste0(
        "SUM(", cols_quoted, ") AS ", cols_quoted,
        collapse = ", "
    )
    
    query <- paste0(
        "SELECT ", sum_expr,
        " FROM ", table_quoted
    )
    
    res <- DBI::dbGetQuery(con, query)
    return(res)
    # Return named numeric vector
    return(as.numeric(res[1, ]))
}


add_or_replace_table_in_db <- function(table, nameid, dbpath) {
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  
  set_busytimeout(conn, time=10000)
  DBI::dbBegin(conn)
  
  tryCatch({
    DBI::dbWriteTable(conn, nameid, table, overwrite = T)
    DBI::dbCommit(conn)
    
    }, error = function(e) {
      DBI::dbRollback(conn)
      
    }, finally = DBI::dbDisconnect(conn)
  )
  #DBI::dbWriteTable(conn, nameid, table, overwrite = T)
  
  #DBI::dbDisconnect(conn)
}

remove_section <- function(fil, typo, dbpath) {
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  
  set_busytimeout(conn, time=10000)
  DBI::dbBegin(conn)
  
  tryCatch({
    #print(paste0("DELETE FROM ", fil, " WHERE termtype = '",typo,"'"))
    DBI::dbExecute(conn, paste0("DELETE FROM '", fil, "' WHERE `termtype` = '",typo,"'"))
    DBI::dbCommit(conn)
    #print("SUCCESS")
  }, error = function(e) {
    DBI::dbRollback(conn)
    print("NO!")
    
  }, finally = DBI::dbDisconnect(conn)
  )
}


append_table <- function(existing_table_name, new_table, dbpath ) {
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  
  set_busytimeout(conn, time=10000)
  DBI::dbBegin(conn)
  
  tryCatch({
    DBI::dbAppendTable(conn, existing_table_name, new_table)
    DBI::dbCommit(conn)
    
  }, error = function(e) {
    DBI::dbRollback(conn)
    
  }, finally = DBI::dbDisconnect(conn)
  )
}



list_tables <- function(dbpath) {
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  
  set_busytimeout(conn, time=10000)
  
  tables <- tryCatch({
    DBI::dbListTables(conn)
    
  }, error = function(e) {
    return(NULL)
    
  }, finally = DBI::dbDisconnect(conn))
  
  return(tables)
}


change_list_columns <- function(table, dbpath) {
  
  vec <- purrr::map_lgl(table, is.list)
  
  table[vec][is.na(table[vec])] <- "-"
  
  table[ , vec] <- apply(table[ , vec, drop=F], 2,           
                      function(x) as.numeric(as.character(x)))
  return(table)
}


load_entire_table <- function(tablename, dbpath) { 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  
  set_busytimeout(conn, time=10000)
  
  df <- tryCatch({
    DBI::dbGetQuery(conn, paste0("SELECT * FROM ", "'",tablename,"'"))
    
  }, error = function(e) {
    return(NULL)
    
  }, finally = DBI::dbDisconnect(conn))
  
  return(df)
}


load_columns_from_table <- function(cols, tablename, dbpath) { 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  set_busytimeout(conn, time=10000)
  
  df <- tryCatch({
    DBI::dbGetQuery(conn, paste0("SELECT ", paste(paste0(paste0("`",cols),"`"), collapse = ","), " FROM ", "'",tablename,"'"))
    
  }, error = function(e) {
	print(e)
    return(NULL)
    
  }, finally = DBI::dbDisconnect(conn))
  
  return(df)
}


rename_table <- function(oldn, newn, dbpath) {
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  
  set_busytimeout(conn, time=10000)
  DBI::dbBegin(conn)
  
  tryCatch({
    DBI::dbExecute(conn, paste0("ALTER TABLE '", oldn, "'  RENAME TO '", newn,"'") )
    DBI::dbCommit(conn)
    
  }, error = function(e) {
    DBI::dbRollback(conn)
    
  }, finally = DBI::dbDisconnect(conn)
  )
  
}


delete_table <- function(tab, dbpath) {
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  
  set_busytimeout(conn, time=10000)
  DBI::dbBegin(conn)
  
  tryCatch({
    DBI::dbExecute(conn, paste0("DROP TABLE '", tab, "'") )
    DBI::dbCommit(conn)
    
  }, error = function(e) {
    DBI::dbRollback(conn)
    
  }, finally = DBI::dbDisconnect(conn)
  )
}


get_data_from_db_in <- function(select, table, column, q_list, dbpath) {
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  set_busytimeout(conn, time=10000)
  
  qs <- paste0('SELECT ',select,' FROM ', table, ' WHERE "', column, '" IN ', q_list)
  
  df <- tryCatch({
    DBI::dbGetQuery(conn, qs)
    
  }, error = function(e) {
    return(NULL)
    
  }, finally = DBI::dbDisconnect(conn))
  
  return(df)

  #out <- DBI::dbGetQuery(conn, qs)

}




load_columns_from_table_where_symb_val <- function(cols, tablename, col2, symb, val, dbpath) { 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  set_busytimeout(conn, time=10000)
  
  if (all(cols == "*")) {
    cols <- "*"
  } else {
    cols <- paste(paste0(paste0("`",cols),"`"), collapse = ",")
  }
  
  
  df <- tryCatch({
    DBI::dbGetQuery(conn, paste0("SELECT ", cols, " FROM ", "'",tablename,"' WHERE ", paste0(paste0("`",col2),"`"), " ", symb, " ", val) )
    
  }, error = function(e) {
    return(NULL)
    
  }, finally = DBI::dbDisconnect(conn))
  
  return(df)
  
  #df <- DBI::dbGetQuery(conn, paste0("SELECT ", cols, " FROM ", "'",tablename,"' WHERE ", paste0(paste0("`",col2),"`"), " ", symb, " ", val) )
}


load_columns_from_table_where_symb_val_and_where_symb_val <- function(cols, tablename, col2, symb, val, col3, symb1, val1, dbpath) { 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  set_busytimeout(conn, time=10000)
  
  if (all(cols == "*")) {
    cols <- "*"
  } else {
    cols <- paste(paste0(paste0("`",cols),"`"), collapse = ",")
  }
  val <- paste0(paste0("'", val),"'")
  val1 <- paste0(paste0("'", val1),"'")
  
  
  df <- tryCatch({
    DBI::dbGetQuery(conn, paste0("SELECT ", cols, " FROM ", "'",tablename,"' WHERE ", paste0(paste0("`",col2),"`"), " ", symb, " ", val, " AND ", paste0(paste0("`",col3),"`"), " ", symb1, " ", val1) )    
    
  }, error = function(e) {
    return(NULL)
    
  }, finally = DBI::dbDisconnect(conn))
  
  return(df)
  
}


get_countdb <- function(dbpath, tble, len) {
    
    conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
    set_busytimeout(conn, time=10000)
    
    df <- tryCatch({
        DBI::dbGetQuery(conn, paste0("SELECT COUNT(*) FROM ",tble," WHERE Length = ",len,";"))
        
    }, error = function(e) {
        return(NULL)
        
    }, finally = DBI::dbDisconnect(conn))
    
    return(df)
}



find_larger_containing_peptides <- function(query_peps, clmn, tabl, len, col, dbpath, batch_size = 500, preds=T) {

    con <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
	on.exit(DBI::dbDisconnect(con))
    set_busytimeout(con, time=10000)
    nr <- nchar(query_peps[[1]])

	if (clmn != "*" ) {
        clmn <- paste0( paste0("`", clmn), "`")
    }
	
	df <- tryCatch({
        results <- list()
        batches <- split(query_peps, ceiling(seq_along(query_peps) / batch_size))
        
        for (i in seq_along(batches)) {
            
            batch <- batches[[i]]
            
            placeholders <- paste(rep("?", length(batch)), collapse = ",")
            
            sql <- paste0(
                "SELECT ", clmn, ",
              substr(`", col, "`,1,",nr,") AS sub1,
              substr(`", col, "`,2,",nr,") AS sub2
			   FROM `", tabl, "`
			   WHERE (sub1 IN (", placeholders, ")
				  OR sub2 IN (", placeholders, "))", " AND `Length` = ", len
					)
            if (preds == F) {
				sql <- paste0(sql, " AND `ID` NOT LIKE '%predicted%'")
			}
			
            res <- DBI::dbGetQuery(con, sql, params = c(batch, batch))
            #return(res)
            if (nrow(res) > 0) {
                
                match_rows <- do.call(rbind, lapply(seq_len(nrow(res)), function(j) {
                    
                    data.frame(
                        Query = batch[batch %in% c(res$sub1[j], res$sub2[j])],
                        Found = res[[col]][j],
                        stringsAsFactors = FALSE
                    )
                    
                }))
                
                results[[i]] <- match_rows
				}
			}
			do.call(rbind, results)[c("Query", "Found")]}, 
			error = function(e) {
			print(e)
			return(data.frame())
			})
	if( is.null(df) ) {
        return(data.frame())
    }
	return(df)
}
#df <- find_containing_peptides(x, "peptide", table = "9", col = "peptide", batch_size = 500)


find_smaller_matching_peps <- function( query_peps, clmn, tabl, len, col, dbpath, batch_size = 500, preds=T) {
    
    con <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
	on.exit(DBI::dbDisconnect(con))
    set_busytimeout(con, time=10000)
    nr <- nchar(query_peps[[1]])
    nrmin <- nr - 1
    
    if (clmn != "*" ) {
        clmn <- paste0( paste0("`", clmn), "`")
    }
    
	df <- tryCatch({
	
	# ineffecient??
    mapping <- data.frame(
        Query = c(query_peps,query_peps),
        Found = c(substr(query_peps,1,nrmin), substr(query_peps,2,nr)),
        stringsAsFactors = FALSE
    )
    
    uniq <- unique(mapping$Found)
    
    results <- list()
    batches <- split(uniq, ceiling(seq_along(uniq) / batch_size))
    
    for (i in seq_along(batches)) {
        
        batch <- batches[[i]]
        
        placeholders <- paste(rep("?", length(batch)), collapse = ",")
        
        sql <- paste0(
            "SELECT ",clmn," FROM `", tabl,
            "` WHERE `", col, "` IN (", placeholders, ")", " AND `Length` = ", len
        )
		if (preds == F) {
            sql <- paste0(sql, " AND `ID` NOT LIKE '%predicted%'")
        }
		
        res <- DBI::dbGetQuery(con, sql, params = batch)
        if (nrow(res) > 0) {
            
            merged <- merge(mapping, res, by.x = "Found", by.y = col)
            
            results[[i]] <- merged
        }
    }
    
    do.call(rbind, results)[c("Query", "Found")]
	}, 
	error = function(e) {print(e)
						return(data.frame())
						})
	if( is.null(df) ) {
        return(data.frame())
    }
	return(df)
}



find_exact_matches <- function( query_peps, clmn, tabl, col, dbpath, batch_size = 500, preds=T) {
    
    con <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
	on.exit(DBI::dbDisconnect(con))
    set_busytimeout(con, time=10000)
    nr <- nchar(query_peps[[1]])
    
    if (clmn != "*" ) {
        clmn <- paste0( paste0("`", clmn), "`")
    }
    
	df <- tryCatch({
    
    uniq <- unique(query_peps)
    
    results <- list()
    batches <- split(uniq, ceiling(seq_along(uniq) / batch_size))
    
    for (i in seq_along(batches)) {
        
        batch <- batches[[i]]
        
        placeholders <- paste(rep("?", length(batch)), collapse = ",")
        
        sql <- paste0(
            "SELECT ",clmn," FROM `", tabl,
            "` WHERE `", col, "` IN (", placeholders, ")"
        )
		if (preds == F) {
            sql <- paste0(sql, " AND `ID` NOT LIKE '%predicted%'")
        }
		
        res <- DBI::dbGetQuery(con, sql, params = batch)
		
        if (nrow(res) > 0) {
            colnames(res) <- "Found"
            results[[i]] <- res
        }
    }
    
    results <- do.call(rbind, results)
	results[["Query"]] <- results[["Found"]]
	results[c("Query", "Found")]
	
	}, 
	error = function(e) {print(e)
						return(data.frame())
						})
	if( is.null(df) ) {
        return(data.frame())
    }
	return(df)
}




load_columns_from_table_where_NOT_OU <- function(cols, tablename, len, dbpath ) { 
    
    conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
    set_busytimeout(conn, time=10000)
    
    if (all(cols == "*")) {
        cols <- "*"
    } else {
        cols <- paste(paste0(paste0("`",cols),"`"), collapse = ",")
    }
    
    cond <- paste0( "WHERE `Length` = ", len,
				    " AND `peptide` NOT LIKE '%O%'",
				    " AND `peptide` NOT LIKE '%U%'"
				   )
    df <- tryCatch({
        DBI::dbGetQuery(conn, paste0("SELECT ", cols, " FROM ", "'",tablename,"'", cond) )
        
    }, error = function(e) {
        return(NULL)
        
    }, finally = DBI::dbDisconnect(conn))
    
    return(df)
    
}


load_columns_from_table_where_NOT_OU_and_not_pred <- function(cols, tablename, len, dbpath ) { 
    
    conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
    set_busytimeout(conn, time=10000)
    
    if (all(cols == "*")) {
        cols <- "*"
    } else {
        cols <- paste(paste0(paste0("`",cols),"`"), collapse = ",")
    }
    
    cond <- paste0( "WHERE `Length` = ", len, " AND `peptide` NOT LIKE '%O%' AND `peptide` NOT LIKE '%U%' AND `ID` NOT LIKE '%predicted%'")
    #print(paste0("SELECT ", cols, " FROM ", "'",tablename,"'", cond))
    df <- tryCatch({
        DBI::dbGetQuery(conn, paste0("SELECT ", cols, " FROM ", "'",tablename,"'", cond) )
        
    }, error = function(e) {
        return(NULL)
        
    }, finally = DBI::dbDisconnect(conn))
    
    return(df)
}


load_columns_from_table_where_not_pred <- function(cols, tablename, len, dbpath ) { 
    
    conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
    set_busytimeout(conn, time=10000)
    
    if (all(cols == "*")) {
        cols <- "*"
    } else {
        cols <- paste(paste0(paste0("`",cols),"`"), collapse = ",")
    }
    
    cond <- paste0( "WHERE `Length` = ", len, " AND `ID` NOT LIKE '%predicted%'")
    
    df <- tryCatch({
        DBI::dbGetQuery(conn, paste0("SELECT ", cols, " FROM ", "'",tablename,"'", cond) )
        
    }, error = function(e) {
        return(NULL)
        
    }, finally = DBI::dbDisconnect(conn))
    
    return(df)
}


load_columns_from_table_where <- function(cols, tablename, col2, cond, dbpath ) { 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  set_busytimeout(conn, time=10000)
  
  if (all(cols == "*")) {
    cols <- "*"
  } else {
    cols <- paste(paste0(paste0("`",cols),"`"), collapse = ",")
  }
  
  cond <- paste0("(", paste(paste0(paste0("'",cond),"'"), collapse = ",") , ")")
  
  df <- tryCatch({
    DBI::dbGetQuery(conn, paste0("SELECT ", cols, " FROM ", "'",tablename,"' WHERE ", paste0(paste0("`",col2),"`"), " IN ", cond) )
    
  }, error = function(e) {
    return(NULL)
    
  }, finally = DBI::dbDisconnect(conn))
  
  return(df)

}


load_concat <- function(tablename, idcol, targetcol, dbpath) { 
  
  query <- sprintf("SELECT %s, GROUP_CONCAT(%s, ';') AS %s FROM `%s` GROUP BY %s", idcol, targetcol, targetcol, tablename, idcol)
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  set_busytimeout(conn, time=10000)
  
  
  df <- tryCatch({
    DBI::dbGetQuery(conn, query)
    
  }, error = function(e) {
    return(NULL)
    
  }, finally = DBI::dbDisconnect(conn))
  
  return(df)
  
}


load_concat_cond <- function(tablename, idcol, targetcol, cond, dbpath ) { 
  
  cond <- paste0("(", paste(paste0(paste0("'",cond),"'"), collapse = ",") , ")")
  w <- paste0( " WHERE ", paste0(paste0("`",targetcol),"`"), " IN ", cond )
  
  query <- sprintf("SELECT %s, GROUP_CONCAT(%s, ';') AS %s FROM `%s` %s GROUP BY %s", idcol, targetcol, targetcol, tablename, w, idcol)
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  set_busytimeout(conn, time=10000)
  
  
  df <- tryCatch({
    DBI::dbGetQuery(conn, query)
    
  }, error = function(e) {
    return(NULL)
    
  }, finally = DBI::dbDisconnect(conn))
  
  return(df)
  
}


load_columns_from_table_where_like <- function(cols, tablename, col2, cond, dbpath ) { 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  set_busytimeout(conn, time=10000)
  
  if (all(cols == "*")) {
    cols <- "*"
  } else {
    cols <- paste(paste0(paste0("`",cols),"`"), collapse = ",")
  }
  
  col2 <- paste0(paste0("`",col2),"`")
  cond <- paste(paste0(paste0("'%", cond), "%'"), collapse = paste0(" OR ",col2, " LIKE ") )

  df <- tryCatch({
    DBI::dbGetQuery(conn, paste0("SELECT ", cols, " FROM ", "'",tablename,"' WHERE ", col2, " LIKE ", cond) )
    
  }, error = function(e) {
    return(NULL)
    
  }, finally = DBI::dbDisconnect(conn))
  
  return(df)
  
}


load_columns_from_table_where_and_where <- function(cols, tablename, col2, cond, cols3, cond3, dbpath ) { 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  set_busytimeout(conn, time=10000)
  
  if (all(cols == "*")) {
    cols <- "*"
  } else {
    cols <- paste(paste0(paste0("`",cols),"`"), collapse = ",")
  }
  
  cond <- paste0("(", paste(paste0(paste0("'",cond),"'"), collapse = ",") , ")")
  cond3 <- paste0("(", paste(paste0(paste0("'",cond3),"'"), collapse = ",") , ")")
  qshit <- paste0("SELECT ", cols, " FROM ", "'",tablename,"' WHERE ", paste0(paste0("`",col2),"`"), " IN ", cond, " AND ",  paste0(paste0("`",cols3),"`"), " IN ", cond3)

  df <- tryCatch({
    DBI::dbGetQuery(conn, qshit )
    
  }, error = function(e) {
    return(NULL)
    
  }, finally = DBI::dbDisconnect(conn))
  
  return(df)
  
  #df <- DBI::dbGetQuery(conn, qshit )
}


load_columns_from_table_where_and_where_and_where <- function(cols, tablename, col2, cond, cols3, cond3, cols4, cond4, dbpath ) { 
  # untested....
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  set_busytimeout(conn, time=10000)
  
  if (all(cols == "*")) {
    cols <- "*"
  } else {
    cols <- paste(paste0(paste0("`",cols),"`"), collapse = ",")
  }
  
  cond <- paste0("(", paste(paste0(paste0("'",cond),"'"), collapse = ",") , ")")
  cond3 <- paste0("(", paste(paste0(paste0("'",cond3),"'"), collapse = ",") , ")")
  cond4 <- paste0("(", paste(paste0(paste0("'",cond4),"'"), collapse = ",") , ")")
  
  qshit <- paste0("SELECT ", cols, " FROM ", "'",tablename,"' WHERE ", paste0(paste0("`",col2),"`"), " IN ", cond, " AND ",  paste0(paste0("`",cols3),"`"), " IN ", cond3, " AND ",  paste0(paste0("`",cols4),"`"), " IN ", cond4)
  
  df <- tryCatch({
    DBI::dbGetQuery(conn, qshit )
    
  }, error = function(e) {
    return(NULL)
    
  }, finally = DBI::dbDisconnect(conn))
  
  return(df)
  
  #df <- DBI::dbGetQuery(conn, qshit )
}


# mainly used to get string edges from string.db
load_columns_from_table_where_and_where_and_symb_val<- function(cols, tablename, col2, cond, col3, cond2, col4, symb, val, dbpath, rem_dups = T) { 
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  set_busytimeout(conn, time=10000)
  
  if (all(cols == "*")) {
    cols <- "*"
  } else {
    cols <- paste(paste0(paste0("`",cols),"`"), collapse = ",")
  }
  
  cond <- paste0("(", paste(paste0(paste0("'",cond),"'"), collapse = ",") , ")")
  cond2 <- paste0("(", paste(paste0(paste0("'",cond2),"'"), collapse = ",") , ")")
  df <- tryCatch({
    DBI::dbGetQuery(conn, paste0("SELECT ", cols, " FROM ", "'",tablename,"' WHERE ", paste0(paste0("`",col2),"`"), " IN ", cond, " AND ", paste0(paste0("`",col3),"`"), " IN ", cond2, " AND ", paste0(paste0("`",col4),"`"), symb, val) )
    
  }, error = function(e) {
    return(NULL)
    
  }, finally = DBI::dbDisconnect(conn))
  
  if (rem_dups == T) {
    
    return( df[!(duplicated(apply(df[c(1,2)], MARGIN = 1, FUN = function(x){ paste(sort(x), collapse = "_")  }))),] )
    
  } else {
    
    return(df)
    
  }
}



get_table_tableinfo <- function(tablename, dbpath ) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  set_busytimeout(conn, time=10000)
  
  df <- tryCatch({
    DBI::dbGetQuery(conn, paste0("PRAGMA table_info('",tablename ,"')"))
    
  }, error = function(e) {
    return(NULL)
    
  }, finally = DBI::dbDisconnect(conn))
  
  return(df)
  
  #info <- DBI::dbGetQuery(conn, paste0("PRAGMA table_info('",tablename ,"')"))
}


update_column_values <- function(table_name, column_name, new_values, uniqIDcolname, dbpath ) {
  
  
  al <- load_entire_table(table_name)
  al[[column_name]] <- new_values
  add_or_replace_table_in_db(al, table_name)
  
}


update_column_valuesSLOW <- function(table_name, column_name, new_values, uniqIDcolname, dbpath ) {
  
  # getting the row ids
  uniqID <- load_columns_from_table(uniqIDcolname, table_name)[[uniqIDcolname]]
  
  if (is.null(uniqID)) {
    return(NULL)
  }
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  set_busytimeout(conn, time=10000)
  
  update_statement <- paste0(
    "UPDATE '", table_name,
    "' SET `", column_name, "` = ? WHERE `", uniqIDcolname, "` = ?"
  )
  
  DBI::dbBegin(conn)
  
  tryCatch({
    for (i in seq_along(new_values)) {
      DBI::dbExecute(conn, update_statement, params = list(new_values[[i]], uniqID[[i]] ))
    }
    DBI::dbCommit(conn)
    
  }, error = function(e) {
    DBI::dbRollback(conn)
    
  }, finally = DBI::dbDisconnect(conn)
  )
  
}


add_column_to_table <- function(table_name, column_name, new_values, uniqIDcolname, uniqID, column_definition="REAL", dbpath ) {

  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  set_busytimeout(conn, time=10000)
  
  
  DBI::dbBegin(conn)
  
  tryCatch({
    # making the column
    alter_statement <- paste0(
      "ALTER TABLE '", table_name,
      "' ADD COLUMN ", column_name, " ", column_definition
    )
    
    DBI::dbExecute(conn, alter_statement)
    
    
    update_statement <- paste0(
      "UPDATE '", table_name,
      "' SET ", column_name, " = ? WHERE ", uniqIDcolname, " = ?"
    )
    
    for (i in seq_along(new_values)) {
      DBI::dbExecute(conn, update_statement, params = list(new_values[[i]], uniqID[[i]] ))
    }
    
    DBI::dbCommit(conn)
    
  }, error = function(e) {
    DBI::dbRollback(conn)
    
  }, finally = DBI::dbDisconnect(conn)
  )
}


set_to_WAL <- function(dbpath) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  DBI::dbExecute(conn, "PRAGMA journal_mode=WAL;")
  DBI::dbDisconnect(conn)
}


check_wal <- function(dbpath ) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  print(DBI::dbGetQuery(conn, "PRAGMA journal_mode;"))
  DBI::dbDisconnect(conn)
}

# has to be done for every connection!
set_busytimeout <- function(conn, time=10000) {
  #conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  DBI::dbExecute(conn, paste0("PRAGMA busy_timeout=",time,";"))
  #DBI::dbDisconnect(conn)
}


check_busytimeout <- function(conn) {
  #conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  print(DBI::dbGetQuery(conn, "PRAGMA busy_timeout;"))
  #DBI::dbDisconnect(conn)
}


delete_all_tables_in_db <- function(nameidlst, dbpath ) {
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  set_busytimeout(conn, time=10000)

  DBI::dbBegin(conn)
  
  tryCatch({
    
    for ( i in nameidlst) {
      DBI::dbExecute(conn, paste0("DROP TABLE '", i, "'") )
    }
    DBI::dbCommit(conn)
    
  }, error = function(e) {
    DBI::dbRollback(conn)
    
  }, finally = DBI::dbDisconnect(conn)
  )
  
}

vacuum_db <- function(dbpath) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  
  set_busytimeout(conn, time=10000)
  
  tryCatch({
    DBI::dbExecute(conn, "VACUUM")
    
  }, error = function(e) {
    DBI::dbRollback(conn)
    
  }, finally = DBI::dbDisconnect(conn)
  )
}

#load_table_with_one_colcond <- function(tablename, col, cond) { 
  
#  conn <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  
#  df <- DBI::dbGetQuery(conn, paste0("SELECT * FROM ", tablename))
  
#  DBI::dbDisconnect(conn)
  
#  return(df)
#}


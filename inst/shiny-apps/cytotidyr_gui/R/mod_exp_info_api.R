###############################################################################

## helper functions

exp_id_from_downloaded_files <- function() {
    exp_id <- unlist(list.dirs())[unlist(lapply(list.dirs(),
                                                grepl,
                                                pattern = '^./[0-9]+$'))]
    exp_id <- substr(exp_id, 3, nchar(exp_id))
    return(exp_id)
}

#get exp population list from cytobank api
get_populations <- function(cyto_session, exp_id){
  print(exp_id)
    return(CytobankAPI::populations.list(cyto_session, exp_id,  output = "default"))
}

#get exp gate list from cytobank api
get_gates <- function(cyto_session, exp_id){
    return(CytobankAPI::gates.list(cyto_session, exp_id, output = "default"))
}

get_compensations <- function(cyto_session, exp_id) {
    return(CytobankAPI::compensations.list(cyto_session, exp_id, output = 'default', timeout = 30))
}

#name lookup table for consistent naming?
# "Panel 1" is hardcoded
#needs exp_id from shiny ui
get_lut <- function(cyto_session, exp_id ) {

    scales <- CytobankAPI::scales.list(cyto_session,
                          exp_id,
                          output = "default")

    mypanel <- CytobankAPI::panels.list(cyto_session,
                           exp_id,
                           output = "default")[[1]][["channels"]]
    scales_df <- as.data.frame(lapply(scales, function(X) unname(unlist(X))))
    mypanel_df <- as.data.frame(lapply(mypanel, function(X) unname(unlist(X))))

    
    lut <- merge.data.frame(mypanel_df,
                            scales_df,
                            by.x= "normalizedShortNameId",
                            by.y ="normalizedShortNameId")
    extrarow <- data.frame(-1, "Null", "Null", -1, 0, 0, 0, 0, 0, 1)
    colnames(extrarow) <- colnames(lut)
    lut <- rbind(lut, extrarow)
    return(head(lut, -1))
}

## ui

# exp_info_ui <- function(id) {
#     ns <- NS(id)
#     tagList(
#         actionButton(ns('get_info_button'), 'Proceed')
#     )
# }
# 
# ## server
# 
# exp_info <- function(input, output, session, cyto_session, x) {
# 
#   
#     #get experiment info from cytobank api
#     #mayber add check_connection conditional
#     exp_info <- eventReactive(input$get_info_button, {
#         progress <- shiny::Progress$new()
#         progress$set(message = "Gathering Experiment info...", value = 0.2)
#         # Close the progress when this reactive exits (even if there's an error)
#         on.exit(progress$close())
#         
#         n <- 5
#         updateProgress <- function(detail = NULL) {
#           progress$inc(amount = 1/n, detail = detail)
#         }
#         exp_id <- exp_id_from_downloaded_files()
#         if (is.function(updateProgress)) {
#           updateProgress(detail = "Fetching Populations")
#         }
#         exp_pops <- get_populations(cyto_session(), exp_id)
#         if (is.function(updateProgress)) {
#           updateProgress(detail = "Fetching Compesnations")
#         }
#         exp_comps <- get_compensations(cyto_session(), exp_id)
# 
#         if (is.function(updateProgress)) {
#           updateProgress(detail = "Fetching Gating Scheme")
#         }
#         exp_gates <- get_gates(cyto_session(), exp_id)
#         if (is.function(updateProgress)) {
#           updateProgress(detail = "Fetching Scales")
#         }
#         exp_lut <- get_lut(cyto_session(), exp_id)
#         
#         updateNavbarPage(x, "mainNavbarPage", "tab2") #proceed to next page
#         
#         return(list('exp_id' = exp_id,
#                     'exp_pops' = exp_pops,
#                     'exp_comps' = exp_comps,
#                     'exp_gates' = exp_gates,
#                     'exp_lut' = exp_lut))
#     })
#     return(exp_info)
# }


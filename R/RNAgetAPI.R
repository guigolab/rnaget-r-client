#' RNAget class
#'
#' @export
#'
RNAget <- R6::R6Class("RNAget",
                      private = list(
                        return_response = function(url, auth_headers){
                          response <- httr::GET(url, self$auth_headers)
                          httr::message_for_status(response)
                          print(httr::http_type(response))
                          if(httr::http_type(response)=="application/json"){
                            data <- jsonlite::fromJSON(httr::content(response, "text"))
                          }else if(httr::http_type(response)=="application/tab-separated-values"){
                            data <- httr::content(response, "text")
                          }else{
                            ##return bytes as fallback, let the user parse the response
                            data <- httr::content(response, "text")
                          }
                          return(data)
                        }
                      ),
                      public = list(
                        base_url = NULL,
                        auth_headers = NULL,
                        initialize = function(base_url, token=NULL) {
                          if(base_url == 'encode'){
                            self$base_url <- 'https://rnaget.encodeproject.org'
                          }else if(base_url == 'gtex'){
                            self$base_url <- 'http://gtexportal.org/rnaget'
                          }else{
                            self$base_url <- base_url
                          }
                          if (is.null(token)) {
                            self$auth_headers <- NULL
                          }else{
                            self$auth_headers <- httr::add_headers("Authorization" = paste("Bearer", token, sep=" "))
                          }
                        },
                        get_studies = function(version = NULL){
                          if(is.null(version)){
                            url <- paste0(self$base_url, "/studies")
                          }else{
                            url <- paste0(self$base_url, "/studies","?", "version=", version)
                          }
                          return(private$return_response(url, self$auth_headers))
                        },

                        get_study = function(id){
                          url <- paste0(self$base_url, "/studies/", id)
                          response <- httr::GET(url, self$auth_headers)
                          httr::message_for_status(response)
                          return(private$return_response(url, self$auth_headers))

                        },
                        get_projects = function(version=NULL){
                          if(is.null(version)){
                            url <- paste0(self$base_url, "/projects")
                          }else{
                            url <- paste0(self$base_url, "/projects","?", "version=", version)
                          }
                          return(private$return_response(url, self$auth_headers))

                        },
                        get_project = function(id){
                          url <- paste0(self$base_url, "/projects/", id)
                          return(private$return_response(url, self$auth_headers))

                        },
                        get_expression_list = function(args=NULL,download=FALSE){
                          if(download){
                            path <- "/expressions/bytes"
                          }else{
                            path <- "/expressions/ticket"
                          }
                          url <- paste0(self$base_url, path)
                          if(is.list(args)){
                            url <- httr::modify_url(url, query=args)
                          }else if(!is.null(args)){
                            stop('args must be a list')
                          }
                          return(private$return_response(url, self$auth_headers))

                        },
                        get_expression_list_bytes = function(args=NULL){
                          url <- paste0(self$base_url, "/expressions/bytes")
                          if(is.list(args)){
                            url <- httr::modify_url(url, query=args)
                          }else if(!is.null(args)){
                            stop('args must be a list')
                          }
                          return(private$return_response(url, self$auth_headers))

                        },
                        get_expression = function(id, args=NULL,download=FALSE){
                          if(download){
                            path <- "/bytes"
                          }else{
                            path <- "/ticket"
                          }
                          url <- paste0(self$base_url, "/expressions/",id,path)
                          if(is.list(args)){
                            url <- httr::modify_url(url, query=args)
                          }else if(!is.null(args)){
                            stop('args must be a list')
                          }
                          return(private$return_response(url, self$auth_headers))

                        },
                        get_continuous_list = function(args=NULL, download=FALSE){
                          if(download){
                            path <- "/continuous/bytes"
                          }else{
                            path <- "/continuous/ticket"
                          }
                          url <- paste0(self$base_url, path)
                          if(is.list(args)){
                            url <- httr::modify_url(url, query=args)
                          }else if(!is.null(args)){
                            stop('args must be a list')
                          }
                          return(private$return_response(url, self$auth_headers))
                        },
                        get_continuous = function(id,args=NULL, download=FALSE){
                          if(download){
                            path <- "/bytes"
                          }else{
                            path <- "/ticket"
                          }
                          url <- paste0(self$base_url, "/continuous/",id,path)
                          if(is.list(args)){
                            url <- httr::modify_url(url, query=args)
                          }else if(!is.null(args)){
                            stop('args must be a list')
                          }
                          return(private$return_response(url, self$auth_headers))
                        },
                        get_expression_formats = function(){
                          url <- paste0(self$base_url, "/expressions", "/formats")
                          return(private$return_response(url, self$auth_headers))

                        },
                        get_expression_units = function(){
                          url <- paste0(self$base_url, "/expressions", "/units")
                          return(private$return_response(url, self$auth_headers))

                        },
                        get_filters = function(type){
                          return(
                            switch(
                              type,
                              projects={
                                private$return_response(paste0(self$base_url, "/projects", "/filters"), self$auth_headers)
                              },
                              studies={
                                private$return_response(paste0(self$base_url, "/studies", "/filters"), self$auth_headers)
                              },
                              expressions={
                                private$return_response(paste0(self$base_url, "/expressions", "/filters"), self$auth_headers)
                              },
                              continuous={
                                private$return_response(paste0(self$base_url, "/continuous", "/filters"), self$auth_headers)
                              },
                              {
                                stop('type must be one of projects, studies, expressions or continuous')
                              }
                            )
                          )
                        }
                      )
)


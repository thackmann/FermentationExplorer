###########################
#Define user interface (UI)
###########################
aboutUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width=2),
      column(
        p(h3("About")),
        p(""),
        p(""),
        p(h5("Development of this resource is described in a manuscript currently under review.  Citation to the manuscript will be provided here upon acceptance.")),
        p(""),
        p(h5(tagList("We acknowledge ",url_BacDive,", ",url_GOLD,", and ",url_IMG," for use of their data.  Data from ",url_BacDive," appear under the terms of a ", url_CC,".  Data from ", url_GOLD," and ",url_IMG," appear under the terms of ",url_JGI))),
        p(""),
        p(h5("This work was supported by an Agriculture and Food Research Initiative Competitive Grant [grant no. 2018-67015-27495] and Hatch Project [accession no. 1019985] from the United States Department of Agriculture National Institute of Food and Agriculture.")),
        p(""),
        width=8),
      column(width=2),
    )
  )
}  

##############
#Define server
##############
aboutServer <- function(input, output, session) {
  #No server-side logic
}
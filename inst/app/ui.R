# Create the ui object using the unexported app_ui function
ui <- visitorFlowTool:::app_ui()
visitorFlowTool:::vftStartupMark("UI built (first page request)")
# The value of the LAST expression is what Shiny serves. A function returning
# the page rendered once, not the tag object Shiny would re-render per request
# - see vftCachedUi() in R/app_ui.R.
visitorFlowTool:::vftCachedUi(ui)

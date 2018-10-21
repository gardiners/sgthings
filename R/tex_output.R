# Functions to produce LaTex output.

#' Generate a LaTeX tabular enironment from a rectangular R object.
#
#' @param A A matrix, dataframe or similar.
#' @param latex_env The type of LaTeX tabular environment to generate.
#' @param .digits Optional digits to round table elements.
#' @param .margin The dimension to iterate over - will be output as rows.
teXtab <- function(A, latex_env = "bmatrix", .digits = NULL, .margin = 1){
  A %>%
  {if(!is.null(.digits)) round(., .digits) else .} %>%
    array_branch(margin = 1) %>%
    map(str_flatten, collapse = " & ") %>%
    str_flatten(collapse = "\\\\ \n") %>%
    str_glue("\\begin{{{latex_env}}}\n",
             ., "\n",
             "\\end{{{latex_env}}}")
}

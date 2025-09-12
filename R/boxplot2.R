#' Basic boxplot plotter.  Templates can be made to adjust it with make_template.
#'
#' @param x data in the form of a dbl vecotor for which you would like the make a boxplot
#' @param template if you have made a template with make_template, placing its name as a string here will run the template code instead.
#'
#' @return a ggplot object
#'
#' @examples
#' make_template("boxplot2", "triangle") # make any desired changes to the boxplot
#' a <- rbeta(1e2,1,3)
#' boxplot2_template(x = a, template = "triangle.R")
#'
#' @export
#'
boxplot2_template <- function(
    x,
    template = NULL
) {
  if(is.null(template)){
    mean_x <- mean(x)
    sd_x <- sd(x)
    outlier_range <- sd_x*2.5
    outlier <- data.frame(x = x[x-outlier_range>0|x+outlier_range<0])
    x_trim <- x[x-outlier_range<0&x+outlier_range>0]
    qx <- quantile(x_trim, probs = c(0, .25, .5, .75, 1))
    x_trim <- data.frame(x = x_trim)

    ggplot(outlier, aes(x = 0))+
      geom_point(aes(y = x), pch = 8)+
      geom_errorbar(ymin = qx[1], ymax = qx[5], width = 0.1)+
      geom_crossbar(ymin = qx[2], y = qx[3], ymax = qx[4], width = 0.2, fill = "white")+
      expand_limits(y = range(x))
  }
  else {
    source(template, local = TRUE)
  }
}

#' Creates templates
#'
#' @param fun the function for which you want to make a template
#' @param template_name the resulting filename for your template.
#'
#' @return A .r file in your working directory.
#'
#' @examples
#' make_template("boxplot2", "triangle") # make any desired changes to the boxplot
#' a <- rbeta(1e2,1,3)
#' boxplot2_template(x = a, template = "triangle.R")
#'
#' @export
#'
make_template <- function(fun = NULL, template_name = "basic"){
  if(fun %in% c("boxplot2")){
  output <- glue("#Could concievably put an explanation of how to edit the code here \n\n",
         "rlang::expr({{",
         system.file("extdata", glue(fun,".txt"), package = "simplex") |>
           readLines() |> paste(collapse = "\n"),"}})"," |> deparse() |> writeLines(","\"",template_name,".R","\"",")")
    invisible(rstudioapi::documentNew(
      text = output,
      type = "r"
    ))
  }
  else {
    glue(fun," is not an existing function of the simplex package")
    invisible(NULL)
  }
}

#' Basic boxplot plotter.  Templates can be made to adjust it with make_template.
#'
#' @param x data in the form of a dbl vecotor for which you would like the make a boxplot
#' @param format Whether the function should plot, or return an object for editing.
#'
#' @return a ggplot object
#'
#' @examples
#' a <- rbeta(1e2,1,3)
#' boxplot2_original(x = a) # just makes a boxplot
#' boxplot2_original(x = a, formate = "edit") # opens a doc for editing
#' boxplot2_adjust() #runs the edited code
#'
#' @export
#'
boxplot2_original <- function(
    x,
    format = "plot"
) {
    mean_x <- mean(x)
    sd_x <- sd(x)
    outlier_range <- sd_x*2.5
    outlier <- data.frame(x = x[x-outlier_range>0|x+outlier_range<0])
    x_trim <- x[x-outlier_range<0&x+outlier_range>0]
    qx <- quantile(x_trim, probs = c(0, .25, .5, .75, 1))
    x_trim <- data.frame(x = x_trim)

    assign("code_output",quote(
    ggplot(outlier, aes(x = 0))+
      geom_point(aes(y = x), pch = 8)+
      geom_errorbar(ymin = qx[1], ymax = qx[5], width = 0.1)+
      geom_crossbar(ymin = qx[2], y = qx[3], ymax = qx[4], width = 0.2, fill = "white")+
      expand_limits(y = range(x))))
    if (format == "plot") eval(code_output)
    else if (format == "edit") {
      output <- glue("#Could concievably put an explanation of how to edit the code here \n\n",
                     "code_output$code <- ","expr({{",
                     system.file("extdata", "boxplot2.txt", package = "simplex") |>
                       readLines() |> paste(collapse = "\n"),"}})")
      rstudioapi::documentNew(
        text = output,
        type = "r"
      )
      assign("code_output", list(code = code_output, env = environment()), envir = .GlobalEnv)
    }
    else list(code = code_output, env = environment())
}

#' @export
boxplot2_adjust <- function(){
  eval(code_output$code, envir = code_output$env)
}

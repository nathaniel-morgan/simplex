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
#' boxplot2(x = a, template = "triangle.R")
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
      expands_limits(y = range(X))
  }
  else {
    source(template, local = TRUE)
  }
}

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

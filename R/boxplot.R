#' Basic boxplot plotter.  Templates can be made to adjust it with make_template.
#'
#' @param x data in the form of a tibble with a column of values named value and column of classes name class
#' @param template if you have made a template with make_template, placing its name as a string here will run the template code instead.
#'
#' @return a ggplot object
#'
#' @examples
#' make_template("boxplot", "triangle") # make any desired changes to the boxplot
#' a <- tibble(value = c(rnorm(2e2,1), 4*rbeta(2e2,1,4)), class = c(rep("a",2e2),rep("b",2e2)))
#' boxplot_template(x = a, template = "triangle.R")
#'
#' @export
#'
boxplot_template <- function(
    x,
    template = NULL
) {
  if(is.null(template)){
    x <- x |>
      group_by(class) |>
      mutate(
        q1 = quantile(value, 0.25),
        q3 = quantile(value, 0.75),
        median = median(value),
        iqr = q3 - q1,
        lower = q1 - 1.5 * iqr,
        upper = q3 + 1.5 * iqr,
        outlier = value < lower | value > upper,
        min = min(ifelse(!outlier, value, NA), na.rm = TRUE),
        max = max(ifelse(!outlier, value, NA), na.rm = TRUE)
      ) |>
      ungroup()

    print(ggplot(x, aes(x = class, y = value))+
            geom_point(data = x |> filter(outlier == TRUE), pch = 8)+
            geom_errorbar(aes(ymin = min, ymax = max), width = 0.1)+
            geom_crossbar(aes(ymin = q1, y = median, ymax = q3), width = 0.2, fill = "white"))
  }
  else {
    source(template, local = TRUE)
  }
}

#' Creates templates
#'
#' @param fun the function for which you want to make a template as a string.
#' @param template_name the resulting filename for your template as a string.
#'
#' @return A .r file in your working directory.
#'
#' @examples
#' make_template("boxplot", "triangle") # make any desired changes to the boxplot
#' a <- tibble(value = c(rnorm(2e2,1), 4*rbeta(2e2,1,4)), class = c(rep("a",2e2),rep("b",2e2)))
#' boxplot_template(x = a, template = "triangle.R")
#'
#' @export
#'
make_template <- function(fun = NULL, template_name = "basic"){
  if(fun %in% c("boxplot", "weather")){
    output <- glue(
      "#Could concievably put an explanation of how to edit the code here \n\n",
      "rlang::expr({{",
      system.file("extdata", glue(fun,".txt"), package = "simplex") |>
      readLines() |>
      paste(collapse = "\n"),
      "}})", " |> deparse() |> writeLines(", "\"", template_name, ".R", "\"", ")"
    )
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

#' Basic boxplot plotter.  code can be edited before plotting with format edit. boxplot_adjust can run the modified code.
#'
#' @param x data in the form of a dbl vector for which you would like the make a boxplot
#' @param format Whether the function should plot, or return an object for editing.
#'
#' @return a ggplot object or a list containing ggplot code along with a function environment.
#'
#' @examples
#' a <- tibble(value = c(rnorm(2e2,1), 4*rbeta(2e2,1,4)), class = c(rep("a",2e2),rep("b",2e2)))
#' boxplot_original(x = a) # just makes a boxplot
#' boxplot_original(x = a, format = "edit") # opens a doc for editing
#' boxplot_adjust() #runs the edited code
#'
#' @export
#'
boxplot_original <- function(
    x,
    format = "plot"
) {
  x <- x |>
    group_by(class) |>
    mutate(
      q1 = quantile(value, 0.25),
      q3 = quantile(value, 0.75),
      median = median(value),
      iqr = q3 - q1,
      lower = q1 - 1.5 * iqr,
      upper = q3 + 1.5 * iqr,
      outlier = value < lower | value > upper,
      min = min(ifelse(!outlier, value, NA), na.rm = TRUE),
      max = max(ifelse(!outlier, value, NA), na.rm = TRUE)
    ) |>
    ungroup()

    assign("code_output",quote(
      ggplot(x, aes(x = class, y = value))+
        geom_point(data = x |> filter(outlier == TRUE), pch = 8)+
        geom_errorbar(aes(ymin = min, ymax = max), width = 0.1)+
        geom_crossbar(aes(ymin = q1, y = median, ymax = q3), width = 0.2, fill = "white")
    ))
    if (format == "plot") eval(code_output)
    else if (format == "edit") {
      output <- glue(
        "#Could concievably put an explanation of how to edit the code here \n\n",
        "code_output$code <- ","expr({{",
        system.file("extdata", "boxplot.txt", package = "simplex") |>
        readLines() |>
        paste(collapse = "\n"), "}})"
      )
      rstudioapi::documentNew(
        text = output,
        type = "r"
      )
      assign("code_output", list(code = code_output, env = environment()), envir = .GlobalEnv)
    }
    else list(code = code_output, env = environment())
}

#' @export
boxplot_adjust <- function(name = code_output){
  eval(name$code, envir = name$env)
}

#' Basic boxplot plotter. code can be modified before plotting with format edit
#'
#' @param x data in the form of a tibble with a column of values named value and column of classes name class
#' @param format Whether the function should plot (plot), or return an object for editing (edit).
#' @param name what name will the list object containing your plot code and environment contain
#'
#' @return a ggplot object or a named list containing ggplot code along with a function environment.
#'
#' @examples
#' a <- tibble(value = c(rnorm(2e2,1), 4*rbeta(2e2,1,4)), class = c(rep("a",2e2),rep("b",2e2)))
#' boxplot_name(x = a) # just makes a boxplot
#' boxplot_name(x = a, format = "edit", name = "circle_points") # opens a doc for editing
#' boxplot_adjust(circle_points) #runs the edited code
#'
#' @export
#'
boxplot_name <- function(
    x,
    format = "plot",
    name = "code_output"
) {
  x <- x |>
    group_by(class) |>
    mutate(
      q1 = quantile(value, 0.25),
      q3 = quantile(value, 0.75),
      median = median(value),
      iqr = q3 - q1,
      lower = q1 - 1.5 * iqr,
      upper = q3 + 1.5 * iqr,
      outlier = value < lower | value > upper,
      min = min(ifelse(!outlier, value, NA), na.rm = TRUE),
      max = max(ifelse(!outlier, value, NA), na.rm = TRUE)
    ) |>
    ungroup()

  assign(name, quote(
    ggplot(x, aes(x = class, y = value))+
      geom_point(data = x |> filter(outlier == TRUE), pch = 8)+
      geom_errorbar(aes(ymin = min, ymax = max), width = 0.1)+
      geom_crossbar(aes(ymin = q1, y = median, ymax = q3), width = 0.2, fill = "white")
  ))
  if (format == "plot") eval(code_output)
  else if (format == "edit") {
    output <- glue(
      "#Could concievably put an explanation of how to edit the code here \n\n",
      name, "$code <- ", "expr({{",
      system.file("extdata", "boxplot.txt", package = "simplex") |>
      readLines() |>
      paste(collapse = "\n"), "}})"
    )
    rstudioapi::documentNew(
      text = output,
      type = "r"
    )
    assign(name, list(code = code_output, env = environment()), envir = .GlobalEnv)
  }
  else list(code = get(name), env = environment())
}

#' Basic boxplot plotter. code can be modified before plotting with format edit, and bound in the global
#'
#' @param x data in the form of a tibble with a column of values named value and column of classes name class
#' @param format Whether the function should plot, or return an object for editing.
#'
#' @return a ggplot object or a bnd class list containing ggplot code along with a function environment.
#'
#' @examples
#' a <- tibble(value = c(rnorm(2e2,1), 4*rbeta(2e2,1,4)), class = c(rep("a",2e2),rep("b",2e2)))
#' boxplot_binding(x = a) # just makes a boxplot
#' diamond_points <- boxplot_binding(x = a, format = "edit") # opens a doc for editing
#' diamond_points #runs the edited code using new S3 method
#'
#' @export
#'
boxplot_binding <- function(
    x,
    format = "plot"
) {
  x <- x |>
    group_by(class) |>
    mutate(
      q1 = quantile(value, 0.25),
      q3 = quantile(value, 0.75),
      median = median(value),
      iqr = q3 - q1,
      lower = q1 - 1.5 * iqr,
      upper = q3 + 1.5 * iqr,
      outlier = value < lower | value > upper,
      min = min(ifelse(!outlier, value, NA), na.rm = TRUE),
      max = max(ifelse(!outlier, value, NA), na.rm = TRUE)
    ) |>
    ungroup()

  assign("code_output", quote(
    ggplot(x, aes(x = class, y = value))+
      geom_point(data = x |> filter(outlier == TRUE), pch = 8)+
      geom_errorbar(aes(ymin = min, ymax = max), width = 0.1)+
      geom_crossbar(aes(ymin = q1, y = median, ymax = q3), width = 0.2, fill = "white")
  ))
  if (format == "plot") eval(code_output)
  else if (format == "edit") {
    output <- glue(
      "#Be sure to fill in the space to the left of $code with the object name you are adjusting \n\n",
      "$code <- ", "expr({{",
      system.file("extdata", "boxplot.txt", package = "simplex") |>
      readLines() |>
      paste(collapse = "\n"), "}})"
    )
    rstudioapi::documentNew(
      text = output,
      type = "r"
    )
    bnd(list(code = code_output, env = environment()))
  }
  else bnd(list(code = get(name), env = environment()))
}

new_bnd <- function(x) {
  structure(list(code = x$code, env = x$env), class = "bnd")
}

bnd <- function(...) {
  new_bnd(...)
}

#' @export
print.bnd <- function(x, ...) {
  eval(x$code, envir = x$env)
  invisible(x)
}

boxplot <- function(x){
  x <- x |>
    group_by(class) |>
    mutate(
      q1 = quantile(value, 0.25),
      q3 = quantile(value, 0.75),
      median = median(value),
      iqr = q3 - q1,
      lower = q1 - 1.5 * iqr,
      upper = q3 + 1.5 * iqr,
      outlier = value < lower | value > upper,
      min = min(ifelse(!outlier, value, NA), na.rm = TRUE),
      max = max(ifelse(!outlier, value, NA), na.rm = TRUE)
    ) |>
    ungroup()

  print(ggplot(x, aes(x = class, y = value))+
          geom_point(data = x |> filter(outlier == TRUE), pch = 8)+
          geom_errorbar(aes(ymin = min, ymax = max), width = 0.1)+
          geom_crossbar(aes(ymin = q1, y = median, ymax = q3), width = 0.2, fill = "white"))
}

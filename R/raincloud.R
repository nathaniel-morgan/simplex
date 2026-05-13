#' Raincloud-style distribution plotter. Templates can be made to adjust it with make_template.
#'
#' @param x data in the form of a tibble with a column of values named value and column of classes named class.
#' @param template if you have made a template with make_template, placing its name as a string here will run the template code instead.
#'
#' @return a ggplot object
#'
#' @examples
#' make_template("raincloud", "custom_raincloud")
#' a <- tibble(value = c(rnorm(2e2, 1), 4 * rbeta(2e2, 1, 4)),
#'             class = c(rep("a", 2e2), rep("b", 2e2)))
#' raincloud_template(x = a, template = "custom_raincloud.R")
#'
#' @importFrom ggplot2 ggplot aes geom_violin geom_jitter geom_boxplot stat_summary labs theme_minimal theme
#' @importFrom glue glue
#' @export
#'
raincloud_template <- function(
    x,
    template = NULL
) {
  if(is.null(template)){
    print(
      ggplot(x, aes(x = class, y = value, fill = class, color = class))+
        geom_violin(alpha = 0.25, trim = FALSE, width = 0.85, color = NA)+
        geom_jitter(width = 0.12, alpha = 0.35, size = 1.2, show.legend = FALSE)+
        geom_boxplot(width = 0.16, outlier.shape = NA, fill = "white", alpha = 0.8, color = "black")+
        stat_summary(fun = median, geom = "point", size = 2.2, color = "black")+
        labs(x = NULL, y = NULL)+
        theme_minimal()+
        theme(legend.position = "none")
    )
  }
  else {
    source(template, local = TRUE)
  }
}

#' Raincloud-style distribution plotter. code can be edited before plotting with format edit. raincloud_adjust can run the modified code.
#'
#' @param x data in the form of a tibble with a column of values named value and column of classes named class.
#' @param format Whether the function should plot, or return an object for editing.
#'
#' @return a ggplot object or a list containing ggplot code along with a function environment.
#'
#' @examples
#' a <- tibble(value = c(rnorm(2e2, 1), 4 * rbeta(2e2, 1, 4)),
#'             class = c(rep("a", 2e2), rep("b", 2e2)))
#' raincloud_original(x = a)
#' raincloud_original(x = a, format = "edit")
#' raincloud_adjust()
#'
#' @export
#'
raincloud_original <- function(
    x,
    format = "plot"
) {
  code_output <- quote(
    ggplot(x, aes(x = class, y = value, fill = class, color = class))+
      geom_violin(alpha = 0.25, trim = FALSE, width = 0.85, color = NA)+
      geom_jitter(width = 0.12, alpha = 0.35, size = 1.2, show.legend = FALSE)+
      geom_boxplot(width = 0.16, outlier.shape = NA, fill = "white", alpha = 0.8, color = "black")+
      stat_summary(fun = median, geom = "point", size = 2.2, color = "black")+
      labs(x = NULL, y = NULL)+
      theme_minimal()+
      theme(legend.position = "none")
  )
  if (format == "plot") eval(code_output)
  else if (format == "edit") {
    output <- glue(
      "#Could concievably put an explanation of how to edit the code here \n\n",
      "code_output$code <- ", "expr({{",
      system.file("extdata", "raincloud.R", package = "simplex") |>
        readLines() |>
        paste(collapse = "\n"),
      "}})"
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
raincloud_adjust <- function(name = code_output){
  eval(name$code, envir = name$env)
}

#' Raincloud-style distribution plotter. code can be modified before plotting with format edit.
#'
#' @param x data in the form of a tibble with a column of values named value and column of classes named class.
#' @param format Whether the function should plot (plot), or return an object for editing (edit).
#' @param name what name will the list object containing your plot code and environment contain.
#'
#' @return a ggplot object or a named list containing ggplot code along with a function environment.
#'
#' @examples
#' a <- tibble(value = c(rnorm(2e2, 1), 4 * rbeta(2e2, 1, 4)),
#'             class = c(rep("a", 2e2), rep("b", 2e2)))
#' raincloud_name(x = a)
#' raincloud_name(x = a, format = "edit", name = "transparent_points")
#' raincloud_adjust(transparent_points)
#'
#' @export
#'
raincloud_name <- function(
    x,
    format = "plot",
    name = "code_output"
) {
  code_output <- quote(
    ggplot(x, aes(x = class, y = value, fill = class, color = class))+
      geom_violin(alpha = 0.25, trim = FALSE, width = 0.85, color = NA)+
      geom_jitter(width = 0.12, alpha = 0.35, size = 1.2, show.legend = FALSE)+
      geom_boxplot(width = 0.16, outlier.shape = NA, fill = "white", alpha = 0.8, color = "black")+
      stat_summary(fun = median, geom = "point", size = 2.2, color = "black")+
      labs(x = NULL, y = NULL)+
      theme_minimal()+
      theme(legend.position = "none")
  )
  if (format == "plot") eval(code_output)
  else if (format == "edit") {
    output <- glue(
      "#Could concievably put an explanation of how to edit the code here \n\n",
      name, "$code <- ", "expr({{",
      system.file("extdata", "raincloud.R", package = "simplex") |>
        readLines() |>
        paste(collapse = "\n"),
      "}})"
    )
    rstudioapi::documentNew(
      text = output,
      type = "r"
    )
    assign(name, list(code = code_output, env = environment()), envir = .GlobalEnv)
  }
  else list(code = code_output, env = environment())
}

#' Raincloud-style distribution plotter. code can be modified before plotting with format edit, and bound in the global.
#'
#' @param x data in the form of a tibble with a column of values named value and column of classes named class.
#' @param format Whether the function should plot, or return an object for editing.
#'
#' @return a ggplot object or a bnd class list containing ggplot code along with a function environment.
#'
#' @examples
#' a <- tibble(value = c(rnorm(2e2, 1), 4 * rbeta(2e2, 1, 4)),
#'             class = c(rep("a", 2e2), rep("b", 2e2)))
#' raincloud_binding(x = a)
#' custom_points <- raincloud_binding(x = a, format = "edit")
#' custom_points
#'
#' @export
#'
raincloud_binding <- function(
    x,
    format = "plot"
) {
  code_output <- quote(
    ggplot(x, aes(x = class, y = value, fill = class, color = class))+
      geom_violin(alpha = 0.25, trim = FALSE, width = 0.85, color = NA)+
      geom_jitter(width = 0.12, alpha = 0.35, size = 1.2, show.legend = FALSE)+
      geom_boxplot(width = 0.16, outlier.shape = NA, fill = "white", alpha = 0.8, color = "black")+
      stat_summary(fun = median, geom = "point", size = 2.2, color = "black")+
      labs(x = NULL, y = NULL)+
      theme_minimal()+
      theme(legend.position = "none")
  )
  if (format == "plot") eval(code_output)
  else if (format == "edit") {
    output <- glue(
      "#Be sure to fill in the space to the left of $code with the object name you are adjusting \n\n",
      "$code <- ", "expr({{",
      system.file("extdata", "raincloud.R", package = "simplex") |>
        readLines() |>
        paste(collapse = "\n"),
      "}})"
    )
    rstudioapi::documentNew(
      text = output,
      type = "r"
    )
    bnd(list(code = code_output, env = environment()))
  }
  else bnd(list(code = code_output, env = environment()))
}

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

app_dir <- file.path("apps", "human_preview_rating_app")
source(file.path(app_dir, "R", "text_helpers.R"))
source(file.path(app_dir, "R", "input_helpers.R"))
source(file.path(app_dir, "R", "app_config.R"))
source(file.path(app_dir, "R", "article_lab_config.R"))
source(file.path(app_dir, "R", "prompt_template_helpers.R"))

expect <- function(value, message) if (!isTRUE(value)) stop(message, call. = FALSE)
expect_error <- function(expr, pattern) {
  error <- tryCatch({ force(expr); NULL }, error = identity)
  expect(inherits(error, "error") && grepl(pattern, conditionMessage(error), perl = TRUE), sprintf("Expected error matching %s.", pattern))
}

template <- paste0("head  \n{{#context}}\nContext:\n{{context}}\n{{/context}}\nmid\n{{#context}}again {{context}}{{/context}}\ntail\t\n")
populated <- article_lab_render_prompt_template(template, list(context = "literal {{unknown}}\nvalue"), "context")
expect(identical(populated, "head  \nContext:\nliteral {{unknown}}\nvalue\nmid\nagain literal {{unknown}}\nvalue\ntail\t\n"), "Populated or repeated blocks were not rendered byte-for-byte.")
empty <- article_lab_render_prompt_template(template, list(context = " \t"), "context")
expect(identical(empty, "head  \nmid\ntail\t\n"), "Empty repeated blocks or surrounding whitespace were rendered incorrectly.")
expect(identical(article_lab_render_prompt_template("A {{value}} Z", list(value = "{{#value}}literal{{/value}}"), "value"), "A {{#value}}literal{{/value}} Z"), "Inserted values were recursively rendered.")

expect_error(article_lab_parse_prompt_template("{{#value}}x", "value"), "Unmatched opening")
expect_error(article_lab_parse_prompt_template("x{{/value}}", "value"), "Unmatched closing")
expect_error(article_lab_parse_prompt_template("{{#value}}x{{/other}}", c("value", "other")), "Mismatched")
expect_error(article_lab_parse_prompt_template("{{#value}}{{#other}}x{{/other}}{{/value}}", c("value", "other")), "Nested")
expect_error(article_lab_parse_prompt_template("{{#value}}x {{unknown}}{{/value}}", "value"), "Unknown")
expect_error(article_lab_parse_prompt_template("{{#value}}x {{bad-name}}{{/value}}", "value"), "Malformed")
expect_error(article_lab_parse_prompt_template("{{#value}}x {{oops}{{/value}}", "value"), "Malformed")

message("Conditional prompt renderer tests passed.")

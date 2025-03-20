# ai stands for repbox AI call. It contains a version object and additional information specific to the AI call
ai_init = function(project="",json_mode=FALSE, schema=NULL, values = NULL, context=NULL, media_files=NULL, tpl=NULL, tpl_file = NULL,   model=ai_opts$model, temperature=ai_opts$temperature, ai_opts=get_ai_opts()) {
  restore.point("ai_init")
  #undebug(to_json_schema)
  schema = to_json_schema_obj(schema)
  if (is.null(tpl) & !is.null(tpl_file)) {
    tpl = merge.lines(suppressWarnings(readLines(tpl_file)))
  } else if (is.null(tpl)) {
    stop("provide a template tpl or tpl_file or haver version$tpl_file filled.")
  }

  ai = list(project = project, tpl=tpl, model=model, schema=schema, json_mode=json_mode, temperature=temperature, media_files=media_files, context=context)
  class(ai) = c("gemini_call","list")
  ai
}


# Only works with rgemini so far
ai_run = function(ai, values=ai$values, verbose=FALSE) {
  restore.point("ai_run")
  if (is.null(ai[["prompt"]]))
    ai = ai_glue(ai, values)

  if (!startsWith(ai$model,"gemini")) {
    stop(paste0("Cannot yet run model ", model))
  }

  library(rgemini)
  ai$media = ai_media(ai$project, ai$media_files, verbose=verbose)

  ai$time_stamp = Sys.time()
  res = run_gemini(ai$prompt, model=ai$model,context=ai$context, response_schema = ai[["schema"]], json_mode=ai$json_mode,temperature = ai$temperature, media = ai$media,detailed_results=TRUE,verbose=FALSE)
  ai[names(res)] = res

  restore.point("ai_run_post")

  ai$run_sec = as.numeric(Sys.time()) - as.numeric(ai$time_stamp)

  if (isTRUE(ai$has_error)) {
    if (verbose)
      cat("\n",ai$err_msg,"\n")
  }
  ai$fine_status = ai_fine_status(ai)
  ai$broad_status = ai_fine_status_to_broad_status(ai$fine_status)
  class(ai) = c("gemini_result","gemini_call", "list")

  return(ai)
}

ai_fine_status = function(ai, status_code = ai$status_code) {
  if (is.null(ai)) return("empty")
  types = c(
    ok = 200,
    error = 400,
    error = 404,
    rate_limit = c(429),
    unavailable = c(503),
    permission = c(403),
    input_limit = c(500),
    timeout = c(504)
  )
  ind = match(status_code, types)
  na_val(names(types[ind]), "unknown")
}

ai_fine_status_to_broad_status = function(fine_status) {
  case_when(
    fine_status %in% c("rate_limit","unavailable", "permission") ~ "outage",
    fine_status %in% c("error", "input_limit", "timeout") ~ "error",
    fine_status == "empty" ~ "empty",
    TRUE ~ "ok"
  )
}


# ai stands for repbox AI call. It contains a version object and additional information specific to the AI call
ai_init = function(project="",json_mode=FALSE, schema=NULL, values = NULL, context=NULL, media_files=NULL, tpl=NULL, tpl_file = NULL,   model=ai_opts$model, temperature=ai_opts$temperature, ai_opts=get_ai_opts()) {
  restore.point("ai_init")
  #undebug(to_json_schema)
  schema = to_json_schema(schema)
  if (is.null(tpl) & !is.null(tpl_file)) {
    tpl = merge.lines(suppressWarnings(readLines(tpl_file)))
  } else if (is.null(tpl)) {
    stop("provide a template tpl or tpl_file or haver version$tpl_file filled.")
  }

  ai = list(project = project, tpl=tpl, model=model, schema=schema, json_mode=version$json_mode, temperature=temperature, media_files=media_files, version=version, context=context)
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
  class(ai) = c("ai","list")

  return(ai)
}

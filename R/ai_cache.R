ai_clear_cache = function() {
  options(aikit_ai_cache = NULL)
}


ai_cache = function(project="") {
  cache = getOption("aikit_ai_cache")
  if (!isTRUE(cache$project == project)) {
    cache = new.env(parent = emptyenv())
    cache$media_cache = new.env(parent=emptyenv())
    cache$data_set_li = list()
    cache$context_cache = new.env(parent = emptyenv())
    cache$project = project
    options(aikit_ai_cache = cache)
  }
  cache
}

# Upload media or return previously uploaded media
ai_media = function(project="", media_files, refresh_sec = 60*45, verbose=TRUE) {
  restore.point("ai_media")
  if (length(media_files)==0)
    return(NULL)
  media = lapply(media_files, function(media_file) ai_single_media(project, media_file, refresh_sec, verbose=verbose))
  media
}

# Upload single media or return previously uploaded media
ai_single_media = function(project, media_file, refresh_sec = 60*45, verbose=TRUE) {
  restore.point("ai_sinle_media")
  mcache = ai_cache(project)$media_cache
  media = mcache[[media_file]]
  outdated = TRUE
  if (!is.null(media)) {
    outdated = isTRUE(as.numeric(Sys.time())-as.numeric(media$time_upload) > refresh_sec)
    if (!outdated) {
      mtime = file_mtime_for_hash(media_file)
      outdated = !identical(mtime, media$local_mtime)
    }
  }
  if (outdated) {
    cat(paste0("\nUpload ", media_file, " to gemini\n"))
    media = rgemini::gemini_media_upload(media_file)
    media$time_upload = Sys.time()
    media$local_file = media_file
    # store mtime to check if local media file has changed...
    media$local_mtime = file_mtime_for_hash(media_file)
    mcache[[media_file]] = media
  }
  media
}

file_mtime_for_hash = function(files) {
  if (length(files)==0) return(NULL)
  mtime = rep(as.POSIXct(NA), length(files))
  exist = file.exists(files)
  mtime[exist] = file.mtime(files[exist])
  mtime
}

# Generate or retrieve previously generated context object
ai_context = function(project="", model=ai_opts$model, media_files =NULL, prompt=NULL, ttl_sec=60*5,  cache_context = isTRUE(ai_opts$cache_context), api_key = getOption("gemini_api_key"), ai_opts = get_ai_opts()) {
  restore.point("ai_context")
  mtime = file_mtime_for_hash(media_file)
  hash = paste0("h",digest::digest(list(project=project, model=model, prompt=prompt, media_files=media_files, mtime=mtime)))
  ccache = ai_cache(project)$context_cache
  context = ccache[[hash]]
  if (!is.null(context) & cache_context) {
    context = gemini_update_context_cache(context, ttl_sec = ttl_sec)
    return(context)
  }
  media = ai_media(project = project,media_files = media_files)
  context = gemini_context(prompt=prompt,model = model,media = media, ttl_sec=ttl_sec,do_cache = cache_context, api_key = api_key)
  context$project = project
  ccache[[hash]] = context
  context
}

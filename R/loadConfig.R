#' @inherit fairify::loadConfig
#' @export
loadConfig <- function(
  userFile = sprintf("config_%s.yml", utils::packageName()),
  pathDefaultCfg = system.file("config.yml", package = utils::packageName())
) {
  fairify::loadConfig(userFile = userFile, pathDefaultCfg = pathDefaultCfg)
}

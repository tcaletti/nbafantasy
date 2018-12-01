#' Calculate DK fantasy points
#'
#' Use a players box score statistics to get their
#' dk fantasy points
#'
#' @param pts Points scored
#' @param rebs Rebounds
#' @param ast Assists
#' @param stl Steals
#' @param blk Blocks
#' @param three 3 pointers made
#' @param tov turnovers
#' @return dk fantasy points
#' @export
dkPoints = function(pts, rebs, ast, stl, blk, three, tov){

  bonus = ifelse(pts >= 10 & rebs >= 10 & ast >= 10, 3,
                 ifelse((pts >= 10 & rebs >= 10) | (pts >= 10 & ast >= 10), 1.5, 0))

  return(pts+rebs*1.25+ast*1.5+stl*2+blk*2+three*.5+bonus-.5*tov)
}

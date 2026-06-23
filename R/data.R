#' fullclean: Data frame of responses to motivations for conducting research
#'
#' Actual data from a study on the motivations for conducting research among Peruvian university professors.
#' The data were collected from several Peruvian universities.
#' 
#' @format Data frame: 589 rows, 19 columns:
#' \describe{
#'   \item{ID}{Subject Identification Number}
#'   \item{EDAD}{Subject's age. Type: numeric}
#'   \item{ESTUDIO}{Educational level. Type: character}
#'   \item{TIEMPODOC}{Teaching experience, in years, categorized. Type: character}
#'   \item{SEXO}{Subject's gender: Male, Female. Type: character}
#'   \item{INV1 ... INV12}{Scale A: Item 1 through INV12. Type: numeric}
#'   \item{INV2 ... INV14}{Scale B: Item 2 through INV14. Type: numeric}
#' }
#'
#' @examples
#' data(fullclean)
#' head(fullclean)
#' summary(fullclean)
#'
#' @name fullclean
#' @docType data
#' @keywords datasets
#' @usage data(fullclean)
NULL
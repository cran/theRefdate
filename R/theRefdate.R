# -*- coding: utf-8 -*-

# Initialisierung: Flavours (Datumsformate) und Paketzustand ----

# Stellen je Flavour: Jahr (von, bis), Monat (von, bis), Tag (von, bis).
# Die alten Kuerzel LP (= dmy) und VK (= iso) bleiben als Alias erhalten.
.stellen <- list(
  dmy       = c( 7L , 10L , 4L , 5L , 1L , 2L ) ,
  iso       = c( 1L ,  4L , 6L , 7L , 9L , 10L ) ,
  iso_basic = c( 1L ,  4L , 5L , 6L , 7L ,  8L ) ,
  LP        = c( 7L , 10L , 4L , 5L , 1L , 2L ) ,
  VK        = c( 1L ,  4L , 6L , 7L , 9L , 10L )
)

# Formatbezeichnung fuer Meldungen
.muster <- c( dmy = "TT.MM.JJJJ" , iso = "JJJJ-MM-TT" , iso_basic = "JJJJMMTT" , LP = "TT.MM.JJJJ" , VK = "JJJJ-MM-TT" )

# Paketzustand: Vorgabe-Flavour. Vorgabe ist das bisherige CRAN-Verhalten (dmy).
.zustand <- new.env( parent = emptyenv() )
.zustand$name    <- "dmy"
.zustand$gesetzt <- FALSE

# Interne Hilfsfunktionen (nicht exportiert) ----

# Bricht mit benannter Fehlerquelle ab: Argument, Klasse, Wert, erwartetes Format.
fehlerArgument <- function( argument , wert , erwartet ) {
  wertText <- if( length( wert ) == 0 ) "<leer>" else paste( utils::head( as.character( wert ) , 3 ) , collapse = ", " )
  stop( sprintf( "theRefdate: Argument '%s' (Klasse '%s', Wert '%s'): %s" ,
                 argument , paste( class( wert ) , collapse = "/" ) , wertText , erwartet ) ,
        call. = FALSE )
}

# Liefert die Stellen zu einem Flavour; unbekannter Flavour ist ein Fehler.
stellenVon <- function( format , argument = "format" ) {
  p <- if( is.character( format ) && length( format ) == 1 ) .stellen[[ format ]]
  if( is.null( p ) ) {
    fehlerArgument( argument , format , "erwartet einen der Werte dmy, iso, iso_basic (alt: LP, VK)" )
  }
  p
}

# Meldung, wenn Stellen keine Zahl ergeben (nur im Fehlerfall aufgerufen).
fehlerKeineZahl <- function( datumstring , argument , format ) {
  fehlerArgument( argument , datumstring , paste0( "enthaelt keine Zahl, erwartet Format " , .muster[[ format ]] ) )
}

# Baut einen Datumsstring im angegebenen Flavour (Jahr vierstellig, Monat/Tag zweistellig).
baueDatum <- function( j , m , t , format ) {
  j <- as.integer( j ); m <- as.integer( m ); t <- as.integer( t )
  switch( format ,
          dmy = , LP = sprintf( "%02d.%02d.%04d" , t , m , j ) ,
          iso = , VK = sprintf( "%04d-%02d-%02d" , j , m , t ) ,
          iso_basic  = sprintf( "%04d%02d%02d" , j , m , t ) )
}

# Exportierte API ----

#' flavour
#'
#' Returns the active flavour (date format) of the package theRefdate.
#'
#' @return character string: \code{"dmy"} (DD.MM.YYYY), \code{"iso"} (YYYY-MM-DD) or
#'   \code{"iso_basic"} (YYYYMMDD). The default is \code{"dmy"}. Version 1.0.0 returned
#'   \code{"LP"} for this format.
#' @export
#'
#' @examples flavour() == "dmy"
flavour <- function() {
  .zustand$name
}

#' setFlavour
#'
#' Sets the flavour (date format) used by all functions of the package as long as
#' they are called without their own argument \code{format}.
#' The setting is valid for the whole R session. Changing it to a different
#' flavour after it has been set once gives a warning.
#' If \code{setFlavour} is never called, the package works in the format
#' DD.MM.YYYY (\code{"dmy"}) and returns the same results as version 1.0.0 for
#' every valid input.
#'
#' @param flavour \code{"dmy"} (DD.MM.YYYY), \code{"iso"} (YYYY-MM-DD) or \code{"iso_basic"}
#'   (YYYYMMDD). The old identifiers \code{"LP"} (= dmy) and \code{"VK"} (= iso) are accepted as well.
#'
#' @return the flavour that was set (invisibly)
#' @export
#'
#' @examples
#' setFlavour( "iso" )
#' jahr( "2002-02-02" ) == 2002
#' setFlavour( "dmy" )
setFlavour <- function( flavour ) {

  stellenVon( flavour , "flavour" )
  name <- if( flavour %in% c( "LP" , "VK" ) ) c( LP = "dmy" , VK = "iso" )[[ flavour ]] else flavour
  if( .zustand$gesetzt && name != .zustand$name ) {
    warning( sprintf( "theRefdate: Flavour wechselt von '%s' auf '%s' innerhalb der Sitzung" , .zustand$name , name ) , call. = FALSE )
  }
  .zustand$name    <- name
  .zustand$gesetzt <- TRUE
  invisible( name )
}

#' Function 'jahr'
#'
#' Extracts the year from a date string as 'numeric'.
#'
#' Invalid input, including \code{NA} and the empty string, is an error (version 1.0.0 returned \code{NA}).
#'
#' @param datumstring date string in the format of the flavour (default "DD.MM.YYYY")
#' @param format flavour; the default is the one set with \code{\link{setFlavour}}
#'
#' @return numeric
#' @export
#'
#' @examples jahr("02.02.2002") == 2002
jahr <- function( datumstring , format = .zustand$name ) {
  p <- if( is.character( format ) ) .stellen[[ format ]]
  if( is.null( p ) ) p <- stellenVon( format )
  wert <- as.numeric( substr( datumstring , p[ 1 ] , p[ 2 ] ) )
  if( anyNA( wert ) ) fehlerKeineZahl( datumstring , "datumstring" , format )
  wert
}

#' Function 'monat'
#'
#' Extracts the month from a date string as 'numeric'.
#'
#' Invalid input, including \code{NA} and the empty string, is an error (version 1.0.0 returned \code{NA}).
#'
#' @param datumstring date string in the format of the flavour (default "DD.MM.YYYY")
#' @param format flavour; the default is the one set with \code{\link{setFlavour}}
#'
#' @return numeric
#' @export
#'
#' @examples monat("02.02.2000") == 2
monat <- function( datumstring , format = .zustand$name ) {
  p <- if( is.character( format ) ) .stellen[[ format ]]
  if( is.null( p ) ) p <- stellenVon( format )
  wert <- as.numeric( substr( datumstring , p[ 3 ] , p[ 4 ] ) )
  if( anyNA( wert ) ) fehlerKeineZahl( datumstring , "datumstring" , format )
  wert
}

#' Function 'tag'
#'
#' Extracts the day from a date string as 'numeric'.
#'
#' Invalid input, including \code{NA} and the empty string, is an error (version 1.0.0 returned \code{NA}).
#'
#' @param datumstring date string in the format of the flavour (default "DD.MM.YYYY")
#' @param format flavour; the default is the one set with \code{\link{setFlavour}}
#'
#' @return numeric
#' @export
#'
#' @examples tag("03.02.2000") == 3
tag <- function( datumstring , format = .zustand$name ) {
  p <- if( is.character( format ) ) .stellen[[ format ]]
  if( is.null( p ) ) p <- stellenVon( format )
  wert <- as.numeric( substr( datumstring , p[ 5 ] , p[ 6 ] ) )
  if( anyNA( wert ) ) fehlerKeineZahl( datumstring , "datumstring" , format )
  wert
}

#' dateDiff
#'
#' Computes the difference between two dates in the format of the flavour.
#' Returns the differences in years, months and total months as a named list.
#'
#' Invalid input, including \code{NA} and the empty string, is an error (version 1.0.0 returned \code{NA}).
#'
#' @param frueher earlier date in the format of the flavour
#' @param spaeter later date in the format of the flavour
#' @param format flavour; the default is the one set with \code{\link{setFlavour}}
#'
#' @return list with diffJ (years), diffM (months), diffDauer (duration in fractional years), diffDauerM (total months)
#' @export
#'
#' @examples dateDiff("01.01.2000", "01.12.1999")$diffDauerM == -1
dateDiff <- function( frueher , spaeter , format = .zustand$name ) {

  p <- if( is.character( format ) ) .stellen[[ format ]]
  if( is.null( p ) ) p <- stellenVon( format )
  jahreFrueher   <- as.numeric( substr( frueher , p[ 1 ] , p[ 2 ] ) )
  jahreSpaeter   <- as.numeric( substr( spaeter , p[ 1 ] , p[ 2 ] ) )
  monateFrueher  <- as.numeric( substr( frueher , p[ 3 ] , p[ 4 ] ) )
  monateSpaeter  <- as.numeric( substr( spaeter , p[ 3 ] , p[ 4 ] ) )
  if( anyNA( c( jahreFrueher , monateFrueher ) ) ) fehlerKeineZahl( frueher , "frueher" , format )
  if( anyNA( c( jahreSpaeter , monateSpaeter ) ) ) fehlerKeineZahl( spaeter , "spaeter" , format )

  jahreDiff  <- jahreSpaeter - jahreFrueher
  monateDiff <- monateSpaeter - monateFrueher

  if(    jahreDiff > 0
      && monateDiff < 0 ) {
    jahreDiff  <- jahreDiff - 1
    monateDiff <- monateDiff + 12
  }
  else
  if(    jahreDiff < 0
      && monateDiff > 0 ) {
    jahreDiff  <- jahreDiff + 1
    monateDiff <- monateDiff - 12
  }

  dauerDiff       <- jahreDiff + monateDiff / 12
  dauerDiffMonate <- jahreDiff * 12 + monateDiff

  return( list( "diffJ" = jahreDiff , "diffM" = monateDiff , "diffDauer" = dauerDiff , "diffDauerM" = dauerDiffMonate ) )
}

#' dateAddMonths
#'
#' Adds or subtracts a number of whole months to or from a date. The day is left unchanged.
#'
#' Invalid input, including \code{NA} and the empty string, is an error (version 1.0.0 returned \code{NA}).
#'
#' @param termin start date in the format of the flavour
#' @param monthsToBeAdded number of whole months (positive: add, negative: subtract)
#' @param format flavour; the default is the one set with \code{\link{setFlavour}}
#'
#' @return date in the format of the flavour
#' @export
#'
#' @examples dateAddMonths("01.01.2000", -3 ) == "01.10.1999"
dateAddMonths <- function( termin , monthsToBeAdded , format = .zustand$name ) {

  if( !is.numeric( monthsToBeAdded ) || length( monthsToBeAdded ) != 1 || is.na( monthsToBeAdded )
      || abs( monthsToBeAdded - round( monthsToBeAdded ) ) > 1e-9 ) {
    fehlerArgument( "monthsToBeAdded" , monthsToBeAdded , "erwartet eine ganze Zahl (numeric, Laenge 1)" )
  }

  p <- if( is.character( format ) ) .stellen[[ format ]]
  if( is.null( p ) ) p <- stellenVon( format )
  jahrAlt  <- as.numeric( substr( termin , p[ 1 ] , p[ 2 ] ) )
  monatAlt <- as.numeric( substr( termin , p[ 3 ] , p[ 4 ] ) )
  tagAlt   <- as.numeric( substr( termin , p[ 5 ] , p[ 6 ] ) )
  if( anyNA( c( jahrAlt , monatAlt , tagAlt ) ) ) fehlerKeineZahl( termin , "termin" , format )

  gesamt  <- jahrAlt * 12 + ( monatAlt - 1 ) + round( monthsToBeAdded )
  jahrNeu <- gesamt %/% 12
  if( jahrNeu < 0 || jahrNeu > 9999 ) {
    fehlerArgument( "monthsToBeAdded" , monthsToBeAdded , paste0( "Ergebnisjahr " , jahrNeu , " liegt ausserhalb 0 bis 9999" ) )
  }

  return( baueDatum( jahrNeu , gesamt %% 12 + 1 , tagAlt , format ) )
}

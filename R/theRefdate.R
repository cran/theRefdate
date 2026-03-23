# -*- coding: utf-8 -*-

# ======================================================================
# Interne Implementierungen (nicht exportiert)
# ======================================================================

#' datumsDifferenz
#'
#' Berechnet die Differenz zwischen zwei Terminen im Format "TT.MM.JJJJ".
#' Gibt eine Liste mit Jahres-, Monats- und Gesamtdifferenz zurueck.
#'
#' @param frueher Datum im Format "TT.MM.JJJJ"
#' @param spaeter Datum im Format "TT.MM.JJJJ"
#'
#' @return list mit diffJ (Jahre), diffM (Monate), diffDauer (Bruchjahre), diffDauerM (Gesamtmonate)
datumsDifferenz <- function( frueher = NA_character_ , spaeter = NA_character_ ) {

  jahreFrueher <- jahr( frueher )
  jahreSpaeter <- jahr( spaeter )
  jahreDiff    <- jahreSpaeter - jahreFrueher

  monateFrueher <- monat( frueher )
  monateSpaeter <- monat( spaeter )
  monateDiff    <- monateSpaeter - monateFrueher

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

#' addiereMonate
#'
#' Addiert oder subtrahiert eine Anzahl Monate zu einem Termin im Format "TT.MM.JJJJ".
#' Verwendet rekursive Jahresschritte fuer Betraege groesser 12.
#'
#' @param termin Ausgangstermin im Format "TT.MM.JJJJ"
#' @param monateHinzu Anzahl Monate (positiv: addieren, negativ: subtrahieren)
#'
#' @return Termin im Format "TT.MM.JJJJ"
addiereMonate <- function( termin = NA_character_ , monateHinzu = NA_real_ ) {

  terminTag <- tag( termin )

  if( terminTag < 10 ) {
    terminTag <- paste( "0" , terminTag , sep="" )
  }

  terminMonat    <- monat( termin )
  terminMonatNeu <- terminMonat + monateHinzu

  # Monatszugabe (monateHinzu > 0):
  if( terminMonatNeu > 0 ) {

    if( terminMonatNeu <= 12 ) {

      if( terminMonatNeu < 10 ) {
        terminMonatNeu <- paste( "0" , terminMonatNeu , sep="" )
      }
      return( paste( terminTag , terminMonatNeu , jahr( termin ) , sep="." ) )

    } else {
      terminJahrNeu <- jahr( termin ) + 1
      # Rekursion: Jahr +1, Restmonate weitergeben.
      # -1 kompensiert den Startmonat "01" im rekursiven Aufruf:
      terminMonatNeu <- terminMonatNeu - 12 - 1
      return( addiereMonate( paste( terminTag , "01" , terminJahrNeu , sep="." ) , terminMonatNeu ) )
    }

  # Monatsabzug (monateHinzu <= 0):
  } else {
    terminJahrNeu <- jahr( termin ) - 1

    if( terminMonat < 10 ) {
      terminMonat <- paste( "0" , as.character( terminMonat ) , sep="" )
    } else {
      terminMonat <- as.character( terminMonat )
    }
    return( addiereMonate( paste( terminTag , terminMonat , terminJahrNeu , sep="." ) , monateHinzu + 12 ) )
  }
}

# ======================================================================
# Exportierte API
# ======================================================================

#' flavour
#'
#' Gibt den Flavour (Auspraegung) des Packages theRefdate zurueck.
#'
#' @return character string, either \code{"LP"} or \code{"VK"}
#' @export
#'
#' @examples flavour() == "LP"
flavour <- function() {
  "LP"
}

#' Funktion 'jahr'
#'
#' Extrahiert die Jahreszahl aus uebergebenem Datumsstring als 'numeric'
#'
#' @param datumstring Datumsstring im Format "TT.MM.JJJJ"
#'
#' @return numeric
#' @export
#'
#' @examples jahr("02.02.2002") == 2002
jahr <- function( datumstring = NA_character_ ) {
  as.numeric( substr( datumstring , 7 , 10 ) )
}

#' Funktion 'monat'
#'
#' Extrahiert die Monatszahl aus uebergebenem Datumsstring als 'numeric'
#'
#' @param datumstring Datumsstring im Format "TT.MM.JJJJ"
#'
#' @return numeric
#' @export
#'
#' @examples monat("02.02.2000") == 2
monat <- function( datumstring = NA_character_ ) {
  as.numeric( substr( datumstring , 4 , 5 ) )
}

#' Funktion 'tag'
#'
#' Extrahiert die Tageszahl aus uebergebenem Datumsstring als 'numeric'
#'
#' @param datumstring Datumsstring im Format "TT.MM.JJJJ"
#'
#' @return numeric
#' @export
#'
#' @examples tag("03.02.2000") == 3
tag <- function( datumstring = NA_character_ ) {
  as.numeric( substr( datumstring , 1 , 2 ) )
}

#' dateDiff
#'
#' Berechnet die Differenz zwischen zwei Terminen im Format "TT.MM.JJJJ".
#' Liefert Jahres-, Monats- und Gesamtdifferenz als benannte Liste.
#'
#' @param frueher Datum im Format "TT.MM.JJJJ" (frueherer Termin)
#' @param spaeter Datum im Format "TT.MM.JJJJ" (spaeterer Termin)
#'
#' @return list mit diffJ (Jahre), diffM (Monate), diffDauer (Bruchjahre), diffDauerM (Gesamtmonate)
#' @export
#'
#' @examples dateDiff("01.01.2000", "01.12.1999")$diffDauerM == -1
dateDiff <- function( frueher = NA_character_ , spaeter = NA_character_ ) datumsDifferenz( frueher , spaeter )

#' dateAddMonths
#'
#' Addiert oder subtrahiert eine Anzahl Monate zu einem Termin im Format "TT.MM.JJJJ".
#'
#' @param termin Ausgangstermin im Format "TT.MM.JJJJ"
#' @param monthsToBeAdded Anzahl Monate (positiv: addieren, negativ: subtrahieren)
#'
#' @return Termin im Format "TT.MM.JJJJ"
#' @export
#'
#' @examples dateAddMonths("01.01.2000", -3 ) == "01.10.1999"
dateAddMonths <- function( termin = NA_character_ , monthsToBeAdded = NA_real_ ) addiereMonate( termin , monthsToBeAdded )

# Hilfsfunktion: Block mit gesetztem Flavour ausfuehren, danach auf dmy zuruecksetzen.
mitFlavour <- function(name, code) {
  suppressWarnings(setFlavour(name))
  on.exit(suppressWarnings(setFlavour("dmy")))
  force(code)
}

# Datumsstring je Flavour aus (Jahr, Monat, Tag)
d <- function(j, m, t, name) {
  switch(name,
    dmy       = sprintf("%02d.%02d.%04d", t, m, j),
    iso       = sprintf("%04d-%02d-%02d", j, m, t),
    iso_basic = sprintf("%04d%02d%02d", j, m, t))
}

test_that("Vorgabe ist dmy, flavour() und Alias", {
  expect_equal(flavour(), "dmy")
  expect_equal(mitFlavour("VK", flavour()), "iso")
  expect_equal(mitFlavour("LP", flavour()), "dmy")
  expect_equal(mitFlavour("iso_basic", flavour()), "iso_basic")
})

test_that("setFlavour warnt beim Wechsel und lehnt Unbekanntes ab", {
  suppressWarnings(setFlavour("dmy"))
  expect_warning(setFlavour("iso"), "wechselt")
  expect_silent(setFlavour("iso"))
  expect_error(setFlavour("xyz"), "Argument 'flavour'")
  expect_error(setFlavour(1), "Klasse 'numeric'")
  suppressWarnings(setFlavour("dmy"))
})

for (fl in c("dmy", "iso", "iso_basic")) {
  test_that(paste("jahr, monat, tag,", fl), {
    mitFlavour(fl, {
      expect_equal(jahr(d(2000, 1, 1, fl)), 2000)
      expect_equal(jahr(d(901, 7, 1, fl)), 901)
      expect_equal(monat(d(1999, 12, 1, fl)), 12)
      expect_equal(monat(d(1789, 6, 14, fl)), 6)
      expect_equal(monat(d(3000, 3, 1, fl)), 3)
      expect_equal(tag(d(2000, 2, 12, fl)), 12)
      expect_equal(tag(d(2000, 1, 1, fl)), 1)
    })
  })

  test_that(paste("dateAddMonths,", fl), {
    mitFlavour(fl, {
      a <- d(2000, 1, 1, fl)
      expect_equal(dateAddMonths(a,  -1), d(1999, 12, 1, fl))
      expect_equal(dateAddMonths(a,   1), d(2000,  2, 1, fl))
      expect_equal(dateAddMonths(a,  13), d(2001,  2, 1, fl))
      expect_equal(dateAddMonths(a, -13), d(1998, 12, 1, fl))
      expect_equal(dateAddMonths(a, -12), d(1999,  1, 1, fl))
      expect_equal(dateAddMonths(a,  12), d(2001,  1, 1, fl))
      expect_equal(dateAddMonths(a, -1201), d(1899, 12, 1, fl))
      expect_equal(dateAddMonths(a,  1201), d(2100,  2, 1, fl))
      expect_equal(dateAddMonths(a, 0), a)
      # Jahr unter 1000 wird vierstellig aufgefuellt
      expect_equal(dateAddMonths(d(1001, 8, 1, fl), -25), d(999, 7, 1, fl))
      # Tag bleibt unveraendert, keine Kappung am Monatsende
      expect_equal(dateAddMonths(d(2000, 1, 31, fl), 1), d(2000, 2, 31, fl))
    })
  })

  test_that(paste("dateDiff,", fl), {
    mitFlavour(fl, {
      x <- dateDiff(d(2000, 1, 1, fl), d(1999, 12, 1, fl))
      expect_equal(x$diffJ, 0)
      expect_equal(x$diffM, -1)
      expect_equal(x$diffDauer, -1/12)
      expect_equal(x$diffDauerM, -1)
      y <- dateDiff(d(1999, 12, 1, fl), d(2000, 1, 1, fl))
      expect_equal(y$diffJ, 0)
      expect_equal(y$diffM, 1)
      expect_equal(y$diffDauer, 1/12)
      expect_equal(y$diffDauerM, 1)
    })
  })
}

test_that("falsche Eingaben nennen das Argument", {
  expect_error(jahr(NA_character_), "Argument 'datumstring'")
  expect_error(jahr("01.01.2000", format = "xyz"), "Argument 'format'")
  expect_equal(jahr("2000-01-01", format = "iso"), 2000)
  expect_equal(dateAddMonths("20000101", 1, format = "iso_basic"), "20000201")
  expect_error(suppressWarnings(monat("2000-01-01")), "Argument 'datumstring'.*TT.MM.JJJJ")
  expect_error(dateAddMonths("2000", 1), "Argument 'termin'")
  expect_error(dateAddMonths("01.01.2000", NA), "Argument 'monthsToBeAdded'")
  expect_error(dateAddMonths("01.01.2000", 1.5), "ganze Zahl")
  expect_error(dateDiff("01.01.2000", "abc"), "Argument 'spaeter'")
  mitFlavour("iso", expect_error(suppressWarnings(jahr("abcdefghij")), "JJJJ-MM-TT"))
  mitFlavour("iso_basic", expect_error(suppressWarnings(tag("2000-01-01")), "JJJJMMTT"))
})

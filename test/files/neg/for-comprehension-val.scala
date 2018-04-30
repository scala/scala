class A {
  for (x <- 1 to 5 ; y = x) yield x+y           // ok
  for (x <- 1 to 5 ; val y = x) yield x+y       // fail
  for (val x <- 1 to 5 ; y = x) yield x+y       // fail
  for (val x <- 1 to 5 ; val y = x) yield x+y   // fail

  for (z <- 1 to 2 ; x <- 1 to 5 ; y = x) yield x+y           // ok
  for (z <- 1 to 2 ; x <- 1 to 5 ; val y = x) yield x+y       // fail
  for (z <- 1 to 2 ; val x <- 1 to 5 ; y = x) yield x+y       // fail
  for (z <- 1 to 2 ; val x <- 1 to 5 ; val y = x) yield x+y   // fail
}

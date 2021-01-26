object X {
  for {
    x <- 1 to 5 if true if true
  } yield x

  for {
    x <- 1 to 5 if true
    if true
  } yield x

  for {
    x <- 1 to 5
    if true if true
  } yield x

  for {
    x <- 1 to 5
    if true ; if true
  } yield x

  for {
    x <- 1 to 5
    if true
    if true
  } yield x
}

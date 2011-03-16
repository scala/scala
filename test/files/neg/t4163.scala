class Bug {
  val z = (
      for {
        x = 3
        y <- 0 to 100
      } yield y
    ).toArray
}

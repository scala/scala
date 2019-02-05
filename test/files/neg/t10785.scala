object t10785 {
  implicit def safeInt(x: Int): Either[String, Int] = Right(x)

  for {
    x <- Left("error")
    y <- 1 + x
  } yield x
}

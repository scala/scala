
trait t10785 {
  implicit def safeInt(x: Int): Either[String, Int] = Right(x)

  for (x <- Left[String, Int]("") ; y <- 1 + x) yield x
  for (x <- Left("") ; y <- 1 + x) yield x

  //Left[String, Int]("error").flatMap(x => 1.+(x).map(y => x))
  //Left("error").flatMap(x => 1.$plus(x).map(y => x))
  //Left("error").flatMap(x => 1.+(x).map(y => x))
}

package test {
  object NotNoPrefix {
    final class Id[A](val a: A) extends AnyVal
    final class Ids[A](val as: Seq[A]) extends AnyVal
    final class Bid[A, B](val ab: Map[A, B]) extends AnyVal
  }
}

object Test extends App {
  import test.NotNoPrefix._

  println(classOf[Id[Int]])
  println(classOf[Id[_]])

  println(classOf[Ids[Int]])
  println(classOf[Ids[_]])

  println(classOf[Bid[Int, Int]])
  println(classOf[Bid[Int, _]])
  println(classOf[Bid[_, Int]])
  println(classOf[Bid[_, _]])

  type Iddy[A] = Id[A]
  type Idsy[A] = Ids[A]
  type Biddy[A, B] = Bid[A, B]
  type Biddouble[A] = Bid[A, Double]
  type Bixt[L] = Biddouble[_]
  type Bixty = Bixt[_]

  println(classOf[Iddy[Int]])
  println(classOf[Iddy[_]])

  println(classOf[Idsy[Int]])
  println(classOf[Idsy[_]])

  println(classOf[Biddy[Int, Int]])
  println(classOf[Biddy[Int, _]])
  println(classOf[Biddy[_, Int]])
  println(classOf[Biddy[_, _]])

  println(classOf[Biddouble[Int]])
  println(classOf[Biddouble[_]])

  println(classOf[Bixt[Int]])
  println(classOf[Bixt[_]])

  println(classOf[Bixty])
}
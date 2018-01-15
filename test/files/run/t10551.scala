package test {
  object NotNoPrefix {
    final class Id[A](val a: A) extends AnyVal
    final class Ids[A](val as: Seq[A]) extends AnyVal
    final class Bid[A, B](val ab: Map[A, B]) extends AnyVal
  }
}

object Test extends App {
  import test.NotNoPrefix._
  
  def check[A](cls: Class[A])(implicit tag: reflect.ClassTag[A]): Unit = {
    val suffix = if (cls != tag.runtimeClass) " != " + tag.runtimeClass else ""
    println(cls + suffix)
  }

  check(classOf[Id[Int]])
  check(classOf[Id[_]])

  check(classOf[Ids[Int]])
  check(classOf[Ids[_]])

  check(classOf[Bid[Int, Int]])
  check(classOf[Bid[Int, _]])
  check(classOf[Bid[_, Int]])
  check(classOf[Bid[_, _]])

  type Iddy[A] = Id[A]
  type Idsy[A] = Ids[A]
  type Biddy[A, B] = Bid[A, B]
  type Biddouble[A] = Bid[A, Double]
  type Bixt[L] = Biddouble[_]
  type Bixty = Bixt[_]

  check(classOf[Iddy[Int]])
  check(classOf[Iddy[_]])

  check(classOf[Idsy[Int]])
  check(classOf[Idsy[_]])

  check(classOf[Biddy[Int, Int]])
  check(classOf[Biddy[Int, _]])
  check(classOf[Biddy[_, Int]])
  check(classOf[Biddy[_, _]])

  check(classOf[Biddouble[Int]])
  check(classOf[Biddouble[_]])

  check(classOf[Bixt[Int]])
  check(classOf[Bixt[_]])

  check(classOf[Bixty])
}
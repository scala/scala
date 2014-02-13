import scala.language.implicitConversions

object Test {
  def main(args: Array[String]): Unit = {
    Moop1.ob1
    Moop1.ob2
    Moop1.ob3
    Moop2.ob1
    Moop2.ob2
    Moop2.ob3
    Moop3.ob1
    Moop3.ob2
    Moop3.ob3
  }
}

// int object vs.
object Moop1 {
  object ob1 {
    implicit object f1 extends (Int => String) { def apply(x: Int): String = "Int" }
    implicit object f2 extends (Long => String) { def apply(x: Long): String = "Long" }

    println(5: String)
  }
  object ob2 {
    implicit object f1 extends (Int => String) { def apply(x: Int): String = "Int" }
    implicit def f2(x: Long): String = "Long"

    println(5: String)
  }
  object ob3 {
    implicit object f1 extends (Int => String) { def apply(x: Int): String = "Int" }
    implicit val f2: Long => String = _ => "Long"

    println(5: String)
  }
}

// int def vs.
object Moop2 {
  object ob1 {
    implicit def f1(x: Int): String = "Int"
    implicit object f2 extends (Long => String) { def apply(x: Long): String = "Long" }

    println(5: String)
  }
  object ob2 {
    implicit def f1(x: Int): String = "Int"
    implicit def f2(x: Long): String = "Long"

    println(5: String)
  }
  object ob3 {
    implicit def f1(x: Int): String = "Int"
    implicit val f2: Long => String = _ => "Long"

    println(5: String)
  }
}

// int val vs.
object Moop3 {
  object ob1 {
    implicit val f1: Int => String  = _ => "Int"
    implicit object f2 extends (Long => String) { def apply(x: Long): String = "Long" }

    println(5: String)
  }
  object ob2 {
    implicit val f1: Int => String  = _ => "Int"
    implicit def f2(x: Long): String = "Long"

    println(5: String)
  }
  object ob3 {
    implicit val f1: Int => String  = _ => "Int"
    implicit val f2: Long => String = _ => "Long"

    println(5: String)
  }
}


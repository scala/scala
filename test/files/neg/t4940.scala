//> using options -Werror -Xlint
class C {
  val f: PartialFunction[String, Int] = (x: Int) => x match { case "x" => 3 }   // error

  val g: PartialFunction[String, Int] = (x: X) => x match { case "x" => 3 }     // error

  val m: PartialFunction[Int, Int] = (x: Double) => x match { case 3.14 => 3 }  // error

  val g3: PartialFunction[Y, Int] = (x: X) => x match { case _: X => 3 }        // error
}

class Y
class X extends Y

object Test extends App {
  val c = new C
  println(c.f.applyOrElse("hello, world", (s: String) => -1))
  println(c.f.applyOrElse("x", (s: String) => -1))
  println(c.g.applyOrElse("hello, world", (s: String) => -1))
  println(c.m.applyOrElse(42, (n: Int) => -1))
}

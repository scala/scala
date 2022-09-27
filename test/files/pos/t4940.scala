
class C {
  val f: PartialFunction[String, Int] = (x: String) => x match { case "x" => 3 }

  val g: PartialFunction[X, Int] = (x: X) => x match { case X(i) => i }

  val m: PartialFunction[Double, Int] = (x: Double) => x match { case 3.14 => 3 }
}

class D {
  val f: PartialFunction[String, Int] = _ match { case "x" => 3 }

  val g: PartialFunction[X, Int] = _ match { case X(i) => i }

  val m: PartialFunction[Double, Int] = _ match { case 3.14 => 3 }
}

case class X(x: Int)

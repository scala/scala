//> using options -Werror -Xlint
class C {
  val f: PartialFunction[String, Int] = (x: String) => x match { case "x" => 3 }
  val f2: PartialFunction[String, Int] = (x: String) => x match { case "x" => x.toString.toInt }

  val g: PartialFunction[X, Int] = (x: X) => x match { case X(i) => i }
  val g2: PartialFunction[X, Int] = (x: Y) => x match { case X(i) => i }
  //val g3: PartialFunction[Y, Int] = (x: X) => x match { case X(i) => i }

  val m: PartialFunction[Double, Int] = (x: Double) => x match { case 3.14 => 3 }
}

class D {
  val f: PartialFunction[String, Int] = _ match { case "x" => 3 }

  val g: PartialFunction[X, Int] = _ match { case X(i) => i }

  val m: PartialFunction[Double, Int] = _ match { case 3.14 => 3 }
}

class E {
  val f: PartialFunction[String, Int] = x => x.toInt

  val g: PartialFunction[X, Int] = x => x.x

  val m: PartialFunction[Double, Long] = d => d.round
}

trait Y
case class X(x: Int) extends Y

class ActuallyOK {
  val map = Map(42 -> "foo")
  def k = List(27).collect {
    map.get(_) match {
      case Some(i) => i
    }
  }
}

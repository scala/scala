
class C {
  val f: PartialFunction[String, Int] = (x: Int) => x match { case "x" => 3 }   // error

  val g: PartialFunction[String, Int] = (x: X) => x match { case "x" => 3 }     // error

  val m: PartialFunction[Int, Int] = (x: Double) => x match { case 3.14 => 3 }  // error
}

class X

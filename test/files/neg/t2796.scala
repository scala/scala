trait Base {
  val abstractVal: String
  final val useAbstractVal = abstractVal
}

trait T1 extends {
  val abstractVal = "T1.abstractVal" // warn
} with Base

trait T2 extends {
  type X = Int                       // warn
} with Base

class C1 extends {
  val abstractVal = "C1.abstractVal" // okay
} with Base

object Test {
  def main(args: Array[String]) {
    assert(new C1 ().useAbstractVal == "C1.abstractVal")
    // This currently fails. a more ambitious approach to this ticket would add $earlyinit$
    // to traits and call it from the right places in the right order.
    //
    // For now, we'll just issue a warning.
    assert(new T1 {}.useAbstractVal == "T1.abstractVal")
  }
}

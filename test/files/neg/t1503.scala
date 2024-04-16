//> using options -Xlint -Werror -Wvalue-discard
//
object Whatever {
  override def equals(x: Any) = true
}

class Test {
  def matchWhateverCCE(x: Any) = x match { case n @ Whatever => n }   // used to warn

  val a: Nil.type = (Vector(): Any) match { case n @ Nil => n }       // error

  val b: Nil.type = (Vector(): Any) match { case n @ (m @ Nil) => n } // error was: CCE

  //val c = List(42) match { case xs @ (ys @ _*) => xs }              // syntax error in parser

  // numeric value classes compare equals betwixt themselves

  val d: Int = (1.0: Any) match { case x @ 1 => x }                   // error

  val e: Int = (1.0: Any) match { case x @ (_: 1) => x }              // error was: CCE

  // edge case, Boolean and Unit only equal themselves

  val f: Boolean  = (true: Any) match { case b @ true => b }
  val f2: Boolean = (true: Any) match { case b @ (_: true) => b }
  val f3: true    = (true: Any) match { case b @ (_: true) => b }

  def g(): Unit  = ((): Any) match { case u @ () => u }
  def g2(): Unit = ((): Any) match { case u @ (_: Unit) => u }        // no value discard

  def h(x: Any): String = x match { case s @ "hello, world" => s }
  def h2(x: Any): String = x match { case s @ (_: "hello, world") => s }
  //def h3(x: Any): "hello, world" = x match { case s @ "hello, world" => s }  // crash

  //def j(x: Any): Array[Int] = x match { case xs @ Array(42) => xs } // found Array[T] required Array[Int]
}

// scalac: -Xlint -Werror
//
object Whatever {
  override def equals(x: Any) = true
}

class Test {
  def matchWhateverCCE(x: Any) = x match { case n @ Whatever => n }   // used to warn

  val a: Nil.type = (Vector(): Any) match { case n @ Nil => n }       // error

  val b: Nil.type = (Vector(): Any) match { case n @ (m @ Nil) => n } // error was: CCE

  //val c = List(42) match { case xs @ (ys @ _*) => xs }              // syntax error

  val d: Int = (1.0: Any) match { case x @ 1 => x }                   // error

  val e: Int = (1.0: Any) match { case x @ (_: 1) => x }              // error CCE
} 

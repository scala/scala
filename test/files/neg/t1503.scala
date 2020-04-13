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
} 

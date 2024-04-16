//
//> using options -Werror -opt:inline:** -Wopt:none,_
//
// compare -opt-warnings:none,at-inline-failed-summary

trait Try

object Try {
  def apply(s: String): Try = Success(s)
}

case class Success(s: String) extends Try
case class Failure(e: Throwable) extends Try

class C {
  private def get(a: String): Unit = Try(a) match {
    case Failure(e: Exception) => 
    case Failure(e) => println(e.toString)
  }
}

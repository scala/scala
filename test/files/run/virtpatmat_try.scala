object Test extends App {
  case class A(val x: String) extends Throwable
  class B extends Exception { override def toString = "B" }
  def bla = 0

  try {
    throw new A("meh")
  } catch { // this should emit a "catch-switch"
    case y: A => println(y.x)
    case (_ : A | _ : B)  => println("B")
    case _: Throwable => println("other")
  }

  try {
    throw new B()
  } catch { // case classes and alternative flattening aren't supported yet, but could be in principle
    // case A(x) => println(x)
    case y: A => println(y.x)
    case x@((_ : A) | (_ : B))  => println(x)
    case _: Throwable => println("other")
  }

 def simpleTry: Unit = {
    try {
      bla
    } catch {
      case x: Exception if x.getMessage == "test" => println("first case " + x)
      case x: Exception => println("second case " + x)
    }
  }

  def typedWildcardTry: Unit = {
    try { bla } catch { case _: ClassCastException => bla }
  }

  def wildcardTry: Unit = {
    try { bla } catch { case _: Throwable => bla }
  }

  def tryPlusFinally: Unit = {
    try { bla } finally { println("finally") }
  }

  def catchAndPassToLambda: Unit = {
    try { bla } catch { case ex: Exception => val f = () => ex }
  }
}

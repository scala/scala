
trait T {
  final val x = 42
  def f = {
    import x._
    val g: Int => Int = +
    g(1)
  }
}
/*
 *
test/files/neg/t8855.scala:5: error: stable identifier required, but 42 found.
    import x._
           ^
Exception in thread "main" java.lang.NullPointerException
	at scala.tools.nsc.typechecker.Contexts.registerImport(Contexts.scala:93)
 */

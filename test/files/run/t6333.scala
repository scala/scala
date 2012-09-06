object Test extends App {
  import util.Try

  val a = "apple"
  def fail: String = throw new Exception("Fail!")
  def argh: Try[String] = throw new Exception("Argh!")

  // No throw tests
  def tryMethods(expr: => String): Unit = {
    Try(expr) orElse argh
    Try(expr).transform(_ => argh, _ => argh)
    Try(expr).recoverWith { case e if (a == fail) => Try(a) }
    Try(expr).recoverWith { case _ => argh }
    Try(expr).getOrElse(a)
    // TODO - Fail getOrElse?
    Try(expr) orElse argh
    Try(expr) orElse Try(a)
    Try(expr) map (_ => fail)
    Try(expr) map (_ => a)
    Try(expr) flatMap (_ => argh)
    Try(expr) flatMap (_ => Try(a))
    Try(expr) filter (_ => throw new Exception("O NOES"))
    Try(expr) filter (_ => true)
    Try(expr) recover { case _ => fail }
    Try(expr).failed
  }
  tryMethods(a)
  tryMethods(fail)
}

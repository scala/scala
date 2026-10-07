//> using options -Wunused:-patvars,_ -Werror

// verify NO warning when -Wunused:-patvars

case class C(a: Int, b: String, c: Option[String])
case class D(a: Int)

trait Boundings {

  private val x = 42                      // warn, to ensure that warnings are enabled

  def c = C(42, "hello", Some("world"))
  def d = D(42)

  def f() = {
    val C(x, y, Some(z)) = c              // no warn
    17
  }
  def g() = {
    val C(x @ _, y @ _, Some(z @ _)) = c  // no warn
    17
  }
  def h() = {
    val C(x @ _, y @ _, z @ Some(_)) = c  // no warn for z?
    17
  }

  def v() = {
    val D(x) = d                          // no warn
    17
  }
  def w() = {
    val D(x @ _) = d                      // no warn
    17
  }

}

trait Forever {
  def f = {
    val t = Option((17, 42))
    for {
      ns <- t
      (i, j) = ns                        // no warn
    } yield (i + j)
  }
  def g = {
    val t = Option((17, 42))
    for {
      ns <- t
      (i, j) = ns                        // no warn
    } yield 42
  }
}

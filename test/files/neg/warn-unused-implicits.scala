
trait InterFace {
  /** Call something. */
  def call(a: Int, b: String, c: Double)(implicit s: String): Int
}

trait BadAPI extends InterFace {
  def f(a: Int,
        b: String,
        c: Double
       )(implicit s: String): Int = {  // warn
    println(b + c)
    a
  }
  @deprecated ("no warn in deprecated API", since="yesterday")
  def g(a: Int,
        b: String,
        c: Double
       )(implicit s: String): Int = {  // no warn
    println(b + c)
    a
  }
  override def call(a: Int,
                    b: String,
                    c: Double
                   )(implicit s: String): Int = {  // no warn, required by superclass
    println(b + c)
    a
  }

  def i(implicit s: String, t: Int) = t           // yes, warn
}

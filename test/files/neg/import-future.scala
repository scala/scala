// scalac: -Xsource:3
//

class D {
  def *(y: Int): Int = y
  def unrelated(y: Int): Int = y
}

object Test {
  val d = new D

  def one: Int = {
    import d.`*`

    unrelated(1) // error

    *(1)
  }

  def two: Int = {
    import d.*

    unrelated(1)

    *(1)
  }
}

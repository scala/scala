// A bunch of ridiculous seeming tests until you realize much
// of this didn't work until the commit which accompanies this.
object Test {
  def f1(x: AnyRef with Singleton): AnyRef with Singleton = x
  def f2[T <: AnyRef with Singleton](x: T): T = x

  val x1: AnyRef with Singleton = "abc"
  val x2 = "def"
  final val x3 = "ghi"
  val x4: String = "jkl"

  // compiles...
  def narrow1(x: AnyRef): AnyRef with Singleton = x

  // compiles, still doesn't help.
  def narrow2(x: AnyRef): AnyRef with Singleton = x.asInstanceOf[x.type]

  // fails, wait, what? This fails and narrow1 compiles?
  def narrow3(x: AnyRef): AnyRef with Singleton = x.asInstanceOf[AnyRef with Singleton]

  // ok
  def narrow4[T <: AnyRef](x: T): AnyRef with Singleton = x

  object imp {
    implicit def narrow4[T <: AnyRef](x: T): AnyRef with Singleton = x
    val x5: String = "mno"
    def imp1 = f1(x5)

    // f2(x5)   // doesn't work but I think it should
    def imp2 = f2(narrow4(x5))
  }

  def main(args: Array[String]): Unit = {
    // compiles
    f1(x1)
    f1(x2)
    f1(x3)
    f1(x4)

    f2(x1)
    // f2(x2)
    // f2(x3)   // maybe this one should work
    // f2(x4)

    f1(narrow1(x4))
    f1(narrow2(x4))
    f1(narrow3(x4))
    f1(narrow4(x4))
    f2(narrow1(x4))
    f2(narrow2(x4))
    f2(narrow3(x4))
    f2(narrow4(x4))
  }
}


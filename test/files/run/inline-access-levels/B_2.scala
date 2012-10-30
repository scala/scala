package test

object B {

  private var xvar: Int = 0
  private val xval: Int = 0
  private[this] val xval2: Int = 0
  private def xdef: Int = 0

  @inline def actOnX(f: Int => Int) = {
    xvar = f(xvar + xval + xval2 + xdef)
  }
}

final class B {
  private var xvar: Int = 0
  private val xval: Int = 0
  private[this] val xval2: Int = 0
  private def xdef: Int = 0

  @inline def actOnX(f: Int => Int) = {
    B.xvar = f(B.xvar + B.xval + B.xdef)

    // this fails now that we don't publicise at SuperAccessors.
    // Inliners generates code that calls x$var
    // java.lang.NoSuchMethodError: test.B$.xvar()I
    xvar = 0


    // all okay, private[X] is already !isPrivate.
    f(A.packagePrivate)
    val a = new A
    a.packagePrivateClass
  }
}

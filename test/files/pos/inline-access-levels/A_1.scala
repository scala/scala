package test

object A {

  private var xvar: Int = 0
  private val xval: Int = 0
  private[this] val xval2: Int = 0
  private def xdef: Int = 0

  @inline def actOnX(f: Int => Int) = {
    xvar = f(xvar + xval + xval2 + xdef)
  }
}

final class A {
	private var xvar: Int = 0
  private val xval: Int = 0
  private[this] val xval2: Int = 0
  private def xdef: Int = 0

	@inline def actOnX(f: Int => Int) = {
    A.xvar = f(A.xvar + A.xval + A.xdef)
    xvar = f(xvar)
  }
}

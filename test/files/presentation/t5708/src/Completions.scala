package test

object Compat {
  final val CONST_STRING = "constant"
  lazy val foo = 4

  private val privateV = ""
  private[test] val pkgPrivateV = ""
  protected val protectedV = ""

  private def privateM = ""
  private[test] def pkgPrivateM = ""
  protected def protectedValM = ""
}

class Foo {
  Compat./*!*/CONST_STRING // its 'accessible' flag is false
}

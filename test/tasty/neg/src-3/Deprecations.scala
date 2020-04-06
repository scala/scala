package tastytest

object Deprecations {

  final val msg = "This will be removed"
  final val since = "2011"
  final val sinceForever = true

  class Old {

    @deprecated(msg, since)
    def unused(): Unit = ()

    @deprecated("this is useless", since = if (Deprecations.sinceForever) "forever" else "never")
    def completelyUseless(): Unit = ()

    @deprecated(msg, 25 match {
      case n if n > 50 => "big"
      case _           => "small"
    })
    def matchAnnot(): Unit = ()

    @deprecated(msg, {class Foo() { def since = "1923"}; new Foo().since })
    def classAnnot(): Unit = ()

  }

}

package tastytest

object Deprecations {

  final val msg = "This will be removed"
  final val since = "2011"

  class Old {
    @deprecated(msg, since)
    def unused(): Unit = ()

    @deprecated("this is useless", if (true) "forever" else "never")
    def completelyUseless(): Unit = ()
  }

}

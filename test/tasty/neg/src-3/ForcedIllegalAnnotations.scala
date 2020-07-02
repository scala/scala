package tastytest

object ForcedIllegalAnnotations {

  final val msg = "This will be removed"

  class Match {

    @deprecated(msg, 25 match {
      case n if n > 50 => "big"
      case _           => "small"
    })
    def forcedMatchInAnnot(): Unit = ()

  }

  class Block {
    @deprecated(msg, {class Foo() { def since = "1923"}; new Foo().since })
    def forcedBlockInAnnot(): Unit = ()
  }

}

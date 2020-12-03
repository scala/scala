package tastytest

object Infix {

  final case class BoxedInt(toInt: Int) {

    infix def min (that: BoxedInt): BoxedInt =
      if toInt.min(that.toInt) == toInt then this
      else that

  }

}

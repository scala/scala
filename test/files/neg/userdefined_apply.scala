object ClashOverloadNoSig {
  // error: overloaded method apply needs result type
  private def apply(x: Int) = if (x > 0) new ClashOverloadNoSig(x) else apply("")

  def apply(x: String): ClashOverloadNoSig = ???
}

case class ClashOverloadNoSig private(x: Int)

object ClashRecNoSig {
  // error: recursive method apply needs result type
  private def apply(x: Int) = if (x > 0) ClashRecNoSig(1) else ???
}

case class ClashRecNoSig private(x: Int)

object NoClashNoSig {
  // error: overloaded method apply needs result type
  private def apply(x: Boolean) = if (x) NoClashNoSig(1) else ???
}

case class NoClashNoSig private(x: Int)

object NoClashOverload {
  // error: overloaded method apply needs result type
  private def apply(x: Boolean) = if (x) NoClashOverload(1) else apply("")

  def apply(x: String): NoClashOverload = ???
}

case class NoClashOverload private(x: Int)

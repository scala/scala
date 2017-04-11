object ClashOverloadNoSig {
  // error: overloaded method apply needs result type
  private def apply(x: Int) = if (x > 0) new ClashOverloadNoSig(x) else apply("")

  def apply(x: String): ClashOverloadNoSig = ???
}

case class ClashOverloadNoSig private(x: Int)

object ClashRecNoSig {
  // TODO: status quo is that the error refers to an overloaded method, which is actually recursive
  // (we should have unlinked the symbol in the `if(suppress)` part of `applyUnapplyMethodCompleter`)
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


class BaseNCNSP[T] {
  // TODO: suppress the following error
  // error: NoClashNoSigPoly.type does not take parameters
  def apply(x: T) = if (???) NoClashNoSigPoly(1) else ???
}

object NoClashNoSigPoly extends BaseNCNSP[Boolean]
// TODO: position error at definition of apply in superclass instead of on case clss
// error: recursive method apply needs result type
case class NoClashNoSigPoly private(x: Int)


class BaseCNSP[T] {
  // TODO: suppress the following error
  // error: ClashNoSigPoly.type does not take parameters
  def apply(x: T) = if (???) ClashNoSigPoly(1) else ???
}

object ClashNoSigPoly extends BaseCNSP[Int]
// TODO: position error at definition of apply in superclass instead of on case clss
// error: recursive method apply needs result type
case class ClashNoSigPoly private(x: Int)

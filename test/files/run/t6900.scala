import annotation.tailrec

trait Universe {
  type T <: AnyRef
}

final class Bug {
  var i = 1
  def stop() = { i -= 1; i < 0 }
  // the alias bypasses the fast path in erasures InfoTransformer
  // predicated on `TypeMap.noChangeToSymbols`
  type Alias = Any

  @tailrec
  // So we get two symbols for `universe`, the original on the ValDef
  // and a clone in the MethodType of `f`.
  def f(universe: Universe, l: Alias): universe.T = {
    if (stop()) null.asInstanceOf[universe.T] else f(universe, null)
  }

  @tailrec
  def g(universe: Universe)(l: Alias): universe.T = {
    if (stop()) null.asInstanceOf[universe.T] else g(universe)(l)
  }

  @tailrec
  def h(universe: Universe)(l: List[universe.T]): List[universe.T] = {
    if (stop()) Nil else h(universe)(l)
  }
}

object Test extends App {
  assert(new Bug().f(null, null) == null)
  assert(new Bug().g(null)(null) == null)
  assert(new Bug().h(null)(null) == Nil)
}
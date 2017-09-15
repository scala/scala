abstract class Test {
  trait Symbol
  trait Type

  @noinline
  private final def javaSig0(sym0: Symbol, info: Type, markClassUsed: Symbol => Unit): Option[String] = {
    def boxedSig(tp: Type): Unit = jsig(tp, primitiveOK = false)

    @noinline
    def jsig(tp0: Type, existentiallyBound: List[Symbol] = Nil, toplevel: Boolean = false, primitiveOK: Boolean = true): Unit = ()

    None
  }
}

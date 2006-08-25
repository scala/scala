import scala.tools.nsc.transform.Transform

abstract class Detach extends Transform {
  import global._
  class DetachTransformer extends Transformer {
    def foo: Scope = newScope(
      List[Symbol]() map { sym =>
        val newAcc = sym.cloneSymbol
        // def setFlag(mask: long): this.type
        newAcc.setFlag(0)
      }
    )
  }
}

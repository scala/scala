package scala.reflect
package internal
package transform

trait Transforms { self: SymbolTable =>

  object refChecks extends { val global: Transforms.this.type = self } with RefChecks
  object uncurry   extends { val global: Transforms.this.type = self } with UnCurry
  object erasure   extends { val global: Transforms.this.type = self } with Erasure

  def javaType(sym: Symbol) =
    erasure.transformInfo(sym,
      uncurry.transformInfo(sym,
        refChecks.transformInfo(sym, sym.info)))

}
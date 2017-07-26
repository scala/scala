package scala.tools.nsc
package backend.jvm

abstract class CodeGen[G <: Global](val global: G) {
  import global._
  val bTypes: BTypesFromSymbols[global.type]

  object CodeGenImpl extends {
    val global: CodeGen.this.global.type = CodeGen.this.global
    val bTypes: CodeGen.this.bTypes.type = CodeGen.this.bTypes
  } with BCodeSyncAndTry
}

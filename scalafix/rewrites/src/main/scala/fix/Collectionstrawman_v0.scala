package fix

import scalafix._
import scala.meta._

case class Collectionstrawman_v0(mirror: Mirror)
    extends SemanticRewrite(mirror) {
  val immutableListSymbol = Symbol("_root_.scala.collection.immutable.List.")
  def rewrite(ctx: RewriteCtx): Patch = {
    ctx.tree.collect {
      case name @ Term.Name(_) if name.symbol == immutableListSymbol =>
        ctx.addGlobalImport(importer"scala.{List => _}") +
          ctx.addGlobalImport(importer"strawman.collection.immutable.List")
    }.asPatch
  }
}

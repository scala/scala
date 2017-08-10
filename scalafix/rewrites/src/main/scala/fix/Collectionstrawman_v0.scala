package fix
// scalafmt: { maxColumn = 120 }

import scalafix._
import scalafix.syntax._
import scalafix.internal.util.SymbolOps
import scala.meta._

case class Collectionstrawman_v0(mirror: SemanticCtx) extends SemanticRewrite(mirror) {
  val immutableListSymbol = Symbol("_root_.scala.collection.immutable.List.")
  val unimports = Set(
    Symbol("_root_.scala.List."),
    Symbol("_root_.scala.Seq."),
    Symbol("_root_.scala.Vector."),
    Symbol("_root_.scala.`::`."),
    Symbol("_root_.scala.`#::`."),
    Symbol("_root_.scala.Predef.Map."),
    Symbol("_root_.scala.Predef.augmentStrugming."),
    Symbol("_root_.scala.Predef.intArrayOps.")
  )

  def ifSymbolFound(ctx: RewriteCtx): Patch = {
    val toUnimport = ctx.mirror.database.names.flatMap { r =>
      val norm = r.sym.normalized
      if (unimports.contains(norm)) norm :: Nil
      else Nil
    }
    val unimportss = toUnimport.toList.distinct.flatMap { sym =>
      SymbolOps.toImporter(sym).toList.collect {
        case Importer(qual, Importee.Name(n) :: Nil) =>
          Importer(qual, Importee.Unimport(n) :: Nil)
      }
    }
    val grouped = unimportss.groupBy(_.ref.syntax).collect {
      case (qual, iss @ is :: _) =>
        val names = iss.collect { case Importer(_, name :: Nil) => name }
        Importer(is.ref, names)
    }
    grouped.map(ctx.addGlobalImport(_)).asPatch
  }

  def rewrite(ctx: RewriteCtx): Patch = {
    def p(name: String) =
      s"scala.Predef.$name" -> s"strawman.collection.immutable.$name"
    def s(name: String) =
      s"scala.$name" -> s"strawman.collection.immutable.$name"
    def i(name: String) =
      s"scala.collection.immutable.$name" -> s"strawman.collection.immutable.$name"
    def m(name: String) =
      s"scala.collection.mutable.$name" -> s"strawman.collection.mutable.$name"
//    ifSymbolFound(ctx) +
    ctx.replaceSymbols(
      i("HashMap"),
      i("Map"),
      p("Map"),
      s("List"),
      i("List"),
      s("Nil"),
      i("Nil"),
      s("Stream"),
      s("`#::`"),
      s("Vector"),
      i("Vector"),
      m("ArrayBuffer")
    )
  }
}

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
    Symbol("_root_.scala.Predef.intArrayOps."),
    Symbol("_root_.scala.Predef.augmentString."),
    Symbol("_root_.scala.Predef.intArrayOps.")
  )

  def normalize(symbol: Symbol): Symbol = {
    def loop(symbol: Symbol): Symbol = symbol match {
      case Symbol.Global(qual, Signature.Term("package")) => loop(qual)
      case Symbol.Global(qual, name)                      => Symbol.Global(loop(qual), name)
      case x                                              => x
    }
    loop(symbol.normalized)
  }

  def ifSymbolFound(ctx: RewriteCtx): Patch = {
    val toUnimport = ctx.mirror.database.names.flatMap { r =>
      val norm = normalize(r.sym)
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
    ifSymbolFound(ctx) + ctx.replaceSymbols(
      "scala.collection.immutable.HashMap" ->
        "strawman.collection.immutable.HashMap",
      "scala.collection.immutable.Map" ->
        "strawman.collection.immutable.Map",
      "scala.Predef.Map" ->
        "strawman.collection.immutable.Map",
      "scala.collection.immutable.List" ->
        "strawman.collection.immutable.List",
      "scala.collection.immutable.Nil" ->
        "strawman.collection.immutable.Nil",
      "scala.package.Stream" ->
        "strawman.collection.immutable.LazyList",
      "scala.package.`#::`" ->
        "strawman.collection.immutable.LazyList.`#::`",
      "scala.package.Vector" ->
        "strawman.collection.immutable.Vector",
      "scala.collection.mutable.ArrayBuffer" ->
        "strawman.collection.mutable.ArrayBuffer"
    )
  }
}

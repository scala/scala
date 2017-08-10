package fix
// scalafmt: { maxColumn = 120 }

import scala.collection.JavaConverters._
import scalafix._
import scalafix.syntax._
import scalafix.internal.util.SymbolOps
import scala.meta._
import org.scalameta.logger

case class Collectionstrawman_v0(mirror: SemanticCtx) extends SemanticRewrite(mirror) {
  val immutableListSymbol = Symbol("_root_.scala.collection.immutable.List.")
  val unimports = Map(
    Symbol("_root_.scala.Predef.augmentString.") ->
      Symbol("_root_.strawman.collection.stringToStringOps."),
    Symbol("_root_.scala.Predef.intArrayOps.") ->
      Symbol("_root_.strawman.collection.arrayToArrayOps.")
  )

  def ifSymbolFound(ctx: RewriteCtx): Patch = {
    logger.elem(ctx.mirror.names)
    val toImport = ctx.mirror.names
      .flatMap(r =>
        unimports.get {
          val x = r.sym.normalized
          logger.elem(x)
          x
      })
      .map(ctx.addGlobalImport)
    toImport.asPatch
  }

  def rewrite(ctx: RewriteCtx): Patch = {
    def p(name: String) =
      s"scala.Predef.$name" -> s"strawman.collection.immutable.$name"
    def s(name: String, rename: Option[String] = None) =
      s"scala.$name" -> s"strawman.collection.immutable.${rename.getOrElse(name)}"
    def i(name: String) =
      s"scala.collection.immutable.$name" -> s"strawman.collection.immutable.$name"
    def m(name: String) =
      s"scala.collection.mutable.$name" -> s"strawman.collection.mutable.$name"
    ifSymbolFound(ctx) +
      ctx.replaceSymbols(
        i("HashMap"),
        i("Map"),
        p("Map"),
        s("List"),
        i("List"),
        s("Nil"),
        i("Nil"),
        s("`::`"),
        i("`::`"),
        s("`+:`"),
        i("`+:`"),
        s("`:+`"),
        i("`:+`"),
        s("Stream", Some("LazyList")),
        s("`#::`"),
        s("`#::`"),
        s("Vector"),
        i("Vector"),
        m("ArrayBuffer")
      )
  }
}

package fix

import scalafix._
import scalafix.syntax._
import scala.meta._

case class Collectionstrawman_v0(mirror: SemanticCtx)
    extends SemanticRewrite(mirror) {
  val immutableListSymbol = Symbol("_root_.scala.collection.immutable.List.")
  val unimports = Map(
    Symbol("_root_.scala.Predef.augmentString.") ->
      Symbol("_root_.strawman.collection.stringToStringOps."),
    Symbol("_root_.scala.Predef.intArrayOps.") ->
      Symbol("_root_.strawman.collection.arrayToArrayOps.")
  )

  def ifSymbolFound(ctx: RewriteCtx): Patch = {
    val toImport = ctx.mirror.names
      .flatMap(r => unimports.get(r.sym.normalized))
      .map(ctx.addGlobalImport)
    toImport.asPatch
  }

  def rewrite(ctx: RewriteCtx): Patch = {
    ctx.debugMirror()
    def p(name: String) =
      s"scala.Predef.$name" -> s"strawman.collection.immutable.$name"
    def s(name: String, rename: Option[String] = None) =
      s"scala.$name" -> s"strawman.collection.immutable.${rename.getOrElse(name)}"
    def i(name: String, rename: Option[String] = None) =
      s"scala.collection.immutable.$name" ->
        s"strawman.collection.immutable.${rename.getOrElse(name)}"
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
        i("Stream", Some("LazyList")),
        s("Stream", Some("LazyList")),
        s("`#::`"),
        s("`#::`"),
        s("Vector"),
        i("Vector"),
        m("ArrayBuffer")
      )
  }
}

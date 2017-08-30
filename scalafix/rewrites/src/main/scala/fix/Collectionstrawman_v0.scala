package fix

import scalafix._
import scalafix.syntax._
import scala.meta._

case class Collectionstrawman_v0(sctx: SemanticCtx)
    extends SemanticRewrite(sctx) {

  implicit class XtensionSymbolCollection(symbol: Symbol) {
    def name = symbol match {
      case Symbol.Global(_, sig) => sig.name
      case _                     => symbol.syntax
    }
  }

  val unimports = Map(
    Symbol("_root_.scala.Predef.augmentString.") ->
      Symbol("_root_.strawman.collection.stringToStringOps."),
    Symbol("_root_.scala.Predef.intArrayOps.") ->
      Symbol("_root_.strawman.collection.arrayToArrayOps.")
  )

  val additionalUnimports = Map(
    "augmentString" -> "wrapString",
    "wrapString" -> "augmentString"
  )

  def replaceExtensionMethods(ctx: RewriteCtx): Patch = {
    val toImport = for {
      r <- ctx.semanticCtx.names
      in = r.sym.normalized
      out <- unimports.get(in).toList
    } yield {
      val name = in.name
      val names = name :: additionalUnimports
        .get(name)
        .fold(List.empty[String])(_ :: Nil)
      ctx.addGlobalImport(out) +
        ctx.addGlobalImport(
          Importer(q"scala.Predef", names.map(n => Importee.Unimport(Name(n)))))
    }
    val predefUnderscore =
      if (toImport.isEmpty) Patch.empty
      else ctx.addGlobalImport(importer"scala.Predef._")
    toImport.asPatch + predefUnderscore
  }

  val rangeImport = Symbol("_root_.strawman.collection.immutable.Range.")
  val inclusiveRange = Symbol(
    "_root_.scala.runtime.RichInt#to(I)Lscala/collection/immutable/Range/Inclusive;.")
  val rangeSymbol = Symbol(
    "_root_.scala.runtime.RichInt#until(I)Lscala/collection/immutable/Range;.")
  def replaceRange(ctx: RewriteCtx): Patch = {
    ctx.tree.collect {
      case tree @ Term.ApplyInfix(lhs, op, targs, arg :: Nil)
          if op.symbol.contains(inclusiveRange) =>
        ctx.replaceTree(tree, q"Range.inclusive($lhs, $arg)".syntax) +
          ctx.addGlobalImport(rangeImport)
      case tree @ Term.ApplyInfix(lhs, op, targs, arg :: Nil)
          if op.symbol.contains(rangeSymbol) =>
        ctx.replaceTree(tree, q"Range($lhs, $arg)".syntax) +
          ctx.addGlobalImport(rangeImport)
    }
  }.asPatch

  def replaceSymbols(ctx: RewriteCtx): Patch = {
    def p(name: String) =
      s"scala.Predef.$name" -> s"strawman.collection.immutable.$name"
    def s(name: String, rename: Option[String] = None) =
      s"scala.$name" -> s"strawman.collection.immutable.${rename.getOrElse(name)}"
    def i(name: String, rename: Option[String] = None) =
      s"scala.collection.immutable.$name" ->
        s"strawman.collection.immutable.${rename.getOrElse(name)}"
    def m(name: String) =
      s"scala.collection.mutable.$name" -> s"strawman.collection.mutable.$name"
    ctx.replaceSymbols(
      p("Set"),
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
      s("Traversable", Some("Iterable")),
      "scala.Iterable" -> "strawman.collection.Iterable",
      "scala.Traversable" -> "strawman.collection.Iterable",
      "scala.collection.TraversableLike.toIterator" -> "iterator",
      "scala.`#::`" -> "strawman.collection.immutable.LazyList.`#::`",
      s("Vector"),
      i("Vector"),
      m("ArrayBuffer")
    )
  }

  case class SymbolMatcher(symbols: Symbol*) {
    def unapply(arg: Tree): Option[(Tree, Symbol)] =
      sctx.symbol(arg.pos).flatMap { sym =>
        if (symbols.exists(_.isSameNormalized(sym))) Some(arg -> sym)
        else None
      }
  }

  object WithSymbol {
    def unapply(arg: Tree): Option[(Tree, Symbol)] =
      sctx.symbol(arg.pos).map(x => arg -> x)
  }

  val toGenericX = SymbolMatcher(
    Symbol("_root_.scala.collection.TraversableOnce.toMap.")
  )
  val toImmutableX = SymbolMatcher(
    Symbol("_root_.scala.collection.TraversableOnce.toList."),
    Symbol("_root_.scala.collection.TraversableOnce.toSet.")
  )
  val toTpe = SymbolMatcher(
    Symbol("_root_.scala.collection.TraversableLike.to.")
  )
  val iterator = SymbolMatcher(
    Symbol("_root_.scala.collection.LinearSeqLike.iterator.")
  )

  def replaceToList(ctx: RewriteCtx) =
    ctx.tree.collect {
      case iterator(n: Name, _) =>
        ctx.addRight(n.tokens.last, "()")
      case toImmutableX(n: Name, s) =>
        ctx.replaceTree(n, s"to(strawman.collection.immutable.${s.name.stripPrefix("to")})")
      case toGenericX(n: Name, s) =>
        ctx.replaceTree(n, s"to(strawman.collection.${s.name.stripPrefix("to")})")
      case toTpe(n: Name, _) =>
        ctx.debug(n)
        (for {
          name <- n.tokens.lastOption
          _ = ctx.debug(name)
          open <- ctx.tokenList.find(name)(t => t.is[Token.LeftBracket])
          _ = ctx.debug(open)
          close <- ctx.matching.close(open.asInstanceOf[Token.LeftBracket])
          replacedTokens = ctx.tokenList.slice(open, close)
        } yield
          ctx.replaceToken(open, "(") +
            ctx.replaceToken(close, ")")).getOrElse(Patch.empty)
    }.asPatch

  def rewrite(ctx: RewriteCtx): Patch = {
    replaceToList(ctx) +
      replaceSymbols(ctx) +
      replaceExtensionMethods(ctx) +
      replaceRange(ctx)
  }
}

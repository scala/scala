package fix

import scalafix._
import scalafix.syntax._
import scalafix.util._
import scala.meta._

case class Collectionstrawman_v0(index: SemanticdbIndex)
  extends SemanticRule(index, "Collectionstrawman_v0") {

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
    "wrapString" -> "augmentString",
    "intArrayOps" -> "genericArrayOps"
  )

  def replaceExtensionMethods(ctx: RuleCtx): Patch = {
    val toImport = for {
      r <- ctx.index.names
      in = r.symbol.normalized
      out <- unimports.get(in).toList
    } yield {
      val name = in.name
      val names = name :: additionalUnimports.get(name)
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
  def replaceRange(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case tree @ Term.ApplyInfix(lhs, op, _, arg :: Nil)
          if op.symbol.contains(inclusiveRange) =>
        ctx.replaceTree(tree, q"Range.inclusive($lhs, $arg)".syntax) +
          ctx.addGlobalImport(rangeImport)
      case tree @ Term.ApplyInfix(lhs, op, _, arg :: Nil)
          if op.symbol.contains(rangeSymbol) =>
        ctx.replaceTree(tree, q"Range($lhs, $arg)".syntax) +
          ctx.addGlobalImport(rangeImport)
    }
  }.asPatch

  def replaceSymbols(ctx: RuleCtx): Patch = {
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
      "scala.`#::`" -> "strawman.collection.immutable.LazyList.`#::`",
      s("Vector"),
      i("Vector"),
      m("ArrayBuffer")
    )
  }

  val toGenericX = SymbolMatcher.normalized(
    Symbol("_root_.scala.collection.TraversableOnce.toMap.")
  )
  val toImmutableX = SymbolMatcher.normalized(
    Symbol("_root_.scala.collection.TraversableOnce.toList."),
    Symbol("_root_.scala.collection.TraversableOnce.toSet.")
  )
  val toTpe = SymbolMatcher.normalized(
    Symbol("_root_.scala.collection.TraversableLike.to.")
  )
  val iterator = SymbolMatcher.normalized(
    Symbol("_root_.scala.collection.LinearSeqLike.iterator."),
    Symbol("_root_.scala.collection.TraversableLike.toIterator.")
  )
  val tupleZipped = SymbolMatcher.normalized(
    Symbol("_root_.scala.runtime.Tuple2Zipped.Ops.zipped."),
    Symbol("_root_.scala.runtime.Tuple3Zipped.Ops.zipped.")
  )

  def replaceToList(ctx: RuleCtx) =
    ctx.tree.collect {
      case iterator(t: Name) =>
        ctx.replaceTree(t, "iterator()")
      case toImmutableX(t @ Name(n)) =>
        ctx.replaceTree(t, s"to(strawman.collection.immutable.${n.stripPrefix("to")})")
      case toGenericX(t @ Name(n)) =>
        ctx.replaceTree(t, s"to(strawman.collection.${n.stripPrefix("to")})")
      case toTpe(n: Name) =>
        (for {
          name <- n.tokens.lastOption
          open <- ctx.tokenList.find(name)(t => t.is[Token.LeftBracket])
          close <- ctx.matchingParens.close(open.asInstanceOf[Token.LeftBracket])
        } yield
          ctx.replaceToken(open, "(") +
            ctx.replaceToken(close, ")")
        ).asPatch
    }.asPatch

  def replaceTupleZipped(ctx: RuleCtx) =
    ctx.tree.collect {
      case tupleZipped(Term.Select(Term.Tuple(args), name)) =>
        val removeTokensPatch =
          (for {
            zipped <- name.tokens.headOption
            closeTuple <- ctx.tokenList.leading(zipped).find(_.is[Token.RightParen])
            openTuple <- ctx.matchingParens.open(closeTuple.asInstanceOf[Token.RightParen])
            maybeDot = ctx.tokenList.slice(closeTuple, zipped).find(_.is[Token.Dot])
          } yield {
            ctx.removeToken(openTuple) +
              maybeDot.map(ctx.removeToken).asPatch +
              ctx.removeToken(zipped)
          }).asPatch

        def removeSurroundingWhiteSpaces(tk: Token) =
          (ctx.tokenList.trailing(tk).takeWhile(_.is[Token.Space]).map(ctx.removeToken) ++
            ctx.tokenList.leading(tk).takeWhile(_.is[Token.Space]).map(ctx.removeToken)).asPatch

        val commas =
          for {
            (prev, next) <- args.zip(args.tail)
            tokensBetweenArgs = ctx.tokenList.slice(prev.tokens.last, next.tokens.head)
            comma <- tokensBetweenArgs.find(_.is[Token.Comma])
          } yield comma

        val replaceCommasPatch = commas match {
          case head :: tail =>
            ctx.replaceToken(head, ".lazyZip(") +
              removeSurroundingWhiteSpaces(head) ++
              tail.map { comma =>
                ctx.replaceToken(comma, ").lazyZip(") +
                  removeSurroundingWhiteSpaces(comma)
              }
          case _ => Patch.empty
        }

        removeTokensPatch + replaceCommasPatch
    }.asPatch

  override def fix(ctx: RuleCtx): Patch = {
    replaceToList(ctx) +
      replaceSymbols(ctx) +
      replaceExtensionMethods(ctx) +
      replaceRange(ctx) +
      replaceTupleZipped(ctx)
  }
}

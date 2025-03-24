/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package typechecker
package splain

import scala.collection.immutable.{List, Nil, Seq}
import scala.collection.mutable
import scala.language.implicitConversions

class FormatCache[K, V](cache: mutable.Map[K, V]) {
  def apply(k: K, orElse: => V): V = cache.getOrElseUpdate(k, orElse)
}

object FormatCache {
  def apply[K, V]() = new FormatCache[K, V](mutable.Map())
}

trait SplainFormatters {
  self: Analyzer =>

  import global._
  import definitions._

  implicit def asSimpleName(s: String): SimpleName = SimpleName(s)

  def formatType(tpe: Type, top: Boolean): Formatted

  object Refined {
    def unapply(tpe: Type): Option[(List[Type], Scope)] = tpe match {
      case RefinedType(parents, decls) => Some((parents, decls))
      case t @ SingleType(_, _)        => unapply(t.underlying)
      case _                           => None
    }
  }

  trait SpecialFormatter {
    def apply[A](
        tpe: Type, simple: String, args: List[A], formattedArgs: => List[Formatted], top: Boolean,
    )(rec: (A, Boolean) => Formatted): Option[Formatted]

    def diff(left: Type, right: Type, top: Boolean): Option[Formatted]
  }

  object FunctionFormatter extends SpecialFormatter {
    def apply[A](
        tpe: Type, simple: String, args: List[A], formattedArgs: => List[Formatted], top: Boolean,
    )(rec: (A, Boolean) => Formatted) = {
      if (simple.startsWith("Function")) {
        val fmtArgs = formattedArgs
        val (params, returnt) = fmtArgs.splitAt(fmtArgs.length - 1)
        Some(FunctionForm(params, returnt.headOption.getOrElse(UnitForm), top))
      } else None
    }

    def diff(left: Type, right: Type, top: Boolean) = None
  }

  object TupleFormatter extends SpecialFormatter {
    def apply[A](
        tpe: Type, simple: String, args: List[A], formattedArgs: => List[Formatted], top: Boolean
    )(rec: (A, Boolean) => Formatted) = {
      if (simple.startsWith("Tuple")) Some(TupleForm(formattedArgs))
      else None
    }

    def diff(left: Type, right: Type, top: Boolean) = None
  }

  object RefinedFormatter extends SpecialFormatter {

    object DeclSymbol {
      def unapply(sym: Symbol): Option[(Formatted, Formatted)] =
        if (sym.hasRawInfo) Some((Simple(sym.simpleName.toString), formatType(sym.rawInfo, top = true)))
        else None
    }

    lazy val ignoredTypes: List[Type] = List(typeOf[Object], typeOf[Any], typeOf[AnyRef])

    def sanitizeParents: List[Type] => List[Type] = { ps =>
      val tpes = ps.distinct
      val result = tpes.filterNot(t => ignoredTypes.exists(_ =:= t))

      if (result.isEmpty) tpes.headOption.toList
      else result
    }

    object Refined {
      def unapply(tpe: Type): Option[(List[Type], Scope)] =
        tpe match {
          case TypeRef(pre, sym, List(RefinedType(parents, decls)))
            if decls.isEmpty && pre.typeSymbol.fullName == "zio" && sym.fullName == "zio.Has" =>
            val sanitized = sanitizeParents(parents)
            if (sanitized.length == 1)
              Some((List(TypeRef(pre, sym, sanitized.headOption.toList)), decls))
            else
              None
          case RefinedType(types, scope) =>
            if (scope.isEmpty) {
              val subtypes = types.map(_.dealias).flatMap {
                case Refined(types, _) =>
                  types
                case tpe =>
                  List(tpe)
              }
              Some((subtypes, scope))
            } else
              Some((types, scope))
          case t@SingleType(_, _) =>
            unapply(t.underlying)
          case _ =>
            None
        }
    }

    def formatDecl: Symbol => Formatted = {
      case DeclSymbol(n, t) => Decl(n, t)
      case sym => Simple(sym.toString)
    }

    override def apply[A](
        tpe: Type,
        simple: String,
        args: List[A],
        formattedArgs: => List[Formatted],
        top: Boolean
    )(rec: (A, Boolean) => Formatted): Option[Formatted] = {
      tpe match {
        case Refined(parents, decls) =>
          Some(RefinedForm(sanitizeParents(parents).map(formatType(_, top)), decls.toList.map(formatDecl)))
        case _ => None
      }
    }

    val none: Formatted = Simple("<none>")

    def separate[A](left: List[A], right: List[A]): (List[A], List[A], List[A]) = {
      val leftS = Set(left: _*)
      val rightS = Set(right: _*)
      val common = leftS.intersect(rightS)
      val uniqueLeft = leftS -- common
      val uniqueRight = rightS -- common
      (common.toList, uniqueLeft.toList, uniqueRight.toList)
    }

    def matchTypes(left: List[Type], right: List[Type]): List[Formatted] = {
      val (common, uniqueLeft, uniqueRight) =
        separate(left.map(formatType(_, top = true)), right.map(formatType(_, top = true)))
      val diffs = uniqueLeft
          .zipAll(uniqueRight, none, none)
          .map {
            case (l, r) =>
              Diff(l, r)
          }
      common ++ diffs
    }

    def filterDecls(syms: List[Symbol]): List[(Formatted, Formatted)] =
      syms.collect {
        case DeclSymbol(sym, rhs) =>
          (sym, rhs)
      }

    def matchDecls(left: List[Symbol], right: List[Symbol]): List[Formatted] = {
      val (common, uniqueLeft, uniqueRight) = separate(filterDecls(left), filterDecls(right))
      val diffs = uniqueLeft
          .map(Some(_))
          .zipAll(uniqueRight.map(Some(_)), None, None)
          .collect {
            case (Some((sym, l)), Some((_, r))) => DeclDiff(sym, l, r)
            case (None, Some((sym, r)))         => DeclDiff(sym, none, r)
            case (Some((sym, l)), None)         => DeclDiff(sym, l, none)
          }
      common.map {
        case (sym, rhs) => Decl(sym, rhs)
      } ++ diffs
    }

    def diff(left: Type, right: Type, top: Boolean): Option[Formatted] =
      (left, right) match {
        case (Refined(leftParents, leftDecls), Refined(rightParents, rightDecls)) =>
          val parents = matchTypes(sanitizeParents(leftParents), sanitizeParents(rightParents)).sorted
          val decls = matchDecls(leftDecls.toList, rightDecls.toList).sorted
          Some(RefinedForm(parents, decls))
        case _ => None
      }
  }

  object ByNameFormatter extends SpecialFormatter {
    def apply[A](
        tpe: Type, simple: String, args: List[A], formattedArgs: => List[Formatted], top: Boolean,
    )(rec: (A, Boolean) => Formatted): Option[Formatted] = tpe match {
      case TypeRef(_, ByNameParamClass, List(a)) => Some(ByName(formatType(a, top = true)))
      case _                                     => None
    }

    def diff(left: Type, right: Type, top: Boolean): Option[Formatted] = None
  }
}

object SplainFormatting {

  import scala.reflect.internal.TypeDebugging.AnsiColor._

  val ELLIPSIS: String = "⋮".blue
}

trait SplainFormatting extends SplainFormatters {
  self: Analyzer =>

  import global._

  import SplainFormatting._
  import scala.reflect.internal.TypeDebugging.AnsiColor._

  case class ImplicitErrorLink(
      fromTree: ImplicitError,
      fromHistory: DivergentImplicitTypeError
  ) {

    val sameCandidateTree: Boolean = fromTree.candidate equalsStructure fromHistory.underlyingTree

    val samePendingType: Boolean = fromTree.specifics match {
      case ss: ImplicitErrorSpecifics.NotFound =>
        fromHistory.pt0 =:= ss.param.tpe
      case _ =>
        false
    }

    val moreSpecificPendingType: Boolean = fromTree.specifics match {
      case ss: ImplicitErrorSpecifics.NotFound =>
        fromHistory.pt0 <:< ss.param.tpe
      case _ =>
        false
    }

    val sameStartingWith: Boolean = {
      fromHistory.sym.fullLocationString == fromTree.candidate.symbol.fullLocationString
    }

    lazy val divergingSearchStartingWithHere: Boolean = sameStartingWith

    lazy val divergingSearchDiscoveredHere: Boolean = sameCandidateTree && moreSpecificPendingType
  }

  object ImplicitErrorLink {}

  case class ImplicitErrorTree(
      error: ImplicitError,
      children: Seq[ImplicitErrorTree] = Nil
  ) {

    import ImplicitErrorTree._

    def doCollectFull(alwaysDisplayRoot: Boolean = false): Seq[ErrorNode] =
      if (children.isEmpty) Seq(ErrorNode(error, alwaysShow = true))
      else {

        Seq(ErrorNode(error, alwaysShow = alwaysDisplayRoot)) ++ {

          if (children.size >= 2) children.flatMap(_.doCollectFull(alwaysDisplayRoot = true))
          else children.flatMap(_.doCollectFull(alwaysDisplayRoot = false))
        }
      }

    lazy val collectFull: Seq[ErrorNode] = doCollectFull(alwaysDisplayRoot = true)

    lazy val collectCompact: Seq[ErrorNode] = {

      val displayed = collectFull.zipWithIndex.filter {
        case (v, _) =>
          v.alwaysShow
      }

      val ellipsisIndices = displayed.map(_._2 - 1).toSet + (collectFull.size - 1)

      val withEllipsis = displayed.map {
        case (v, i) =>
          if (!ellipsisIndices.contains(i)) v.copy(showEllipsis = true)
          else v
      }

      withEllipsis
    }

    case class FormattedChain(
        source: Seq[ErrorNode]
    ) {

      val toList: List[String] = {
        val collected = source.toList
        val baseIndent = collected.headOption.map(_.nesting).getOrElse(0)

        val formatted = collected.map { v =>
          val formatted = v.formatted
          if (v.showEllipsis) formatted.copy(_2 = formatted._2 :+ ELLIPSIS)
          else formatted
        }

        indentTree(formatted, baseIndent)
      }

      override lazy val toString: String = toList.mkString("\n")
    }

    object FormattedChain {

      object Full extends FormattedChain(collectFull)

      object Compact extends FormattedChain(collectCompact)

      val display: FormattedChain = if (settings.VimplicitsVerboseTree.value) Full else Compact
    }

    override def toString: String = FormattedChain.Full.toString
  }

  object ImplicitErrorTree {

    case class ErrorNode(
        error: ImplicitError,
        alwaysShow: Boolean,
        showEllipsis: Boolean = false
    ) {

      def nesting: RunId = error.nesting

      val formatted: (String, List[String], RunId) =
        formatNestedImplicit(error)
    }

    def fromNode(
        Node: ImplicitError,
        offsprings: List[ImplicitError]
    ): ImplicitErrorTree = {
      val topNesting = Node.nesting

      val children = fromChildren(
        offsprings,
        topNesting
      )

      ImplicitErrorTree(Node, children)
    }

    def fromChildren(
        offsprings: List[ImplicitError],
        topNesting: Int
    ): List[ImplicitErrorTree] = {

      if (offsprings.isEmpty)
        return Nil

      val minNesting = offsprings.map(v => v.nesting).min

      if (minNesting < topNesting + 1)
        throw new InternalError(
          "Detail: nesting level of offsprings of an implicit search tree node should be higher"
        )

      val wII = offsprings.zipWithIndex

      val childrenII = wII
          .filter {
            case (sub, _) =>
              if (sub.nesting < minNesting) {
                throw new InternalError(
                  s"Detail: Sub-node in implicit tree can only have nesting level larger than top node," +
                      s" but (${sub.nesting} < $minNesting)"
                )
              }

              sub.nesting == minNesting
          }
          .map(_._2)

      val ranges = {

        val seqs = (childrenII ++ Seq(offsprings.size))
            .sliding(2)
            .toList

        seqs.map {
          case Seq(from, until) =>
            from -> until
          case _ =>
            throw new InternalError("Detail: index should not be empty")
        }
      }

      val children = ranges.map { range =>
        val _top = offsprings(range._1)

        val _offsprings = offsprings.slice(range._1 + 1, range._2)

        fromNode(
          _top,
          _offsprings
        )
      }

      mergeDuplicates(children)
      //      children
    }

    def mergeDuplicates(children: List[ImplicitErrorTree]): List[ImplicitErrorTree] = {
      val errors = children.map(_.error).distinct

      val grouped = errors.map { ee =>
        val group = children.filter(c => c.error == ee)

        val mostSpecificError = group.head.error
        // TODO: this old design is based on a huge hypothesis, should it be improved
        //        val mostSpecificError = group.map(_.error).maxBy(v => v.candidate.toString.length)

        val allChildren = group.flatMap(v => v.children)
        val mergedChildren = mergeDuplicates(allChildren)

        ImplicitErrorTree(mostSpecificError, mergedChildren)
      }

      grouped.distinctBy(v => v.FormattedChain.Full.toString) // TODO: this may lose information
    }
  }


  def formatNestedImplicit(err: ImplicitError): (String, List[String], Int) = {

    val candidate = ImplicitError.cleanCandidate(err)
    val problem = s"${candidate.red} invalid because"
    val reason = err.specifics match {
      case e: ImplicitErrorSpecifics.NotFound => implicitMessage(e.param, NoImplicitFoundAnnotation(err.candidate, e.param)._2)
      case e: ImplicitErrorSpecifics.NonconformantBounds => formatNonConfBounds(e)
    }
    val base = (problem, reason, err.nesting)

    val reasons = base._2

    (problem, reasons, base._3)
  }

  val breakInfixLength: Int = 70

  def dealias(tpe: Type) =
    if (isAux(tpe)) tpe
    else (tpe match {
      case ExistentialType(_, t) => t
      case _                     => tpe
    }).dealias

  def extractArgs(tpe: Type) = tpe match {
    case PolyType(params, result) => result.typeArgs.map {
      case t if params.contains(t.typeSymbol) => WildcardType
      case a                                  => a
    }
    case t: AliasTypeRef if !isAux(tpe) =>
      t.betaReduce.typeArgs.map(a => if (a.typeSymbolDirect.isTypeParameter) WildcardType else a)
    case _ => tpe.typeArgs
  }

  def isRefined(tpe: Type) = tpe.dealias match {
    case RefinedType(_, _) => true
    case _                 => false
  }

  def isSymbolic(tpe: Type) = {
    val n = tpe.typeConstructor.typeSymbol.name
    !isRefined(tpe) && n.encodedName.toString != n.decodedName.toString
  }

  def ctorNames(tpe: Type): List[String] =
    scala.util.Try(tpe.typeConstructor.toString)
      .map(_.split('.').toList)
      .getOrElse(List(tpe.toString))

  def isAux(tpe: Type) = ctorNames(tpe).lastOption.contains("Aux")

  def formatRefinement(sym: Symbol) = {
    if (sym.hasRawInfo) s"$sym = ${showType(sym.rawInfo)}"
    else sym.toString
  }

  def formatAuxSimple(tpe: Type): (List[String], String) = {
    val names = ctorNames(tpe)
    (names.dropRight(2), ctorNames(tpe).takeRight(2).mkString("."))
  }

  def symbolPath(sym: Symbol): List[String] =
    sym
      .ownerChain
      .takeWhile(sym => sym.isType && !sym.isPackageClass)
      .map(_.name.decodedName.toString)
      .reverse

  def sanitizePath(path: List[String]): List[String] =
    path.takeWhile(_ != "type").filter(!_.contains("$"))

  def pathPrefix: List[String] => String = {
    case Nil                => ""
    case List("<noprefix>") => ""
    case a                  => a.mkString("", ".", ".")
  }

  def qualifiedName(path: List[String], name: FormattedName): String = name match {
    case SimpleName(name) => s"${pathPrefix(path)}$name"
    case InfixName(name) => name
  }

  def stripModules(path: List[String], name: FormattedName): String = {
    val qName = qualifiedName(path, name)
    if (shorthands(qName)) name.name else qName
  }

  case class TypeParts(sym: Symbol, tt: Type) {
    def modulePath: List[String] = (tt, sym) match {
      case (TypeRef(pre, _, _), _) if !pre.toString.isEmpty => sanitizePath(pre.toString.split("\\.").toList)
      case (SingleType(_, _), sym)                          => symbolPath(sym).dropRight(1)
      case (_, _)                                           => Nil
    }

    def ownerPath: List[String] = {
      val parts = sym.ownerChain.reverse.map(_.name.decodedName.toString)
      parts.splitAt(Math.max(0, parts.size - 1))._1
    }

    def shortName: String = tt.safeToString.stripPrefix(tt.prefixString.split('.').dropRight(1).mkString(".") + ".")
  }

  def stripType(tpe: Type): (List[String], String) = tpe match {
    case tt: SingletonType =>
      val parts = TypeParts(tt.termSymbol, tt)
      parts.modulePath -> parts.shortName

    case tt: RefinedType =>
      val parts = TypeParts(tt.typeSymbol, tt)
      parts.modulePath -> parts.shortName

    case _ =>
      // TODO: should this also use TypeParts ?
      val sym     = if (tpe.takesTypeArgs) tpe.typeSymbolDirect else tpe.typeSymbol
      val symName = sym.name.decodedName.toString
      val parts   = TypeParts(sym, tpe)
      (parts.modulePath, if (sym.isModuleClass) s"$symName.type" else symName)
  }

  def formatNormalSimple(tpe: Type): (List[String], String) = tpe match {
    case a @ WildcardType => (Nil, a.toString)
    case a                => stripType(a)
  }

  def formatSimpleType(tpe: Type): (List[String], String) =
    if (isAux(tpe)) formatAuxSimple(tpe)
    else formatNormalSimple(tpe)

  def indentLine(line: String, n: Int = 1, prefix: String = "  ")    = (prefix * n) + line
  def indent(lines: List[String], n: Int = 1, prefix: String = "  ") = lines.map(indentLine(_, n, prefix))

  /** If the args of an applied type constructor are multiline,
   *  create separate lines for the constructor name and the closing bracket;
   *  else return a single line. */
  def showTypeApply(cons: String, args: List[TypeRepr], break: Boolean): TypeRepr = {
    val flatArgs = bracket(args.map(_.flat))
    val flat     = FlatType(s"$cons$flatArgs")
    def brokenArgs = args match {
      case head :: tail => tail.foldLeft(head.lines)((z, a) => z ::: "," :: a.lines)
      case _            => Nil
    }
    def broken = BrokenType(s"$cons[" :: indent(brokenArgs) ::: List("]"))
    if (break) decideBreak(flat, broken) else flat
  }

  def showTuple(args: List[String]) = args match {
    case head :: Nil => s"Tuple1[$head]"
    case _           => args.mkString("(", ",", ")")
  }

  def showFuncParams(args: List[String]) = args match {
    case head :: Nil => head
    case _           => args.mkString("(", ",", ")")
  }

  def showRefined(parents: List[String], decls: List[String]) = {
    val p = parents.mkString(" with ")
    val d = if (decls.isEmpty) "" else decls.mkString(" {", "; ", "}")
    s"$p$d"
  }

  def bracket[A](params: List[A]) = params.mkString("[", ", ", "]")

  def formatFunction(args: List[String]) = {
    val (params, returnt) = args.splitAt(args.length - 1)
    s"${showTuple(params)} => ${showTuple(returnt)}"
  }

  def decideBreak(flat: FlatType, broken: => BrokenType): TypeRepr =
    if (flat.flat.length > breakInfixLength) broken
    else flat

  /** Turn a nested infix type structure into a flat list
   *  {{{
   *    ::[A, ::[B, C]]] => List(A, ::, B, ::, C)
   *  }}}
   */
  def flattenInfix(tpe: Infix): List[Formatted] = {
    def step(tpe: Formatted): List[Formatted] = tpe match {
      case Infix(infix, left, right, _) => left :: infix :: step(right)
      case a                            => List(a)
    }
    step(tpe)
  }

  /** Break a list produced by [[flattenInfix]] into lines by taking two
   *  elements at a time, then appending the terminal.
   *  If the expression's length is smaller than the threshold specified via
   *  plugin parameter, return a single line. */
  def breakInfix(types: List[Formatted]): TypeRepr = {
    val form   = types.map(showFormattedL(_, break = true))
    def broken = form.sliding(2, 2).flatMap {
      case FlatType(tpe) :: FlatType(infix) :: Nil => List(s"$tpe $infix")
      case left :: right :: Nil                    => left.lines ++ right.lines
      case last :: Nil                             => last.lines
      case _                                       => Nil
    }.toList
    decideBreak(FlatType(form.flatMap(_.lines).mkString(" ")), BrokenType(broken))
  }

  val showFormattedLCache = FormatCache[(Formatted, Boolean), TypeRepr]()
  val formatTypeCache     = FormatCache[(Type, Boolean), Formatted]()
  val formatDiffCache     = FormatCache[(Type, Type, Boolean), Formatted]()

  val specialFormatters: List[SpecialFormatter] =
    List(FunctionFormatter, TupleFormatter, RefinedFormatter, ByNameFormatter)

  def truncateDecls(decls: List[Formatted]): Boolean = settings.VimplicitsMaxRefined.value < decls.map(_.length).sum

  def showFormattedQualified(path: List[String], name: FormattedName): TypeRepr =
    FlatType(stripModules(path, name))

  def formattedDiff(left: Formatted, right: Formatted): String = (left, right) match {
    case (Qualified(lpath, lname), Qualified(rpath, rname)) if lname == rname =>
      val prefix = lpath.reverseIterator.zip(rpath.reverseIterator).takeWhile { case (l, r) => l == r }.size + 1
      s"${qualifiedName(lpath.takeRight(prefix), lname).red}|${qualifiedName(rpath.takeRight(prefix), rname).green}"
    case (left, right) =>
      val l = showFormatted(left)
      val r = showFormatted(right)
      s"${l.red}|${r.green}"
  }

  def showFormattedLImpl(tpe: Formatted, break: Boolean): TypeRepr = tpe match {
    case Simple(name)                 => FlatType(name.name)
    case Qualified(path, name)        => showFormattedQualified(path, name)
    case Applied(cons, args)          => showTypeApply(showFormatted(cons), args.map(showFormattedL(_, break)), break)
    case tpe @ Infix(_, _, _, top)    => wrapParensRepr(if (break) breakInfix(flattenInfix(tpe)) else FlatType(flattenInfix(tpe).map(showFormatted).mkString(" ")), top)
    case UnitForm                     => FlatType("Unit")
    case FunctionForm(args, ret, top) => FlatType(wrapParens(s"${showFuncParams(args.map(showFormatted))} => ${showFormatted(ret)}", top))
    case TupleForm(elems)             => FlatType(showTuple(elems.map(showFormatted)))
    case RefinedForm(elems, decls)    => FlatType(showRefined(elems.map(showFormatted), if (truncateDecls(decls)) List("...") else decls.map(showFormatted)))
    case Diff(left, right)            => FlatType(formattedDiff(left, right))
    case Decl(sym, rhs)               => FlatType(s"type ${showFormatted(sym)} = ${showFormatted(rhs)}")
    case DeclDiff(sym, left, right)   => FlatType(s"type ${showFormatted(sym)} = ${formattedDiff(left, right)}")
    case ByName(tpe)                  => FlatType(s"(=> ${showFormatted(tpe)})")
  }

  def showFormattedL(tpe: Formatted, break: Boolean): TypeRepr = showFormattedLCache((tpe, break), showFormattedLImpl(tpe, break))
  def showFormatted(tpe: Formatted): String                    = showFormattedL(tpe, break = false).tokenize
  def showType(tpe: Type): String                              = showFormattedL(formatType(tpe, top = true), break = false).joinLines
  def showTypeBreakL(tpe: Type): List[String]                  = showFormattedL(formatType(tpe, top = true), break = true).lines

  def wrapParens(expr: String, top: Boolean): String = if (top) expr else s"($expr)"

  def wrapParensRepr(tpe: TypeRepr, top: Boolean): TypeRepr = tpe match {
    case FlatType(tpe)     => FlatType(wrapParens(tpe, top))
    case BrokenType(lines) => if (top) tpe else BrokenType("(" :: indent(lines) ::: List(")"))
  }

  def formatSpecial[A](
      tpe: Type, simple: String, args: List[A], formattedArgs: => List[Formatted], top: Boolean,
  )(rec: (A, Boolean) => Formatted): Option[Formatted] =
    specialFormatters.iterator.map(_.apply(tpe, simple, args, formattedArgs, top)(rec)).collectFirst { case Some(a) => a }

  def formatInfix[A](
      path: List[String], simple: String, left: A, right: A, top: Boolean,
  )(rec: (A, Boolean) => Formatted): Formatted =
    Infix(Qualified(path, InfixName(simple)), rec(left, false), rec(right, false), top)

  def formatWithInfix[A](tpe: Type, args: List[A], top: Boolean)(rec: (A, Boolean) => Formatted): Formatted = {
    val (path, simple)     = formatSimpleType(tpe)
    lazy val formattedArgs = args.map(rec(_, true))
    formatSpecial(tpe, simple, args, formattedArgs, top)(rec).getOrElse {
      args match {
        case left :: right :: Nil if isSymbolic(tpe) => formatInfix(path, simple, left, right, top)(rec)
        case _ :: _                                  => Applied(Qualified(path, SimpleName(simple)), formattedArgs)
        case _                                       => Qualified(path, SimpleName(simple))
      }
    }
  }

  def formatTypeImpl(tpe: Type, top: Boolean): Formatted = {
    val dtpe = dealias(tpe)
    formatWithInfix(dtpe, extractArgs(dtpe), top)(formatType)
  }

  def formatType(tpe: Type, top: Boolean): Formatted = formatTypeCache((tpe, top), formatTypeImpl(tpe, top))

  def formatDiffInfix(left: Type, right: Type, top: Boolean): Formatted =
    formatWithInfix(left, extractArgs(left).zip(extractArgs(right)), top) { case ((l, r), t) => formatDiff(l, r, t) }

  def formatDiffSpecial(left: Type, right: Type, top: Boolean): Option[Formatted] =
    specialFormatters.iterator.map(_.diff(left, right, top)).collectFirst { case Some(a) => a }

  def formatDiffSimple(left: Type, right: Type): Formatted =
    Diff(formatType(left, top = true), formatType(right, top = true))

  def formatDiffImpl(found: Type, req: Type, top: Boolean): Formatted = {
    val (left, right) = dealias(found) -> dealias(req)

    val normalized = Seq(left, right).map(_.normalize).distinct
    if (normalized.size == 1) return formatType(normalized.head, top)

    if (left.typeSymbol == right.typeSymbol) formatDiffInfix(left, right, top)
    else formatDiffSpecial(left, right, top).getOrElse(formatDiffSimple(left, right))
  }

  def formatDiff(left: Type, right: Type, top: Boolean): Formatted =
    formatDiffCache((left, right, top), formatDiffImpl(left, right, top))

  def formatNonConfBounds(err: ImplicitErrorSpecifics.NonconformantBounds): List[String] = {
    val params = bracket(err.tparams.map(_.defString))
    val types  = bracket(err.targs.map(showType))
    List("nonconformant bounds;", types.red, params.green)
  }

  def hideImpError(error: ImplicitError): Boolean = error.specifics match {
    case ImplicitErrorSpecifics.NonconformantBounds(_, _, _) => true
    case ImplicitErrorSpecifics.NotFound(_)                  => false
  }

  def indentTree(tree: List[(String, List[String], Int)], baseIndent: Int): List[String] = {
    val nestings = tree.map(_._3).distinct.sorted
    tree.flatMap { case (head, tail, nesting) =>
      val ind = baseIndent + nestings.indexOf(nesting).abs
      indentLine(head, ind, "――") :: indent(tail, ind)
    }
  }

  def formatIndentTree(chain: List[ImplicitError], baseIndent: Int) =
    indentTree(chain.map(formatNestedImplicit), baseIndent)

  def deepestLevel(chain: List[ImplicitError]) =
    chain.foldLeft(0)((z, a) => if (a.nesting > z) a.nesting else z)

  def formatImplicitChainTreeFull(chain: List[ImplicitError]): List[String] =
    formatIndentTree(chain, chain.headOption.map(_.nesting).getOrElse(0))

  def formatImplicitChainFlat(chain: List[ImplicitError]): List[String] =
    chain.map(formatNestedImplicit).flatMap { case (h, t, _) => h :: t }

  def implicitMessage(param: Symbol, annotationMsg: String): List[String] = {
    val tpe = param.tpe
    val msg = if (annotationMsg.isEmpty) Nil else annotationMsg.split("\n").toList.map(_.blue) :+ ""
    val head  = s"${"!".red}${"I".blue} ${param.name.toString.yellow}:"
    val lines = showTypeBreakL(tpe).map(_.green) match {
      case single :: Nil => List(s"$head $single")
      case l             => head :: indent(l)
    }
    lines ::: indent(msg)
  }

  def splitChains(errors: List[ImplicitError]): List[List[ImplicitError]] = {
    errors.foldRight(Nil: List[List[ImplicitError]]) {
      case (a, chains @ ((chain @ (prev :: _)) :: tail)) =>
        if (a.nesting > prev.nesting) List(a) :: chains
        else (a :: chain) :: tail
      case (a, _) => List(List(a))
    }
  }

  def formatImplicitError(
      param: Symbol,
      errors: List[ImplicitError],
      annotationMsg: String
  ): String = {

    val msg = implicitMessage(param, annotationMsg)
    val errorTrees = ImplicitErrorTree.fromChildren(errors, -1)

    val errorTreesStr = errorTrees.map(_.FormattedChain.display.toString)

    val components: Seq[String] =
      Seq("implicit error;") ++
        msg ++
        errorTreesStr

    val result = components.mkString("\n")

    result
  }
}

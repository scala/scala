/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
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

import collection.mutable

import StringColor._

object Messages
{
  val hasMatching = "hasMatchingSymbol reported error: "

  val typingTypeApply =
    "typing TypeApply reported errors for the implicit tree: "
}

class FormatCache[K, V](cache: mutable.Map[K, V], var hits: Long)
{
  def apply(k: K, orElse: => V) = {
    if (cache.contains(k)) hits += 1
    cache.getOrElseUpdate(k, orElse)
  }

  def stats = s"${cache.size}/$hits"
}

object FormatCache
{
  def apply[K, V] = new FormatCache[K, V](mutable.Map(), 0)
}

trait SplainFormatters
{ self: Analyzer =>
  import global._

  def formatType(tpe: Type, top: Boolean): Formatted

  object Refined {
    def unapply(tpe: Type): Option[(List[Type], Scope)] =
      tpe match {
        case RefinedType(parents, decls) =>
          Some((parents, decls))
        case t @ SingleType(_, _) =>
          unapply(t.underlying)
        case _ =>
          None
      }
  }

  trait SpecialFormatter
  {
    def apply[A](tpe: Type, simple: String, args: List[A],
      formattedArgs: => List[Formatted], top: Boolean,
      rec: A => Boolean => Formatted): Option[Formatted]

    def diff(left: Type, right: Type, top: Boolean): Option[Formatted]
  }

  object FunctionFormatter
  extends SpecialFormatter
  {
    def apply[A](tpe: Type, simple: String, args: List[A],
      formattedArgs: => List[Formatted], top: Boolean,
      rec: A => Boolean => Formatted) = {
        if (simple.startsWith("Function"))
          Some(FunctionForm.fromArgs(formattedArgs, top))
        else None
    }

    def diff(left: Type, right: Type, top: Boolean) = None
  }

  object TupleFormatter
  extends SpecialFormatter
  {
    def apply[A](tpe: Type, simple: String, args: List[A],
      formattedArgs: => List[Formatted], top: Boolean,
      rec: A => Boolean => Formatted) = {
        if (simple.startsWith("Tuple"))
          Some(TupleForm(formattedArgs))
        else None
    }

    def diff(left: Type, right: Type, top: Boolean) = None
  }

  object RefinedFormatter extends SpecialFormatter {
    object DeclSymbol {
      def unapply(sym: Symbol): Option[(Formatted, Formatted)] =
        if (sym.hasRawInfo)
          Some((Simple(sym.simpleName.toString), formatType(sym.rawInfo, true)))
        else
          None
    }

    def ignoredTypes: List[Type] = List(typeOf[Object], typeOf[Any], typeOf[AnyRef])

    def sanitizeParents: List[Type] => List[Type] = {
      case List(tpe) =>
        List(tpe)
      case tpes =>
        tpes.filterNot(t => ignoredTypes.exists(_ =:= t))
    }

    def formatDecl: Symbol => Formatted = {
      case DeclSymbol(n, t) =>
        Decl(n, t)
      case sym =>
        Simple(sym.toString)
    }

    def apply[A](
      tpe: Type,
      simple: String,
      args: List[A],
      formattedArgs: => List[Formatted],
      top: Boolean,
      rec: A => Boolean => Formatted,
    ): Option[Formatted] =
      tpe match {
        case Refined(parents, decls) =>
          Some(RefinedForm(sanitizeParents(parents).map(formatType(_, top)), decls.toList.map(formatDecl)))
        case _ =>
          None
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
      val (common, uniqueLeft, uniqueRight) = separate(left.map(formatType(_, true)), right.map(formatType(_, true)))
      val diffs = uniqueLeft
        .toList
        .zipAll(uniqueRight.toList, none, none)
        .map { case (l, r) =>
          Diff(l, r)
        }
      common.toList ++ diffs
    }

    def filterDecls(syms: List[Symbol]): List[(Formatted, Formatted)] =
      syms.collect { case DeclSymbol(sym, rhs) =>
        (sym, rhs)
      }

    def matchDecls(left: List[Symbol], right: List[Symbol]): List[Formatted] = {
      val (common, uniqueLeft, uniqueRight) = separate(filterDecls(left), filterDecls(right))
      val diffs = uniqueLeft
        .toList
        .map(Some(_))
        .zipAll(uniqueRight.toList.map(Some(_)), None, None)
        .collect {
          case (Some((sym, l)), Some((_, r))) =>
            DeclDiff(sym, l, r)
          case (None, Some((sym, r))) =>
            DeclDiff(sym, none, r)
          case (Some((sym, l)), None) =>
            DeclDiff(sym, l, none)
        }
      common.toList.map { case (sym, rhs) =>
        Decl(sym, rhs)
      } ++ diffs
    }

    def diff(left: Type, right: Type, top: Boolean): Option[Formatted] =
      (left, right) match {
        case (Refined(leftParents, leftDecls), Refined(rightParents, rightDecls)) =>
          val parents = matchTypes(sanitizeParents(leftParents), sanitizeParents(rightParents)).sorted
          val decls = matchDecls(leftDecls.toList, rightDecls.toList).sorted
          Some(RefinedForm(parents, decls))
        case _ =>
          None
      }
  }

  object ByNameFormatter extends SpecialFormatter {
    def apply[A](
      tpe: Type,
      simple: String,
      args: List[A],
      formattedArgs: => List[Formatted],
      top: Boolean,
      rec: A => Boolean => Formatted,
    ): Option[Formatted] =
      tpe match {
        case TypeRef(_, sym, List(a)) if sym.name.decodedName.toString == "<byname>" =>
          Some(ByName(formatType(a, true)))
        case _ =>
          None
      }

    def diff(left: Type, right: Type, top: Boolean): Option[Formatted] = None
  }
}

trait SplainFormatting
extends SplainFormatters
{ self: Analyzer =>
  import global._

  def breakInfixLength: Int = 70

  def splainSettingTruncRefined: Option[Int] = {
    val value = settings.VimplicitsMaxRefined.value
    if (value == 0) None else Some(value)
  }

  implicit def colors =
    if(settings.implicitsSettingNoColor) StringColors.noColor
    else StringColors.color

  def dealias(tpe: Type) =
    if (isAux(tpe)) tpe
    else {
      val actual = tpe match {
        case ExistentialType(_, t) => t
        case _ => tpe
      }
      actual.dealias
    }

  def extractArgs(tpe: Type) = {
    tpe match {
      case PolyType(params, result) =>
        result.typeArgs.map {
          case t if params.contains(t.typeSymbol) => WildcardType
          case a => a
        }
      case t: AliasTypeRef if !isAux(tpe) =>
        t.betaReduce.typeArgs.map(a => if (a.typeSymbolDirect.isTypeParameter) WildcardType else a)
      case _ => tpe.typeArgs
    }
  }

  def isRefined(tpe: Type) = tpe.dealias match {
    case RefinedType(_, _) => true
    case _ => false
  }

  def isSymbolic(tpe: Type) = {
    val n = tpe.typeConstructor.typeSymbol.name
    !isRefined(tpe) && (n.encodedName.toString != n.decodedName.toString)
  }

  def ctorNames(tpe: Type): List[String] =
    scala.util.Try(tpe.typeConstructor.toString)
      .map(_.split('.').toList)
      .getOrElse(List(tpe.toString))

  def isAux(tpe: Type) = ctorNames(tpe).lastOption.contains("Aux")

  def formatRefinement(sym: Symbol) = {
    if (sym.hasRawInfo) {
      val rhs = showType(sym.rawInfo)
      s"$sym = $rhs"
    }
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
    path
      .takeWhile(_ != "type")
      .filterNot(_.contains("$"))

  def pathPrefix: List[String] => String = {
    case Nil =>
      ""
    case List("<noprefix>") =>
      ""
    case a =>
      a.mkString("", ".", ".")
  }

  def qualifiedName(path: List[String], name: String): String = s"${pathPrefix(path)}$name"

  def stripModules(path: List[String], name: String): Option[Int] => String = {
    case Some(keep) =>
      qualifiedName(path.takeRight(keep), name)
    case None =>
      name
  }

  case class TypeParts(sym: Symbol, tt: Type) {

    def modulePath: List[String] =
      (tt, sym) match {
        case (TypeRef(pre, _, _), _) if !pre.toString.isEmpty =>
          sanitizePath(pre.toString.split("\\.").toList)
        case (SingleType(_, _), sym) =>
          symbolPath(sym).dropRight(1)
        case (_, _) =>
          Nil
      }

    def ownerPath: List[String] = {
      val chain = sym.ownerChain.reverse
      val parts = chain.map(_.name.decodedName.toString)
      val (paths, names) = parts.splitAt(
        Math.max(0, parts.size - 1),
      )
      paths
    }

    def shortName: String = {
      val prefixes = tt.prefixString.split('.').dropRight(1)
      val prefix = prefixes.mkString(".") + "."
      val name = tt.safeToString
      name.stripPrefix(prefix)
    }
  }

  def stripType(tpe: Type): (List[String], String) =
    tpe match {
      case tt: SingletonType =>
        val sym = tt.termSymbol
        val parts = TypeParts(sym, tt)

        parts.modulePath -> parts.shortName

      case tt: RefinedType =>
        val sym = tt.typeSymbol
        val parts = TypeParts(sym, tt)

        parts.modulePath -> parts.shortName

      case _ =>
        // TODO: should this also use TypeParts ?
        val sym =
          if (tpe.takesTypeArgs)
            tpe.typeSymbolDirect
          else
            tpe.typeSymbol
        val symName = sym.name.decodedName.toString
        val parts = TypeParts(sym, tpe)

        val name =
          if (sym.isModuleClass)
            s"$symName.type"
          else
            symName
        (parts.modulePath, name)
    }

  def formatNormalSimple(tpe: Type): (List[String], String) =
    tpe match {
      case a @ WildcardType =>
        (Nil, a.toString)
      case a =>
        stripType(a)
    }

  def formatSimpleType(tpe: Type): (List[String], String) =
    if (isAux(tpe))
      formatAuxSimple(tpe)
    else
      formatNormalSimple(tpe)

  def indentLine(line: String, n: Int = 1, prefix: String = "  ") = (prefix * n) + line

  def indent(lines: List[String], n: Int = 1, prefix: String = "  ") = lines.map(indentLine(_, n, prefix))

  /**
   * If the args of an applied type constructor are multiline, create separate
   * lines for the constructor name and the closing bracket; else return a
   * single line.
   */
  def showTypeApply
  (cons: String, args: List[TypeRepr], break: Boolean)
  : TypeRepr = {
    val flatArgs = bracket(args map (_.flat))
    val flat = FlatType(s"$cons$flatArgs")
    def brokenArgs = args match {
      case head :: tail =>
        tail.foldLeft(head.lines)((z, a) => z ::: "," :: a.lines)
      case _ => Nil
    }
    def broken = BrokenType(s"$cons[" :: indent(brokenArgs) ::: List("]"))
    if (break) decideBreak(flat, broken) else flat
  }

  def showTuple(args: List[String]) =
    args match {
      case head :: Nil =>
        s"Tuple1[$head]"
      case _ =>
        args.mkString("(", ",", ")")
    }

  def showFuncParams(args: List[String]) =
    args match {
      case head :: Nil =>
        head
      case _ =>
        args.mkString("(", ",", ")")
    }

  def showRefined(parents: List[String], decls: List[String]) = {
    val p = parents.mkString(" with ")
    val d =
      if (decls.isEmpty)
        ""
      else
        decls.mkString(" {", "; ", "}")
    s"$p$d"
  }

  def bracket[A](params: List[A]) = params.mkString("[", ", ", "]")

  def formatFunction(args: List[String]) = {
    val (params, returnt) = args.splitAt(args.length - 1)
    s"${showTuple(params)} => ${showTuple(returnt)}"
  }

  def decideBreak(flat: FlatType, broken: => BrokenType): TypeRepr =
    if (flat.length > breakInfixLength) broken
    else flat

  /**
   * Turn a nested infix type structure into a flat list
   * ::[A, ::[B, C]]] => List(A, ::, B, ::, C)
   */
  def flattenInfix(tpe: Infix): List[Formatted] = {
    def step(tpe: Formatted): List[Formatted] = tpe match {
      case Infix(infix, left, right, top) =>
        left :: infix :: step(right)
      case a => List(a)
    }
    step(tpe)
  }

  /**
   * Break a list produced by [[flattenInfix]] into lines by taking two
   * elements at a time, then appending the terminal.
   * If the expression's length is smaller than the threshold specified via
   * plugin parameter, return a single line.
   */
  def breakInfix(types: List[Formatted]): TypeRepr = {
    val form = types map showFormattedLBreak
    def broken: List[String] = form
      .sliding(2, 2)
      .toList
      .flatMap {
        case left :: right :: Nil =>
          (left, right) match {
            case (FlatType(tpe), FlatType(infix)) =>
              List(s"$tpe $infix")
            case _ => left.lines ++ right.lines
          }
        case last :: Nil => last.lines
        // for exhaustiveness, cannot be reached
        case l => l.flatMap(_.lines)
      }
    val flat = FlatType(form.flatMap(_.lines) mkString " ")
    decideBreak(flat, BrokenType(broken))
  }

  val showFormattedLCache = FormatCache[(Formatted, Boolean), TypeRepr]

  def truncateDecls(decls: List[Formatted]): Boolean = splainSettingTruncRefined.exists(_ < decls.map(_.length).sum)

  def showFormattedQualified(path: List[String], name: String): TypeRepr =
    FlatType(name)

  def formattedDiff: (Formatted, Formatted) => String = {
    case (Qualified(lpath, lname), Qualified(rpath, rname)) if lname == rname =>
      val prefix =
        lpath
          .reverse
          .zip(rpath.reverse)
          .takeWhile { case (l, r) =>
            l == r
          }
          .size + 1
      s"${qualifiedName(lpath.takeRight(prefix), lname).red}|${qualifiedName(rpath.takeRight(prefix), rname).green}"
    case (left, right) =>
      val l = showFormattedNoBreak(left)
      val r = showFormattedNoBreak(right)
      s"${l.red}|${r.green}"
  }

  def showFormattedLImpl(tpe: Formatted, break: Boolean): TypeRepr =
    tpe match {
      case Simple(name) =>
        FlatType(name)
      case Qualified(Nil, name) =>
        FlatType(name)
      case Qualified(path, name) =>
        showFormattedQualified(path, name)
      case Applied(cons, args) =>
        val reprs = args.map(showFormattedL(_, break))
        showTypeApply(showFormattedNoBreak(cons), reprs, break)
      case tpe @ Infix(_, _, _, top) =>
        val flat = flattenInfix(tpe)
        val broken: TypeRepr =
          if (break)
            breakInfix(flat)
          else
            FlatType(flat.map(showFormattedNoBreak).mkString(" "))
        wrapParensRepr(broken, top)
      case UnitForm =>
        FlatType("Unit")
      case FunctionForm(args, ret, top) =>
        val a = showFuncParams(args.map(showFormattedNoBreak))
        val r = showFormattedNoBreak(ret)
        FlatType(wrapParens(s"$a => $r", top))
      case TupleForm(elems) =>
        FlatType(showTuple(elems.map(showFormattedNoBreak)))
      case RefinedForm(elems, decls) if truncateDecls(decls) =>
        FlatType(showRefined(elems.map(showFormattedNoBreak), List("...")))
      case RefinedForm(elems, decls) =>
        FlatType(showRefined(elems.map(showFormattedNoBreak), decls.map(showFormattedNoBreak)))
      case Diff(left, right) =>
        FlatType(formattedDiff(left, right))
      case Decl(sym, rhs) =>
        val s = showFormattedNoBreak(sym)
        val r = showFormattedNoBreak(rhs)
        FlatType(s"type $s = $r")
      case DeclDiff(sym, left, right) =>
        val s = showFormattedNoBreak(sym)
        val diff = formattedDiff(left, right)
        FlatType(s"type $s = $diff")
      case ByName(tpe) =>
        val t = showFormattedNoBreak(tpe)
        FlatType(s"(=> $t)")
    }

  def showFormattedL(tpe: Formatted, break: Boolean): TypeRepr = {
    val key = (tpe, break)
    showFormattedLCache(key, showFormattedLImpl(tpe, break))
  }

  def showFormattedLBreak(tpe: Formatted): TypeRepr = showFormattedL(tpe, true)

  def showFormattedLNoBreak(tpe: Formatted): TypeRepr = showFormattedL(tpe, false)

  def showFormatted(tpe: Formatted, break: Boolean): String = showFormattedL(tpe, break).joinLines

  def showFormattedNoBreak(tpe: Formatted): String = showFormattedLNoBreak(tpe).tokenize

  def showType(tpe: Type): String = showFormatted(formatType(tpe, true), false)

  def showTypeBreak(tpe: Type): String = showFormatted(formatType(tpe, true), true)

  def showTypeBreakL(tpe: Type): List[String] = showFormattedL(formatType(tpe, true), true).lines

  def wrapParens(expr: String, top: Boolean): String =
    if (top)
      expr
    else
      s"($expr)"

  def wrapParensRepr(tpe: TypeRepr, top: Boolean): TypeRepr =
    tpe match {
      case FlatType(tpe) =>
        FlatType(wrapParens(tpe, top))
      case BrokenType(lines) =>
        if (top)
          tpe
        else
          BrokenType("(" :: indent(lines) ::: List(")"))
    }

  val specialFormatters: List[SpecialFormatter] =
    List(
      FunctionFormatter,
      TupleFormatter,
      RefinedFormatter,
      ByNameFormatter,
    )

  def formatSpecial[A](tpe: Type, simple: String, args: List[A], formattedArgs: => List[Formatted], top: Boolean,
    rec: A => Boolean => Formatted)
  : Option[Formatted] = {
    specialFormatters
      .map(_.apply(tpe, simple, args, formattedArgs, top, rec))
      .collectFirst { case Some(a) => a }
      .headOption
  }

  def formatInfix[A](
    path: List[String],
    simple: String,
    left: A,
    right: A,
    top: Boolean,
    rec: A => Boolean => Formatted,
  ) = {
    val l = rec(left)(false)
    val r = rec(right)(false)
    Infix(Qualified(path, simple), l, r, top)
  }

  def formatWithInfix[A](tpe: Type, args: List[A], top: Boolean, rec: A => Boolean => Formatted): Formatted = {
    val (path, simple) = formatSimpleType(tpe)
    lazy val formattedArgs = args.map(rec(_)(true))
    formatSpecial(tpe, simple, args, formattedArgs, top, rec).getOrElse {
      args match {
        case left :: right :: Nil if isSymbolic(tpe) =>
          formatInfix(path, simple, left, right, top, rec)
        case _ :: _ =>
          Applied(Qualified(path, simple), formattedArgs)
        case _ =>
          Qualified(path, simple)
      }
    }
  }

  def formatTypeImpl(tpe: Type, top: Boolean): Formatted = {
    val dtpe = dealias(tpe)
    val rec = (tp: Type) => (t: Boolean) => formatType(tp, t)
    formatWithInfix(dtpe, extractArgs(dtpe), top, rec)
  }

  val formatTypeCache = FormatCache[(Type, Boolean), Formatted]

  def formatType(tpe: Type, top: Boolean): Formatted = {
    val key = (tpe, top)
    formatTypeCache(key, formatTypeImpl(tpe, top))
  }

  def formatDiffInfix(left: Type, right: Type, top: Boolean): Formatted = {
    val rec = (l: Type, r: Type) => (t: Boolean) => formatDiff(l, r, t)
    val recT = rec.tupled
    val args = extractArgs(left) zip extractArgs(right)
    formatWithInfix(left, args, top, recT)
  }

  def formatDiffSpecial(left: Type, right: Type, top: Boolean): Option[Formatted] = {
    specialFormatters.map(_.diff(left, right, top))
      .collectFirst { case Some(a) => a }
      .headOption
  }

  def formatDiffSimple(left: Type, right: Type): Formatted = {
    val l = formatType(left, true)
    val r = formatType(right, true)
    Diff(l, r)
  }

  def formatDiffImpl(found: Type, req: Type, top: Boolean): Formatted = {
    val (left, right) = dealias(found) -> dealias(req)
    if (left =:= right)
      formatType(left, top)
    else if (left.typeSymbol == right.typeSymbol)
      formatDiffInfix(left, right, top)
    else
      formatDiffSpecial(left, right, top) getOrElse
        formatDiffSimple(left, right)
  }

  val formatDiffCache = FormatCache[(Type, Type, Boolean), Formatted]

  def formatDiff(left: Type, right: Type, top: Boolean): Formatted = {
    val key = (left, right, top)
    formatDiffCache(key, formatDiffImpl(left, right, top))
  }

  def formatNonConfBounds(err: ImplicitErrorSpecifics.NonconformantBounds): List[String] = {
    val params = bracket(err.tparams.map(_.defString))
    val tpes = bracket(err.targs map showType)
    List("nonconformant bounds;", tpes.red, params.green)
  }

  def formatNestedImplicit(err: ImplicitError): (String, List[String], Int) = {
    val candidate = ImplicitError.cleanCandidate(err)
    val problem = s"${candidate.red} invalid because"
    val reason = err.specifics match {
      case e: ImplicitErrorSpecifics.NotFound =>
        implicitMessage(e.param, NoImplicitFoundAnnotation(err.candidate, e.param).map(_._2))
      case e: ImplicitErrorSpecifics.NonconformantBounds =>
        formatNonConfBounds(e)
    }
    (problem, reason, err.nesting)
  }

  def hideImpError(error: ImplicitError): Boolean =
    error.specifics match {
      case ImplicitErrorSpecifics.NonconformantBounds(_, _, _) => true
      case ImplicitErrorSpecifics.NotFound(_) => false
    }

  def indentTree(tree: List[(String, List[String], Int)], baseIndent: Int): List[String] = {
    val nestings = tree.map(_._3).distinct.sorted
    tree
      .flatMap {
        case (head, tail, nesting) =>
          val ind = baseIndent + nestings.indexOf(nesting).abs
          indentLine(head, ind, "――") :: indent(tail, ind)
      }
  }

  def formatIndentTree(chain: List[ImplicitError], baseIndent: Int) = {
    val formatted = chain map formatNestedImplicit
    indentTree(formatted, baseIndent)
  }

  def deepestLevel(chain: List[ImplicitError]) = {
    chain.foldLeft(0)((z, a) => if (a.nesting > z) a.nesting else z)
  }

  def formatImplicitChainTreeCompact(chain: List[ImplicitError]): Option[List[String]] = {
    chain
      .headOption
      .map { head =>
        val max = deepestLevel(chain)
        val leaves = chain.drop(1).dropWhile(_.nesting < max)
        val base = if (head.nesting == 0) 0 else 1
        val (fhh, fht, fhn) = formatNestedImplicit(head)
        val spacer = if (leaves.nonEmpty && leaves.length < chain.length) List("⋮".blue) else Nil
        val fh = (fhh, fht ++ spacer, fhn)
        val ft = leaves map formatNestedImplicit
        indentTree(fh :: ft, base)
      }
  }

  def formatImplicitChainTreeFull(chain: List[ImplicitError]): List[String] = {
    val baseIndent = chain.headOption.map(_.nesting).getOrElse(0)
    formatIndentTree(chain, baseIndent)
  }

  def formatImplicitChainFlat(chain: List[ImplicitError]): List[String] = {
    chain map formatNestedImplicit flatMap { case (h, t, _) => h :: t }
  }

  def formatImplicitChain(chain: List[ImplicitError]): List[String] = {
    val compact = if (settings.implicitsSettingVerboseTree) None else formatImplicitChainTreeCompact(chain)
    compact getOrElse formatImplicitChainTreeFull(chain)
  }

  /**
   * Remove duplicates and special cases that should not be shown.
   * In some cases, candidates are reported twice, once as `Foo.f` and once as
   * `f`. `ImplicitError.equals` checks the simple names for identity, which
   * is suboptimal, but works for 99% of cases.
   * Special cases are handled in [[hideImpError]]
   */
  def formatNestedImplicits(errors: List[ImplicitError]) = {
    val visible = errors filterNot hideImpError
    val chains = splitChains(visible).map(_.distinct).distinct
    chains map formatImplicitChain flatMap ("" :: _) drop 1
  }

  def formatImplicitParam(sym: Symbol) = sym.name.toString

  def effectiveImplicitType(tpe: Type) = {
    if (tpe.typeSymbol.name.toString == "Lazy")
      tpe.typeArgs.headOption.getOrElse(tpe)
    else tpe
  }

  def implicitMessage(param: Symbol, annotationMsg: Option[String]): List[String] = {
    val tpe = param.tpe
    val msg = annotationMsg match {
      case Some(msg) => msg.split("\n").toList.map(_.blue) ++ List("")
      case _ => Nil
    }
    val effTpe = effectiveImplicitType(tpe)
    val paramName = formatImplicitParam(param)
    val bang = "!"
    val i = "I"
    val head = s"${bang.red}${i.blue} ${paramName.yellow}:"
    val lines = showTypeBreakL(effTpe) match {
      case single :: Nil => List(s"$head ${single.green}")
      case l => head :: indent(l).map(_.green)
    }
    lines ++ indent(msg)
  }

  def splitChains(errors: List[ImplicitError]): List[List[ImplicitError]] = {
    errors.foldRight(Nil: List[List[ImplicitError]]) {
      case (a, chains @ ((chain @ (prev :: _)) :: tail)) =>
        if (a.nesting > prev.nesting) List(a) :: chains
        else (a :: chain) :: tail
      case (a, _) =>
        List(List(a))
    }
  }

  def formatImplicitError(param: Symbol, errors: List[ImplicitError], annotationMsg: Option[String]) = {
    val stack = formatNestedImplicits(errors)
    val nl = if (errors.nonEmpty) "\n" else ""
    val ex = stack.mkString("\n")
    val pre = "implicit error;\n"
    val msg = implicitMessage(param, annotationMsg).mkString("\n")
    s"$pre$msg$nl$ex"
  }

  def cacheStats = {
    val sfl = showFormattedLCache.stats
    val ft = formatTypeCache.stats
    val df = formatDiffCache.stats
    s"showFormatted -> $sfl, formatType -> $ft, formatDiff -> $df"
  }
}

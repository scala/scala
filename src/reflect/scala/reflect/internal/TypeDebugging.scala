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

package scala
package reflect
package internal

import util._

trait TypeDebugging {
  self: SymbolTable =>

  import definitions._

  /** There's a whole lot of implementation detail which is nothing but noise when
   *  you are trying to see what's going on. This is my attempt to filter it out.
   */
  object noPrint extends (Tree => Boolean) {
    def skipScalaName(name: Name) = name match {
      case tpnme.Any | tpnme.Nothing | tpnme.AnyRef => true
      case _                                        => false
    }
    def skipRefTree(t: RefTree) = t match {
      case Select(Select(Ident(nme.ROOTPKG), nme.scala_), name) if skipScalaName(name) => true
      case Select(sel, name) if sel.symbol == ScalaPackage && skipScalaName(name)      => true
      case Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR)              => true
      case Ident(nme.ROOTPKG)                                                          => true
      case _                                                                           => skipSym(t.symbol)
    }
    def skipSym(sym: Symbol): Boolean = sym match {
      case null                    => false
      case NothingClass | AnyClass => true
      case PredefModule            => true
      case ObjectClass             => true
      case _                       => sym.hasPackageFlag
    }
    def skipType(tpe: Type): Boolean = (tpe eq null) || skipSym(tpe.typeSymbolDirect)

    def skip(t: Tree): Boolean = t match {
      case EmptyTree                                          => true
      case PackageDef(_, _)                                   => true
      case t: RefTree                                         => skipRefTree(t)
      case TypeBoundsTree(lo, hi)                             => skip(lo) && skip(hi)
      case Block(Nil, expr)                                   => skip(expr)
      case Apply(fn, Nil)                                     => skip(fn)
      case Block(stmt :: Nil, expr)                           => skip(stmt) && skip(expr)
      case DefDef(_, nme.CONSTRUCTOR, Nil, ListOfNil, _, rhs) => skip(rhs)
      case Literal(Constant(()))                              => true
      case tt @ TypeTree()                                    => skipType(tt.tpe)
      case _                                                  => skipSym(t.symbol)
    }
    def apply(t: Tree) = skip(t)
  }

  /** Light color wrappers.
   */
  object typeDebug extends TypeDebugging.AnsiColor {
    private def to_s(x: Any): String = x match {
      // otherwise case classes are caught looking like products
      case _: Tree | _: Type     => "" + x
      case x: IterableOnce[_]    => x.iterator mkString ", "
      case x: Product            => x.productIterator.mkString("(", ", ", ")")
      case _                     => "" + x
    }
    def ptBlock(label: String, pairs: (String, Any)*): String = {
      if (pairs.isEmpty) label + "{ }"
      else {
        val width = (pairs map (_._1.length)).max
        val fmt   = "%-" + (width + 1) + "s %s"
        val strs  = pairs map { case (k, v) => fmt.format(k, to_s(v)) }

        strs.mkString(label + " {\n  ", "\n  ", "\n}")
      }
    }
    def ptLine(pairs: (String, Any)*): String = (
      pairs
              map { case (k,  v) => (k, to_s(v)) }
        filterNot { case (_,  v) => v == "" }
              map { case ("", v) => v ; case (k, v) => s"$k=$v" }
        mkString ", "
    )
    def ptTree(t: Tree): String = t match {
      case PackageDef(pid, _)                                                      => s"package $pid"
      case ModuleDef(_, name, _)                                                   => s"object $name"
      case DefDef(_, name, tparams, _, _, _)                                       => "def " + name + ptTypeParams(tparams)
      case ClassDef(_, name, Nil, _) if t.symbol != null && t.symbol.isModuleClass => s"module class $name"
      case ClassDef(_, name, tparams, _)                                           => "class " + name + ptTypeParams(tparams)
      case td: TypeDef                                                             => ptTypeParam(td)
      case TypeBoundsTree(lo, hi)                                                  =>
        val lo_s = if (noPrint(lo)) "" else " >: " + ptTree(lo)
        val hi_s = if (noPrint(hi)) "" else " <: " + ptTree(hi)
        lo_s + hi_s
      case _ if (t.symbol eq null) || (t.symbol eq NoSymbol) => to_s(t)
      case _                                                 => "" + t.symbol.rawInfo.safeToString
    }
    def ptTypeParam(td: TypeDef): String = {
      val TypeDef(_, name, tparams, rhs) = td
      name.toString + ptTypeParams(tparams) + ptTree(rhs)
    }
    def ptTypeParams(tparams: List[TypeDef]): String = str brackets (tparams map ptTypeParam)

    object str {
      def parentheses(xs: List[_]): String     = xs.mkString("(", ", ", ")")
      def params(params: List[Symbol]): String = {
        val paramsStrPre = if (params.nonEmpty && params.head.isImplicit) "(implicit " else "("
        params.map(_.defStringWithoutImplicit).mkString(paramsStrPre, ", ", ")")
      }
      def brackets(xs: List[_]): String        = if (xs.isEmpty) "" else xs.mkString("[", ", ", "]")
      def tparams(tparams: List[Type]): String = brackets(tparams map debug)
      def parents(ps: List[Type]): String      = (ps map debug).mkString(" with ")
      def refine(defs: Scope): String          = defs.toList.mkString("{", " ;\n ", "}")
      def bounds(lo: Type, hi: Type): String   = {
        val lo_s = if (lo.isNothing) "" else s" >: $lo"
        val hi_s = if (typeIsAnyOrJavaObject(hi)) "" else s" <: $hi"
        lo_s + hi_s
      }
    }
    import str._
    private def debug(tp: Type): String = tp match {
      case TypeRef(pre, sym, args)         => s"${debug(pre)}.${sym.nameString}.${tparams(args)}"
      case ThisType(sym)                   => s"${sym.nameString}.this"
      case SingleType(pre, sym)            => s"${debug(pre)}.${sym.nameString}.type"
      case RefinedType(ps, decls)          => s"${parents(ps)} ${refine(decls)}"
      case ClassInfoType(ps, decls, clazz) => s"class ${clazz.nameString} ${parents(ps)} ${refine(decls)}"
      case PolyType(tparams, result)       => s"${brackets(tparams)}${debug(result)}"
      case TypeBounds(lo, hi)              => bounds(lo, hi)
      case tv @ TypeVar(_, _)              => "" + tv
      case ExistentialType(tparams, qtpe)  => s"forSome ${brackets(tparams)} ${debug(qtpe)}"
      case _                               => s"?${shortClassOfInstance(tp)}?" // tp.toString might produce cyclic error...
    }
    def debugString(tp: Type) = debug(tp)
  }
  def paramString(tp: Type)      = typeDebug.str params tp.params
  def typeParamsString(tp: Type) = typeDebug.str.brackets(tp.typeParams.map(_.defString))
  def debugString(tp: Type)      = typeDebug debugString tp
}

object TypeDebugging {
  object AnsiColor extends AnsiColor {
    implicit class StringColorOps(private val s: String) extends AnyVal {
      def red    = inLightRed(s)
      def green  = inLightGreen(s)
      def yellow = inLightYellow(s)
      def blue   = inLightBlue(s)
    }
  }

  trait AnsiColor extends scala.io.AnsiColor {
    private[this] val colorsOk = scala.util.Properties.coloredOutputEnabled
    private def inColor(s: String, color: String) = if (colorsOk && s != "") color +        s + RESET else s
    private def inBold(s: String, color: String)  = if (colorsOk && s != "") color + BOLD + s + RESET else s

    def inLightRed(s: String)            = inColor(s, RED)
    def inLightBlue(s: String)           = inColor(s, BLUE)
    def inLightGreen(s: String)          = inColor(s, GREEN)
    def inLightYellow(s: String): String = inColor(s, YELLOW)
    def inLightMagenta(s: String)        = inColor(s, MAGENTA)
    def inLightCyan(s: String): String   = inColor(s, CYAN)
    def inGreen(s: String): String       = inBold(s, GREEN)
    def inRed(s: String): String         = inBold(s, RED)
    def inBlue(s: String): String        = inBold(s, BLUE)
    def inCyan(s: String): String        = inBold(s, CYAN)
    def inMagenta(s: String)             = inBold(s, MAGENTA)
    def resetColor(s: String): String    = if (colorsOk) s + RESET else s
  }
}

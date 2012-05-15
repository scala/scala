/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.reflect
package internal

trait TypeDebugging {
  self: SymbolTable =>

  import definitions._

  // @M toString that is safe during debugging (does not normalize, ...)
  object typeDebug {
    private def to_s(x: Any): String = x match {
      // otherwise case classes are caught looking like products
      case _: Tree | _: Type     => "" + x
      case x: TraversableOnce[_] => x mkString ", "
      case x: Product            => x.productIterator mkString ("(", ", ", ")")
      case _                     => "" + x
    }
    def ptIndent(x: Any) = ("" + x).replaceAll("\\n", "  ")
    def ptBlock(label: String, pairs: (String, Any)*): String = {
      if (pairs.isEmpty) label + "{ }"
      else {
        val width = pairs map (_._1.length) max
        val fmt   = "%-" + (width + 1) + "s %s"
        val strs  = pairs map { case (k, v) => fmt.format(k, to_s(v)) }

        strs.mkString(label + " {\n  ", "\n  ", "\n}")
      }
    }
    def ptLine(label: String, pairs: (String, Any)*): String = {
      val strs = pairs map { case (k, v) => k + "=" + to_s(v) }
      strs.mkString(label + ": ", ", ", "")
    }
    def ptTree(t: Tree) = t match {
      case PackageDef(pid, _)            => "package " + pid
      case ModuleDef(_, name, _)         => "object " + name
      case ClassDef(_, name, tparams, _) => "class " + name + str.brackets(tparams)
      case _                             => to_s(t)
    }

    object str {
      def parentheses(xs: List[_]): String     = xs.mkString("(", ", ", ")")
      def brackets(xs: List[_]): String        = if (xs.isEmpty) "" else xs.mkString("[", ", ", "]")
      def tparams(tparams: List[Type]): String = brackets(tparams map debug)
      def parents(ps: List[Type]): String      = (ps map debug).mkString(" with ")
      def refine(defs: Scope): String          = defs.toList.mkString("{", " ;\n ", "}")
    }

    private def debug(tp: Type): String = tp match {
      case TypeRef(pre, sym, args)             => debug(pre) + "." + sym.nameString + str.tparams(args)
      case ThisType(sym)                       => sym.nameString + ".this"
      case SingleType(pre, sym)                => debug(pre) +"."+ sym.nameString +".type"
      case RefinedType(parents, defs)          => str.parents(parents) + str.refine(defs)
      case ClassInfoType(parents, defs, clazz) => "class "+ clazz.nameString + str.parents(parents) + str.refine(defs)
      case PolyType(tparams, result)           => str.brackets(tparams) + " " + debug(result)
      case TypeBounds(lo, hi)                  => ">: "+ debug(lo) +" <: "+ debug(hi)
      case tv @ TypeVar(_, _)                  => tv.toString
      case ExistentialType(tparams, qtpe)      => "forSome "+ str.brackets(tparams) + " " + debug(qtpe)
      case _                                   => "?"+tp.getClass.getName+"?"//tp.toString might produce cyclic error...
    }
    def debugString(tp: Type) = debug(tp)
  }
  def paramString(tp: Type)      = typeDebug.str parentheses (tp.params map (_.defString))
  def typeParamsString(tp: Type) = typeDebug.str brackets (tp.typeParams map (_.defString))
  def typeArgsString(tp: Type)   = typeDebug.str brackets (tp.typeArgs map (_.safeToString))
  def debugString(tp: Type)      = typeDebug debugString tp
}

/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package models

import scala.collection.mutable.{HashMap, HashSet}
import scala.tools.nsc.{Global => Compiler}
import scala.tools.nsc.symtab.{Flags, Names}
import scala.tools.nsc.util.{ Position, SourceFile }
import scala.reflect.NameTransformer

/** This class ...
 *
 *  @author  Sean McDirmid
 *  @version 1.0
 */
class Signatures(val compiler: Compiler) {
  import compiler._

  class Signature(val name: String, val children: List[Signature]) {
    def asString: String = name + "[" + asString0(children) + "]"
  }

  def sort(sigs: List[Signature]) = sigs sortBy (_.name) reverse

  def asString0(sigs: List[Signature]): String =
    sort(sigs) map (_.asString) mkString

  def signature(unit: CompilationUnit): String =
    asString0(signature(unit.body, Nil))

  def signature(trees: List[Tree]): List[Signature] = {
    var ret : List[Signature] = Nil
    for (tree <- trees) ret = signature(tree, ret)
    ret
  }

  /**
   *  @param tree0 ...
   *  @param rest  ...
   *  @return      ...
   */
  def signature(tree0: Tree, rest: List[Signature]): List[Signature] = tree0 match {
    case tree: MemberDef => if (!tree.mods.isPrivate) {
      val name = "" + tree.name + "::" +
        (tree.mods &~ Flags.SYNTHETIC)

      val children: List[Signature] = tree match {
          case impl: ImplDef
            //if (!impl.name.toString.contains("$anonfun$")) =>
            if (impl.name.pos("$anonfun$") == name.length) =>
          val supers = new Signature("$$supers", signature(impl.impl.parents))
          val body   = new Signature("$$body",   signature(impl.impl.body))
          val ret = supers :: body :: Nil
          impl match {
            case cdef: ClassDef =>
              new Signature("$$tparams", signature(cdef.tparams)) :: ret
            case  _ =>
              ret
          }
        case vdef: ValOrDefDef =>
          val ret = signature(vdef.tpt, Nil)
          vdef match {
            case ddef : DefDef =>
              val tparams = new Signature("$$tparams", signature(ddef.tparams))
              var vparamss : List[Signature] = Nil
              for (list <- ddef.vparamss)
                vparamss = signature(list) ::: vparamss
              new Signature("$$ret", ret) :: tparams :: vparamss
            case _ =>
              ret
          }
        case pdef: PackageDef => signature(pdef.stats)
        case _ => Nil
      }
      new Signature(name, children) :: rest

    } else rest
    case tree: TypeTree => new Signature("" + tree.tpe, Nil) :: rest
    case _ => rest
  }
}

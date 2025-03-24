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

import scala.annotation.tailrec

object Formatted {
  @tailrec def comparator(formatted: Formatted): String = formatted match {
    case Infix(left, _, _, _)       => comparator(left)
    case Simple(tpe)                => tpe.name
    case Qualified(Nil, tpe)        => tpe.name
    case Qualified(path, tpe)       => s"${path.mkString}${tpe.name}"
    case UnitForm                   => "()"
    case Applied(cons, _)           => comparator(cons)
    case TupleForm(Nil)             => "()"
    case TupleForm(h :: _)          => comparator(h)
    case FunctionForm(Nil, ret, _)  => comparator(ret)
    case FunctionForm(h :: _, _, _) => comparator(h)
    case RefinedForm(Nil, _)        => "()"
    case RefinedForm(h :: _, _)     => comparator(h)
    case Diff(l, _)                 => comparator(l)
    case Decl(sym, _)               => comparator(sym)
    case DeclDiff(sym, _, _)        => comparator(sym)
    case ByName(tpe)                => comparator(tpe)
  }

  implicit val Ord: Ordering[Formatted] = (x, y) => Ordering[String].compare(comparator(x), comparator(y))
}

sealed trait Formatted {
  def length: Int = this match {
    case Infix(infix, left, right, top) => infix.length + left.length + right.length + 2
    case Simple(tpe)                    => tpe.name.length
    case Qualified(path, tpe)           => path.map(_.length).sum + path.length + tpe.name.length
    case UnitForm                       => 4
    case Applied(cons, args)            =>  args.map(_.length).sum + ( args.length - 1) * 2 + cons.length + 2
    case TupleForm(elems)               => elems.map(_.length).sum + (elems.length - 1) + 2
    case FunctionForm(args, ret, top)   =>  args.map(_.length).sum + ( args.length - 1) + 2 + ret.length + 4
    case RefinedForm(elems, decls)      => elems.map(_.length).sum + (elems.length - 1) * 6
    case Diff(lhs, rhs)                 => lhs.length + rhs.length + 1
    case Decl(sym, rhs)                 => sym.length + rhs.length + 8
    case DeclDiff(sym, lhs, rhs)        => sym.length + lhs.length + rhs.length + 9
    case ByName(tpe)                    => tpe.length + 5
  }
}

sealed trait FormattedName { val name: String }
case class SimpleName(name: String) extends FormattedName
case class InfixName(name: String) extends FormattedName

case class Infix(infix: Formatted, left: Formatted, right: Formatted, top: Boolean) extends Formatted
case class Simple(tpe: FormattedName)                                               extends Formatted
case class Qualified(path: List[String], tpe: FormattedName)                        extends Formatted
case object UnitForm                                                                extends Formatted
case class Applied(cons: Formatted, args: List[Formatted])                          extends Formatted
case class TupleForm(elems: List[Formatted])                                        extends Formatted
case class FunctionForm(args: List[Formatted], ret: Formatted, top: Boolean)        extends Formatted
case class RefinedForm(elems: List[Formatted], decls: List[Formatted])              extends Formatted
case class Diff(left: Formatted, right: Formatted)                                  extends Formatted
case class Decl(sym: Formatted, rhs: Formatted)                                     extends Formatted
case class DeclDiff(sym: Formatted, left: Formatted, right: Formatted)              extends Formatted
case class ByName(tpe: Formatted)                                                   extends Formatted

sealed trait TypeRepr {
  def flat: String
  def lines: List[String]
  def tokenize: String  = lines.mkString(" ")
  def joinLines: String = lines.mkString("\n")
  def indent: TypeRepr
}

case class BrokenType(lines: List[String]) extends TypeRepr {
  def flat   = lines.mkString(" ")
  def indent = BrokenType(lines.map("  " + _))
}

case class FlatType(flat: String) extends TypeRepr {
  def lines  = List(flat)
  def indent = FlatType(s"  $flat")
}

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

sealed trait Formatted
{
  def length: Int
}

object Formatted {
  def comparator: Formatted => String = {
    case Infix(left, _, _, _) =>
      comparator(left)
    case Simple(tpe) =>
      tpe
    case Qualified(Nil, tpe) =>
      tpe
    case Qualified(path, tpe) =>
      s"${path.mkString}$tpe"
    case UnitForm =>
      "()"
    case Applied(cons, _) =>
      comparator(cons)
    case TupleForm(Nil) =>
      "()"
    case TupleForm(h :: _) =>
      comparator(h)
    case FunctionForm(Nil, ret, _) =>
      comparator(ret)
    case FunctionForm(h :: _, _, _) =>
      comparator(h)
    case RefinedForm(Nil, _) =>
      "()"
    case RefinedForm(h :: _, _) =>
      comparator(h)
    case Diff(l, _) =>
      comparator(l)
    case Decl(sym, _) =>
      comparator(sym)
    case DeclDiff(sym, _, _) =>
      comparator(sym)
    case ByName(tpe) =>
      comparator(tpe)
  }

  implicit def Ordering_Formatted: Ordering[Formatted] =
    new Ordering[Formatted] {
      def compare(x: Formatted, y: Formatted): Int = Ordering[String].compare(comparator(x), comparator(y))
    }
}

case class Infix(infix: Formatted, left: Formatted, right: Formatted,
  top: Boolean)
extends Formatted
{
  def length = List(infix, left, right).map(_.length).sum + 2
}

case class Simple(tpe: String)
extends Formatted
{
  def length = tpe.length
}

case class Qualified(path: List[String], tpe: String)
extends Formatted
{
  def length: Int = path.map(_.length).sum + path.length + tpe.length
}

case object UnitForm
extends Formatted
{
  def length = 4
}

case class Applied(cons: Formatted, args: List[Formatted])
extends Formatted
{
  def length = args.map(_.length).sum + (args.length - 1) * 2 + cons.length + 2
}

case class TupleForm(elems: List[Formatted])
extends Formatted
{
  def length = elems.map(_.length).sum + (elems.length - 1) + 2
}

case class FunctionForm(args: List[Formatted], ret: Formatted, top: Boolean)
extends Formatted
{
  def length = args.map(_.length).sum + (args.length - 1) + 2 + ret.length + 4
}

object FunctionForm
{
  def fromArgs(args: List[Formatted], top: Boolean) = {
    val (params, returnt) = args.splitAt(args.length - 1)
    FunctionForm(params, returnt.headOption.getOrElse(UnitForm), top)
  }
}

case class RefinedForm(elems: List[Formatted], decls: List[Formatted])
extends Formatted
{
  def length: Int = elems.map(_.length).sum + (elems.length - 1) * 6
}

case class Diff(left: Formatted, right: Formatted)
extends Formatted
{
  def length = left.length + right.length + 1
}

case class Decl(sym: Formatted, rhs: Formatted)
extends Formatted
{
  def length: Int = sym.length + rhs.length + 8
}

case class DeclDiff(sym: Formatted, left: Formatted, right: Formatted)
extends Formatted
{
  def length: Int = sym.length + left.length + right.length + 9
}

case class ByName(tpe: Formatted)
extends Formatted
{
  def length: Int = tpe.length + 5
}

sealed trait TypeRepr
{
  def broken: Boolean
  def flat: String
  def lines: List[String]
  def tokenize = lines mkString " "
  def joinLines = lines mkString "\n"
  def indent: TypeRepr
}

case class BrokenType(lines: List[String])
extends TypeRepr
{
  def broken = true
  def flat = lines mkString " "
  def indent = BrokenType(lines map ("  " + _))
}

case class FlatType(flat: String)
extends TypeRepr
{
  def broken = false
  def length = flat.length
  def lines = List(flat)
  def indent = FlatType("  " + flat)
}

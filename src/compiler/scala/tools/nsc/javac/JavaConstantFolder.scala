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
package javac

import scala.util.control.ControlThrowable

/** Evaluates the constant expressions (JLS 15.29) in initializers of `final` fields in Java sources,
 *  as parsed by `JavaParsers.constantExprOpt`, with the semantics of Java rather than of Scala.
 *
 *  This gives such fields the same constant type as they get when read from the `ConstantValue` attribute
 *  of the classfile that javac emits.
 */
abstract class JavaConstantFolder {
  val global: Global
  import global._
  import definitions._

  private object NotConstant extends ControlThrowable
  private def notConstant(): Nothing = throw NotConstant

  /** The value of the constant expression `tree`, or `null` if it is not a constant expression.
   *
   *  @param resolve the value of the constant variable referred to by a (qualified) name, or `null`
   */
  def apply(tree: Tree, resolve: Tree => Constant): Constant =
    try eval(tree, resolve)
    catch {
      case NotConstant => null
      case _: ArithmeticException => null // integer division by zero, which javac rejects
    }

  private def eval(tree: Tree, resolve: Tree => Constant): Constant = {
    def loop(tree: Tree): Constant = tree match {
      case Literal(c)                     => checked(c)
      case Ident(_) | Select(_, _)        => checked(resolve(tree))
      case Apply(Select(x, op), Nil)      => unop(op, loop(x))
      case Apply(Select(x, op), List(y))  => binop(op, loop(x), loop(y))
      case If(c, x, y)                    => conditional(loop(c), loop(x), loop(y))
      case Typed(x, tpt)                  => cast(loop(x), tpt)
      case _                              => notConstant()
    }
    loop(tree)
  }

  private def checked(c: Constant): Constant =
    if (c != null && c.tag >= BooleanTag && c.tag <= StringTag) c else notConstant()

  private def convert(c: Constant, tag: Int): Constant = tag match {
    case ByteTag   => Constant(c.byteValue)
    case ShortTag  => Constant(c.shortValue)
    case CharTag   => Constant(c.charValue)
    case IntTag    => Constant(c.intValue)
    case LongTag   => Constant(c.longValue)
    case FloatTag  => Constant(c.floatValue)
    case DoubleTag => Constant(c.doubleValue)
    case _         => notConstant()
  }

  private def nonNull(c: Constant): Constant = if (c != null) c else notConstant()
  private def numeric(c: Constant): Constant = if (c.isNumeric) c else notConstant()
  private def boolean(c: Constant): Boolean = if (c.tag == BooleanTag) c.booleanValue else notConstant()

  // JLS 5.6: numeric promotion
  private def unaryPromoted(c: Constant): Constant = convert(numeric(c), math.max(IntTag, c.tag))
  private def promotedTag(x: Constant, y: Constant): Int = math.max(IntTag, math.max(numeric(x).tag, numeric(y).tag))

  private def unop(op: Name, x: Constant): Constant =
    nonNull(constfold.foldUnop(op, if (op == nme.UNARY_!) x else unaryPromoted(x)))

  private def binop(op: Name, x: Constant, y: Constant): Constant = {
    import nme._
    if (op == ADD && (x.tag == StringTag || y.tag == StringTag))
      Constant(x.stringValue + y.stringValue) // JLS 5.1.11, which `Constant.stringValue` agrees with for primitives
    else if (x.tag == StringTag && y.tag == StringTag) op match {
      // string constants are interned, so reference equality is value equality
      case EQ => Constant(x.stringValue == y.stringValue)
      case NE => Constant(x.stringValue != y.stringValue)
      case _  => notConstant()
    }
    else nonNull(constfold.foldBinopUnchecked(op, x, y)) // numeric promotion and shifts agree with Java
  }

  // JLS 15.25
  private def conditional(c: Constant, x: Constant, y: Constant): Constant = {
    val result = if (boolean(c)) x else y
    if (x.tag == y.tag) result
    else {
      def isSubInt(tag: Int) = tag == ByteTag || tag == ShortTag || tag == CharTag
      def fits(c: Constant, tag: Int) = c.tag == IntTag && convert(c, tag).intValue == c.intValue
      val tag =
        if (isSubInt(x.tag) && fits(y, x.tag)) x.tag
        else if (isSubInt(y.tag) && fits(x, y.tag)) y.tag
        else if (Set(x.tag, y.tag) == Set(ByteTag, ShortTag)) ShortTag
        else promotedTag(x, y)
      convert(result, tag)
    }
  }

  // JLS 15.16, only casts to primitive types and to String
  private def cast(c: Constant, tpt: Tree): Constant = {
    def isString = tpt match {
      case Ident(tpnme.String) | Select(Select(Ident(nme.java), nme.lang), tpnme.String) => true
      case _ => false
    }
    val primitive = if (tpt.tpe == null) NoSymbol else tpt.tpe.typeSymbol
    if (primitive == BooleanClass) Constant(boolean(c))
    else if (primitive == ByteClass) convert(numeric(c), ByteTag)
    else if (primitive == ShortClass) convert(numeric(c), ShortTag)
    else if (primitive == CharClass) convert(numeric(c), CharTag)
    else if (primitive == IntClass) convert(numeric(c), IntTag)
    else if (primitive == LongClass) convert(numeric(c), LongTag)
    else if (primitive == FloatClass) convert(numeric(c), FloatTag)
    else if (primitive == DoubleClass) convert(numeric(c), DoubleTag)
    else if (isString && c.tag == StringTag) c
    else notConstant()
  }
}

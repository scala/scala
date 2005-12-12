/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.reflect;

object Print extends Function1[Any, String] {

  def apply (any: Any): String = {
    if (any.isInstanceOf[TypedCode[Any]])
      apply(any.asInstanceOf[TypedCode[Any]])
    else if (any.isInstanceOf[Code])
      apply(any.asInstanceOf[Code])
    else if (any.isInstanceOf[Symbol])
      apply(any.asInstanceOf[Symbol])
    else if (any.isInstanceOf[Type])
      apply(any.asInstanceOf[Type])
    else "UnknownAny"
  }

  def apply (typedCode: TypedCode[Any]): String =
    Print(typedCode.code);

  def apply (code: Code): String = code match {
    case reflect.Ident(sym) =>
      "Ident (" + Print(sym) + ")"
    case reflect.Select(qual, sym) =>
      "Select (" + Print(qual) + " from " + Print(sym) + ")"
    case reflect.Literal(value) =>
      "Literal (" + value + ")"
    case reflect.Apply(fun, args) =>
      ("Apply (" + Print(fun) + " on " +
       ((args match {
         case Nil => "nothing "
         case _ :: _ => args.map(Print).mkString("", ", ", "")
       }):String) + ")")
    case reflect.TypeApply(fun, args) =>
      ("TypeApply (" + Print(fun) + " on " +
       ((args match {
         case Nil => "nothing"
         case _ :: _ => args.map(Print).mkString("", ", ", "")
       }):String) + ")")
    case reflect.Function(params, body) =>
      ("Function (" +
       ((params match {
         case Nil => "nothing"
         case _ :: _ => params.map(Print).mkString("", ", ", "")
       }):String) + " is " + Print(body) + ")")
    case reflect.This(sym) =>
      "This (" + Print(sym) + ")"
    case _ => "UnknownCode"
  }

  def apply (symbol: Symbol): String = symbol match {
    case reflect.Class(name) =>
      "Class (" + name + ")"
    case reflect.Method(name, datatype) =>
      "Method (" + name + " of " + Print(datatype) + ")"
    case reflect.Field(name, datatype) =>
      "Field (" + name + " of " + Print(datatype) + ")"
    case reflect.TypeField(name, datatype) =>
      "TypeField (" + name + " of " + Print(datatype) + ")"
    case reflect.LocalValue(owner, name, datatype) =>
      ("LocalValue (" + name + " owned by " + Print(owner) +
       " of " + Print(datatype) + ")")
    case reflect.LocalMethod(owner, name, datatype) =>
      ("LocalMethod (" + name + " owned by " + Print(owner) +
       " of " + Print(datatype) + ")")
    case reflect.NoSymbol => "NoSymbol"
    case reflect.RootSymbol => "RootSymbol"
    case _ => "UnknownSymbol"
  }

  def apply (datatype: Type): String = datatype match {
    case reflect.NoPrefix => "NoPrefix"
    case reflect.NoType => "NoType"
    case reflect.NamedType(name) =>
      "NamedType (" + name + ")"
    case reflect.PrefixedType(prefix, symbol) =>
      "PrefixedType (" + Print(symbol) + " in " + Print(prefix) + ")"
    case reflect.SingleType(prefix, symbol) =>
      "SingleType (" + Print(symbol) + " in " + Print(prefix) + ")"
    case reflect.ThisType(clazz) =>
      "ThisType (" + Print(clazz) + ")"
    case reflect.AppliedType(datatype, args) =>
      ("AppliedType (" + Print(datatype) + " on " +
       ((args match {
         case Nil => "nothing"
         case _ :: _ => args.map(Print).mkString("", ", ", "")
       }):String) + ")")
    case reflect.TypeBounds(lo, hi) =>
      "TypeBounds (" + Print(lo) + " to " + Print(hi) + ")"
    case reflect.MethodType(formals, resultType) =>
      ("MethodType (" +
       ((formals match {
         case Nil => "nothing"
         case _ :: _ => formals.map(Print).mkString("", ", ", "")
       }):String) + " is " + Print(resultType) + ")")
    case reflect.PolyType(typeParams, typeBounds, resultType) =>
      ("PolyType (" + (typeParams zip typeBounds).map{
        case Pair(typeParam, Pair(lo, hi)) =>
          Print(lo) + " < " + Print(typeParam) + " < " + Print(hi)
      }.mkString("", ", ", "") + " to " + Print(resultType) + ")")
    case _ => "UnknownType"
  }

}

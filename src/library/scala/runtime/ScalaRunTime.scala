/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime;


import Predef.{Class, Throwable}
import java.lang.Runnable

object ScalaRunTime {

  /** Names for primitive types, used by array unboxing */
  val ByteTag = ".Byte";
  val ShortTag = ".Short";
  val CharTag = ".Char";
  val IntTag = ".Int";
  val LongTag = ".Long";
  val FloatTag = ".Float";
  val DoubleTag = ".Double";
  val BooleanTag = ".Boolean";

  val ByteTYPE = java.lang.Byte.TYPE
  val ShortTYPE = java.lang.Short.TYPE
  val CharTYPE = java.lang.Character.TYPE
  val IntTYPE = java.lang.Integer.TYPE
  val LongTYPE = java.lang.Long.TYPE
  val FloatTYPE = java.lang.Float.TYPE
  val DoubleTYPE = java.lang.Double.TYPE
  val BooleanTYPE = java.lang.Boolean.TYPE
  val UnitTYPE = java.lang.Void.TYPE

  def isValueTag(tag: String) = tag.charAt(0) == '.'
  def isValueClass(clazz: Class) = clazz.isPrimitive()

  abstract class Try[a] {
    def Catch[b >: a](handler: PartialFunction[Throwable, b]): b;
    def Finally(handler: Unit): a;
  }

  def Try[a](block: => a): Try[a] = new Try[a] with Runnable {
    var result: a = _;
    var exception: Throwable = ExceptionHandling.tryCatch(this);

    def run(): Unit = result = block;

    def Catch[b >: a](handler: PartialFunction[Throwable, b]): b =
      if (exception == null)
	result.asInstanceOf[b]
      // !!! else if (exception is LocalReturn)
      // !!!   // ...
      else if (handler isDefinedAt exception)
	handler(exception)
      else
	throw exception;

    def Finally(handler: Unit): a =
      if (exception == null)
        result.asInstanceOf[a]
      else
        throw exception;
  }

  def caseFields(x: CaseClass): List[Any] = {
    val arity = x.caseArity;
    def fields(from: Int): List[Any] =
      if (from >= arity) List()
      else x.caseElement(from) :: fields(from + 1);
    fields(0)
  }
  def caseFields(x: Product): List[Any] = {
    val arity = x.arity;
    def fields(from: Int): List[Any] =
      if (from > arity) List()
      else x.element(from) :: fields(from + 1);
    fields(1)
  }
  def _toStringCaseClass(x: CaseClass): String = {
    caseFields(x).mkString(x.caseName + "(", ",", ")")
  }
  def _toStringProduct(x: Product): String = {
    caseFields(x).mkString(x.productPrefix + "(", ",", ")")
  }
 /** only for bootstrapping 2.2.1 remove afterwards, keeping only _equalsProduct  */
  def _toString(x: AnyRef): String = x match {
    case xc: CaseClass => _toStringCaseClass(xc)
    case xp: Product   => _toStringProduct(xp)
  }
  def _hashCodeCaseClass(x: CaseClass): Int = {
    var code = x.getClass().hashCode();
    val arr =  x.caseArity
    var i = 0;
    while (i < arr) {
      code = code * 41 + x.caseElement(i).hashCode();
      i = i + 1
    }
    code
  }
  def _hashCodeProduct(x: Product): Int = {
    var code = x.getClass().hashCode();
    val arr =  x.arity
    var i = 1;
    while (i <= arr) {
      code = code * 41 + x.element(i).hashCode();
      i = i + 1
    }
    code
  }
 /** only for bootstrapping 2.2.1 remove afterwards, keeping only _equalsProduct  */
  def _hashCode(x: AnyRef): Int = x match {
    case xc: CaseClass => _hashCodeCaseClass(xc)
    case xp: Product   => _hashCodeProduct(xp)
  }
  def _equalsCaseClass(x: CaseClass, y: Any): Boolean = y match {
    case y1: CaseClass =>
      /*(x.getClass() eq y1.getClass() &&*/ {
	val arity = x.caseArity;
	var i = 0;
	while (i < arity && x.caseElement(i) == y1.caseElement(i))
	  i = i + 1;
	i == arity
      }
    case _ =>
      false
  }
  def _equalsProduct(x: Product, y: Any): Boolean = y match {
    case y1: Product if x.arity == y1.arity =>
      /*(x.getClass() eq y1.getClass() &&*/ {
	val arity = x.arity;
	var i = 1;
	while (i <= arity && x.element(i) == y1.element(i))
	  i = i + 1;
	i == arity + 1
      }
    case _ =>
      false
  }
 /** only for bootstrapping 2.2.1 remove afterwards, keeping only _equalsProduct  */
  def _equals(x: AnyRef, y: Any): Boolean = x match {
    case xc: CaseClass => _equalsCaseClass(xc, y)
    case xp: Product   => _equalsProduct(xp, y)
  }
  //def checkDefined[T >: Null](x: T): T =
  //  if (x == null) throw new UndefinedException else x

  def Seq[a](xs: a*): Seq[a] = null; // interpreted specially by new backend.

  def booleanValue(x: BoxedBoolean): Boolean = if (x eq null) false else x.booleanValue();
  def byteValue   (x: BoxedNumber ): Byte    = if (x eq null) 0     else x.byteValue();
  def shortValue  (x: BoxedNumber ): Short   = if (x eq null) 0     else x.shortValue();
  def charValue   (x: BoxedNumber ): Char    = if (x eq null) 0     else x.charValue();
  def intValue    (x: BoxedNumber ): Int     = if (x eq null) 0     else x.intValue();
  def longValue   (x: BoxedNumber ): Long    = if (x eq null) 0L    else x.longValue();
  def floatValue  (x: BoxedNumber ): Float   = if (x eq null) 0.0F  else x.floatValue();
  def doubleValue (x: BoxedNumber ): Double  = if (x eq null) 0.0D  else x.doubleValue();
  def arrayValue  (x: BoxedArray, elemTag: String): AnyRef =
    if (x eq null) null else x.unbox(elemTag);
  def arrayValue  (x: BoxedArray, elemClass: Class): AnyRef =
    if (x eq null) null else x.unbox(elemClass);

  def boxArray(value: AnyRef): BoxedArray = value match {
    case x: Array[Byte] => new BoxedByteArray(x)
    case x: Array[Short] => new BoxedShortArray(x)
    case x: Array[Char] => new BoxedCharArray(x)
    case x: Array[Int] => new BoxedIntArray(x)
    case x: Array[Long] => new BoxedLongArray(x)
    case x: Array[Float] => new BoxedFloatArray(x)
    case x: Array[Double] => new BoxedDoubleArray(x)
    case x: Array[Boolean] => new BoxedBooleanArray(x)
    case x: Array[AnyRef] => new BoxedObjectArray(x)
    case x: BoxedArray => x
  }
}

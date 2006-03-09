/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime;


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

  def isValueTag(tag: String) = tag.charAt(0) == '.'

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

  def _toString(x: CaseClass): String = {
    caseFields(x).mkString(x.caseName + "(", ",", ")")
  }

  def _hashCode(x: CaseClass): Int = {
    var code = x.getClass().hashCode();
    val arity = x.caseArity;
    var i = 0;
    while (i < arity) {
      code = code * 41 + x.caseElement(i).hashCode();
      i = i + 1
    }
    code
  }

  def _equals(x: CaseClass, y: Any): Boolean = y match {
    case y1: CaseClass =>
      (x.getClass() eq y1.getClass()) && {
	val arity = x.caseArity;
	var i = 0;
	while (i < arity && x.caseElement(i) == y1.caseElement(i))
	  i = i + 1;
	i == arity
      }
    case _ =>
      false
  }

  //def checkDefined[T >: AllRef](x: T): T =
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
  def arrayValue  (x: BoxedArray, elemTag: String): Object =
    if (x eq null) null else x.unbox(elemTag);

  def boxArray(value: Object): BoxedArray = value match {
    case x: Array[Byte] => new BoxedByteArray(x)
    case x: Array[Short] => new BoxedShortArray(x)
    case x: Array[Char] => new BoxedCharArray(x)
    case x: Array[Int] => new BoxedIntArray(x)
    case x: Array[Long] => new BoxedLongArray(x)
    case x: Array[Float] => new BoxedFloatArray(x)
    case x: Array[Double] => new BoxedDoubleArray(x)
    case x: Array[Boolean] => new BoxedBooleanArray(x)
    case x: Array[Object] => new BoxedObjectArray(x)
    case x: BoxedArray => x
  }
}

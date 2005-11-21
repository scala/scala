/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2005, LAMP/EPFL             **
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

  trait Try[a] {
    def Catch[b >: a](handler: PartialFunction[Throwable, b]): b;
    def Finally(handler: Unit): a;
  }

  def Try[a](block: => a): Try[a] = new Try[a] with Runnable {
    var result: a = _;
    var exception: Throwable = RunTime.tryCatch(this);

    def run(): Unit = result = block;

    def Catch[b >: a](handler: PartialFunction[Throwable, b]): b =
      if (exception == null)
	result.asInstanceOf$erased[b]
      // !!! else if (exception is LocalReturn)
      // !!!   // ...
      else if (handler isDefinedAt exception)
	handler(exception)
      else
	throw exception;

    def Finally(handler: Unit): a =
      if (exception == null)
        result.asInstanceOf$erased[a]
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

  def Seq[a](xs: a*): Seq[a] = null; // interpreted specially by new backend.
}

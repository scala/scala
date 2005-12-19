/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime.compat;

object Platform {
  def arraycopy(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: int): Unit =
    System.Array.Copy(src.asInstanceOf[System.Array], srcPos, dest.asInstanceOf[System.Array], destPos, length);
  def getClass(obj: AnyRef) = obj.GetType();
  def getClassName(obj: AnyRef) = obj.GetType().FullName;
  def printStackTrace(exc: System.Exception) =
    System.Console.WriteLine(exc.StackTrace);
  def getMessage(exc: System.Exception) = exc.Message;
  def split(str: String, separator: Char): Array[String] = {
    val sep = new Array[Char](1);
    sep(0) = separator;
    str.Split(sep);
  }

  def currentThread = System.Threading.Thread.CurrentThread;

  def parseByte(s: String)  : Byte  = System.Byte.Parse(s);
  def parseShort(s: String) : Short = System.Int16.Parse(s);
  def parseInt(s: String)   : Int   = System.Int32.Parse(s);
  def parseLong(s: String)  : Long  = System.Int64.Parse(s);
  def parseFloat(s: String) : Float = System.Single.Parse(s);
  def parseDouble(s: String): Double = System.Double.Parse(s);

  def isDigit(c: Char): Boolean = System.Char.IsDigit(c);
}

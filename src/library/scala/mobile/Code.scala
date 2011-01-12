/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.mobile


import java.lang.reflect.{Constructor, Method, Modifier}
import java.lang.NoSuchMethodException

/** The class <code>Code</code> provides <code>apply</code> methods
 *  with different arities (actually up to 9 parameters) to invoke
 *  a function simply by specifying its name and argument types.<p/>
 *
 *  Example:<pre>
 *    <b>val</b> url = <b>new</b> URL("http://scala-lang.org/classes/examples.jar");
 *    <b>val</b> obj = <b>new</b> Location(url) create "examples.sort";
 *    <b>val</b> ar = Array(6, 2, 8, 5, 1);
 *    obj[Array[Int], Unit]("println")(ar);
 *    obj[Array[Int], Unit]("sort")(ar);
 *    obj[Array[Int], Unit]("println")(ar);</pre>
 *
 *  @see <a href="Location.html">Location</a>
 *
 *  @author  Stephane Micheloud
 *  @version 1.0, 04/05/2004
 */
class Code(clazz: java.lang.Class[_]) {

  private type JObject = java.lang.Object

  private var instance: JObject = _

  ///////////////////////////// apply methods ///////////////////////////////

  type AnyClass = Class[T] forSome { type T }

  def apply[R](funName: String) =
    () => {
      val args  = Array[JObject]()
      val types = Array[AnyClass]()
      applyFun(funName, args, types).asInstanceOf[R]
    }

  def apply[A0, R](funName: String) =
    (_0: A0) => {
      val p     = boxValue(_0)
      val args  = Array(p._1)
      val types = Array[AnyClass](p._2)
      applyFun(funName, args, types).asInstanceOf[R]
    }

  def apply[A0, A1, R](funName: String) =
    (_0: A0, _1: A1) => {
      val p0    = boxValue(_0)
      val p1    = boxValue(_1)
      val args  = Array(p0._1, p1._1)
      val types = Array[AnyClass](p0._2, p1._2)
      applyFun(funName, args, types).asInstanceOf[R]
    }

  def apply[A0, A1, A2, R](funName: String) =
    (_0: A0, _1: A1, _2: A2) => {
      val p0    = boxValue(_0)
      val p1    = boxValue(_1)
      val p2    = boxValue(_2)
      val args  = Array(p0._1, p1._1, p2._1)
      val types = Array[AnyClass](p0._2, p1._2, p2._2)
      applyFun(funName, args, types).asInstanceOf[R]
    }

  def apply[A0, A1, A2, A3, R](funName: String) =
    (_0: A0, _1: A1, _2: A2, _3: A3) => {
      val p0    = boxValue(_0)
      val p1    = boxValue(_1)
      val p2    = boxValue(_2)
      val p3    = boxValue(_3)
      val args  = Array(p0._1, p1._1, p2._1, p3._1)
      val types = Array[AnyClass](p0._2, p1._2, p2._2, p3._2)
      applyFun(funName, args, types).asInstanceOf[R]
    }

  def apply[A0, A1, A2, A3, A4, R](funName: String) =
    (_0: A0, _1: A1, _2: A2, _3: A3, _4: A4) => {
      val p0    = boxValue(_0)
      val p1    = boxValue(_1)
      val p2    = boxValue(_2)
      val p3    = boxValue(_3)
      val p4    = boxValue(_4)
      val args  = Array(p0._1, p1._1, p2._1, p3._1, p4._1)
      val types = Array[AnyClass](p0._2, p1._2, p2._2, p3._2, p4._2)
      applyFun(funName, args, types).asInstanceOf[R]
    }

  def apply[A0, A1, A2, A3, A4, A5, R](funName: String) =
    (_0: A0, _1: A1, _2: A2, _3: A3, _4: A4, _5: A5) => {
      val p0    = boxValue(_0)
      val p1    = boxValue(_1)
      val p2    = boxValue(_2)
      val p3    = boxValue(_3)
      val p4    = boxValue(_4)
      val p5    = boxValue(_5)
      val args  = Array(p0._1, p1._1, p2._1, p3._1, p4._1, p5._1)
      val types = Array[AnyClass](p0._2, p1._2, p2._2, p3._2, p4._2, p5._2)
      applyFun(funName, args, types).asInstanceOf[R]
    }

  def apply[A0, A1, A2, A3, A4, A5, A6, R](funName: String) =
    (_0: A0, _1: A1, _2: A2, _3: A3, _4: A4, _5: A5, _6: A6) => {
      val p0    = boxValue(_0)
      val p1    = boxValue(_1)
      val p2    = boxValue(_2)
      val p3    = boxValue(_3)
      val p4    = boxValue(_4)
      val p5    = boxValue(_5)
      val p6    = boxValue(_6)
      val args  = Array(p0._1, p1._1, p2._1, p3._1, p4._1, p5._1, p6._1)
      val types = Array[AnyClass](p0._2, p1._2, p2._2, p3._2, p4._2, p5._2, p6._2)
      applyFun(funName, args, types).asInstanceOf[R]
    }

  def apply[A0, A1, A2, A3, A4, A5, A6, A7, R](funName: String) =
    (_0: A0, _1: A1, _2: A2, _3: A3, _4: A4, _5: A5, _6: A6, _7: A7) => {
      val p0    = boxValue(_0)
      val p1    = boxValue(_1)
      val p2    = boxValue(_2)
      val p3    = boxValue(_3)
      val p4    = boxValue(_4)
      val p5    = boxValue(_5)
      val p6    = boxValue(_6)
      val p7    = boxValue(_7)
      val args  = Array(p0._1, p1._1, p2._1, p3._1, p4._1, p5._1, p6._1, p7._1)
      val types = Array[AnyClass](p0._2, p1._2, p2._2, p3._2, p4._2, p5._2, p6._2, p7._2)
      applyFun(funName, args, types).asInstanceOf[R]
    }

  def apply[A0, A1, A2, A3, A4, A5, A6, A7, A8, R](funName: String) =
    (_0: A0, _1: A1, _2: A2, _3: A3, _4: A4, _5: A5, _6: A6, _7: A7, _8: A8) => {
      val p0    = boxValue(_0)
      val p1    = boxValue(_1)
      val p2    = boxValue(_2)
      val p3    = boxValue(_3)
      val p4    = boxValue(_4)
      val p5    = boxValue(_5)
      val p6    = boxValue(_6)
      val p7    = boxValue(_7)
      val p8    = boxValue(_8)
      val args  = Array(p0._1, p1._1, p2._1, p3._1, p4._1, p5._1, p6._1, p7._1, p8._1)
      val types = Array[AnyClass](p0._2, p1._2, p2._2, p3._2, p4._2, p5._2, p6._2, p7._2, p8._2)
      applyFun(funName, args, types).asInstanceOf[R]
    }

  ////////////////////// private functions ///////////////////////

  private def boxValue(value: Any) = value match {
    case x: Byte    => (java.lang.Byte.valueOf(x),        java.lang.Byte.TYPE)
    case x: Boolean => (java.lang.Boolean.valueOf(x),     java.lang.Boolean.TYPE)
    case x: Char    => (java.lang.Character.valueOf(x),   java.lang.Character.TYPE)
    case x: Short   => (java.lang.Short.valueOf(x),       java.lang.Short.TYPE)
    case x: Int     => (java.lang.Integer.valueOf(x),     java.lang.Integer.TYPE)
    case x: Long    => (java.lang.Long.valueOf(x),        java.lang.Long.TYPE)
    case x: Float   => (java.lang.Float.valueOf(x),       java.lang.Float.TYPE)
    case x: Double  => (java.lang.Double.valueOf(x),      java.lang.Double.TYPE)
    case _          =>
      val x = value.asInstanceOf[JObject]
      (x, x.getClass())
  }

  private def isConstructorName(methName: String) = {
    var className = clazz.getName()
    val classInx = className.lastIndexOf(".")
    val methInx = methName.lastIndexOf(".")
    if (classInx > 0 && methInx < 0)
      className = className.substring(classInx + 1, className.length())
    methName.equals(className)
  }

  private def applyFun(methName: String, args: Array[JObject],
                       argTypes: Array[Class[T] forSome { type T }]): JObject = {
    try {
      val method = clazz.getMethod(methName, argTypes : _*)
      var obj: JObject = null
      if (! Modifier.isStatic(method.getModifiers())) {
        if (instance eq null) {
          instance = try {
            clazz.newInstance().asInstanceOf[AnyRef]
          } catch { case _ =>
            val cs = clazz.getConstructors()
//Console.println("cs.length=" + cs.length);
            if (cs.length > 0) {
              cs(0).newInstance("").asInstanceOf[AnyRef]
            } else {
              sys.error("class " + clazz.getName() + " has no public constructor")
            }
          }
        }
        obj = instance
      }
      val result = method.invoke(obj, args : _*)
      if (result eq null) ().asInstanceOf[JObject] else result
    }
    catch {
      case me: NoSuchMethodException =>
        if (isConstructorName(methName)) {
          try {
            val cstr = clazz.getConstructor(argTypes : _*)
            instance = cstr.newInstance(args : _*).asInstanceOf[AnyRef]
            instance
          }
          catch {
            case e: Exception =>
              Console.println(e.getMessage())
              e.printStackTrace()
          }
        }
        else {
          Console.println(me.getMessage())
          me.printStackTrace()
        }
        null
      case e: Exception =>
        Console.println(e.getMessage())
        e.printStackTrace()
        null
      }
    }

}

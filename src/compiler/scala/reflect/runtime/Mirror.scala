package scala.reflect
package runtime

import java.lang.reflect.Array
import ReflectionUtils._
import scala.tools.nsc.util.ScalaClassLoader._

/** The mirror for standard runtime reflection from Java.
 */
class Mirror(var classLoader: ClassLoader) extends Universe with api.Mirror {

  definitions.init()
  import definitions._

  def symbolForName(name: String): Symbol = {
    val clazz = javaClass(name, classLoader)
    classToScala(clazz)
  }

  def companionInstance(clazz: Symbol): AnyRef = {
    val singleton = singletonInstance(classLoader, clazz.fullName)
    singleton
  }

  def symbolOfInstance(obj: Any): Symbol = classToScala(obj.getClass)
  def typeOfInstance(obj: Any): Type = typeToScala(obj.getClass)
  // to do add getClass/getType for instances of primitive types, probably like this:
  // def getClass[T <: AnyVal : ClassTag](x: T): Symbol = classTag[T].sym

  def getValueOfField(receiver: AnyRef, field: Symbol): Any = {
    fieldToJava(field).get(receiver)
  }
  def setValueOfField(receiver: AnyRef, field: Symbol, value: Any): Unit = {
    fieldToJava(field).set(receiver, value)
  }
  def invoke(receiver: AnyRef, meth: Symbol)(args: Any*): Any = {
    if (meth.owner == ArrayClass) {
      meth.name match {
        case nme.length => return Array.getLength(receiver)
        case nme.apply => return Array.get(receiver, args(0).asInstanceOf[Int])
        case nme.update => return Array.set(receiver, args(0).asInstanceOf[Int], args(1))
      }
    }

    val jmeth = methodToJava(meth)
    jmeth.invoke(receiver, args.asInstanceOf[Seq[AnyRef]]: _*)
  }

  private def validateIncomingClassLoader(wannabeCl: ClassLoader) = {
    val ourCls = loaderChain(classLoader)
    if (wannabeCl != null && !(ourCls contains wannabeCl))
      throw new Error("class doesn't belong to the classloader chain of the mirror")
  }

  def classToType(jclazz: java.lang.Class[_]): Type = {
    validateIncomingClassLoader(jclazz.getClassLoader)
    typeToScala(jclazz)
  }

  def classToSymbol(jclazz: java.lang.Class[_]): Symbol = {
    validateIncomingClassLoader(jclazz.getClassLoader)
    classToScala(jclazz)
  }

  def typeToClass(tpe: Type): java.lang.Class[_] =
    typeToJavaClass(tpe)

  def symbolToClass(sym: Symbol): java.lang.Class[_] =
    classToJava(sym)

  override def inReflexiveMirror = true
}

/** test code; should go to tests once things settle down a bit
 *

object Test extends Mirror with App {
  val sym = classToScala(classOf[scala.collection.Iterable[_]])
  println(sym)
  println("parents = "+sym.info.parents)
  println("decls = "+(sym.info.decls.toList map (_.defString)))
  val ms = sym.info.members.toList map (_.initialize)
  println("members = "+(ms map (_.defString) mkString ("\n  ")))
}
*/
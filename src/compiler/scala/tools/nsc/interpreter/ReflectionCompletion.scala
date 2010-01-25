/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import java.lang.reflect
import reflect.Modifier.{ isPrivate, isProtected, isPublic, isStatic }
import scala.util.NameTransformer
import scala.collection.mutable.HashMap
import ReflectionCompletion._

/** A completion aware object representing a single instance of some class.
 *  It completes to instance fields and methods, and delegates to another
 *  InstanceCompletion object if it can determine the result type of the element.
 */
class InstanceCompletion(clazz: Class[_]) extends CompletionAware {
  def methods = clazz.getMethods.toList filterNot (x => isStatic(x.getModifiers))
  def fields = clazz.getFields.toList filterNot (x => isStatic(x.getModifiers))
  val (zeroArg, otherArg) = methods partition (_.getParameterTypes.size == 0)

  lazy val completions = (methods ::: fields) map (_.getName)
  override def mapFunction(s: String) = NameTransformer decode s

  // TODO
  // def idExtras = List("isInstanceOf", "asInstanceOf", "toString")

  override def follow(id: String) = {
    val nextClazz = zeroArg find (m => m.getName == id) map (_.getReturnType)
    if (nextClazz.isDefined) nextClazz map (x => new InstanceCompletion(x))
    else fields find (_.getName == id) map (x => new InstanceCompletion(x.getType))
  }
}

/** The complementary class to InstanceCompletion.  It has logic to deal with
 *  java static members and scala companion object members.
 */
class StaticCompletion(jarEntryName: String) extends CompletionAware {
  def className = jarEntryName.replace('/', '.')
  def isScalaClazz(cl: Class[_]) = allInterfaces(cl) exists (_.getName == "scala.ScalaObject")
  def isJava = !isScalaClazz(clazz)

  lazy val clazz: Class[_] = {
    val cl = Class.forName(className)
    if (className.last != '$' && isScalaClazz(cl)) {
      try Class.forName(className + "$")
      catch { case _: Exception => cl }
    }
    else cl
  }

  def methodFilter: reflect.Method => Boolean =
    if (isJava) m => isStatic(m.getModifiers) && isPublic(m.getModifiers)
    else m => isPublic(m.getModifiers)

  def methods = clazz.getMethods.toList filter methodFilter
  def fields = clazz.getFields.toList

  lazy val completions = (methods ::: fields) map (_.getName)
  override def mapFunction(s: String) = NameTransformer decode s

  // TODO - old version.
  //
  // private def getClassObject(path: String): Option[Class[_]] = {
  //   val cl = clazz.getClassLoader()
  //   try Some(Class.forName(path, true, cl).asInstanceOf[Class[_]])
  //   catch { case _ => None }
  // }
  //
  // def completeStaticMembers(path: String): List[String] = {
  //   // java style, static methods
  //   val js = getClassObject(path) map (getMembers(_, true)) getOrElse Nil
  //   // scala style, methods on companion object
  //   // if getClassObject fails, see if there is a type alias
  //   val clazz = getClassObject(path + "$") orElse {
  //     (ByteCode aliasForType path) flatMap (x => getClassObject(x + "$"))
  //   }
  //   val ss = clazz map (getMembers(_, false)) getOrElse Nil
  //
  //   js ::: ss
  // }
}

// TODO
class PackageObjectCompletion(packageName: String) extends CompletionAware {
  def completions() = error("TODO")

  // def completePackageMembers(path: String): List[String] =
  //   getClassObject(path + "." + "package") map (getMembers(_, false)) getOrElse Nil
}

class ReflectionCompletion { }
object ReflectionCompletion {
  import java.io.File
  import java.util.jar.{ JarEntry, JarFile }
  import scala.tools.nsc.io.Streamable

  val EXPAND_SEPARATOR_STRING = "$$"
  val ANON_CLASS_NAME = "$anon"
  val TRAIT_SETTER_SEPARATOR_STRING = "$_setter_$"
  val IMPL_CLASS_SUFFIX ="$class"
  val INTERPRETER_VAR_PREFIX = "res"

  def allInterfaces(clazz: Class[_]): List[Class[_]] = allInterfaces(clazz, Nil)
  def allInterfaces(clazz: Class[_], acc: List[Class[_]]): List[Class[_]] = {
    if (clazz == null) acc.removeDuplicates
    else allInterfaces(clazz.getSuperclass, acc ::: clazz.getInterfaces.toList)
  }

  // methods to leave out of completion
  val excludeMethods = List("", "hashCode", "equals", "wait", "notify", "notifyAll")

  def shouldHide(x: String) =
    (excludeMethods contains x) ||
    (x contains EXPAND_SEPARATOR_STRING) ||   // XXX
    (x contains ANON_CLASS_NAME) ||
    (x contains TRAIT_SETTER_SEPARATOR_STRING) ||
    (x endsWith IMPL_CLASS_SUFFIX) ||
    (x == "MODULE$") ||
    (x matches """.*\$\d+$""")

  // XXX at the moment this is imperfect because scala's protected semantics
  // differ from java's, so protected methods appear public via reflection;
  // yet scala enforces the protection.  The result is that protected members
  // appear in completion yet cannot actually be called.  Fixing this
  // properly requires a scala.reflect.* API.  Fixing it uglily is possible
  // too (cast to structural type!) but I deem poor use of energy.
  private def skipModifiers(m: reflect.Method) = {
    import java.lang.reflect.Modifier._
    val flags = STATIC | PRIVATE | PROTECTED
    (m.getModifiers & flags) == 0
  }
  private def getAnyClass(x: Any): Class[_] = x.asInstanceOf[AnyRef].getClass

  def methodsOf(target: Any): List[String] =
    getAnyClass(target).getMethods filter skipModifiers map (_.getName) toList
}

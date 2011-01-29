/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import java.lang.reflect
import reflect.{ Modifier, AccessibleObject }
import Modifier.{ isPrivate, isProtected, isStatic }
import ReflectionCompletion._

trait ReflectionCompletion extends CompletionAware {
  def clazz: JClass
  protected def visibleMembers: List[AccessibleObject]
  protected def memberCompletions = visibleMembers filter isPublic map reflectName

  def reflectName(m: AccessibleObject) = m match {
    case x: reflect.Method  => x.getName
    case x: reflect.Field   => x.getName
    case x                  => sys.error(x.toString)
  }
  def isPublic(m: AccessibleObject) = m match {
    case x: reflect.Method  => Modifier isPublic x.getModifiers
    case x: reflect.Field   => Modifier isPublic x.getModifiers
    case x                  => sys.error(x.toString)
  }

  lazy val (staticMethods, instanceMethods) = clazz.getMethods.toList partition (x => isStatic(x.getModifiers))
  lazy val (staticFields, instanceFields) = clazz.getFields.toList partition (x => isStatic(x.getModifiers))

  /** Oops, mirror classes don't descend from scalaobject.
   */
  def isScalaClazz(cl: JClass) = {
    (allInterfacesFor(cl) exists (_.getName == "scala.ScalaObject")) ||
    (classForName(cl.getName + "$").isDefined)
  }
  def allInterfacesFor(cl: JClass): List[JClass] = allInterfacesFor(cl, Nil)

  private def allInterfacesFor(cl: JClass, acc: List[JClass]): List[JClass] = {
    if (cl == null) acc.distinct
    else allInterfacesFor(cl.getSuperclass, acc ::: cl.getInterfaces.toList)
  }
}

/** A completion aware object representing a single instance of some class.
 *  It completes to instance fields and methods, and delegates to another
 *  InstanceCompletion object if it can determine the result type of the element.
 */
class InstanceCompletion(val clazz: JClass) extends ReflectionCompletion {
  protected def visibleMembers = instanceMethods ::: instanceFields
  def extras = List("isInstanceOf", "asInstanceOf", "toString")
  lazy val completions = memberCompletions ::: extras
  def completions(verbosity: Int) = completions

  val (zeroArg, otherArg) = instanceMethods partition (_.getParameterTypes.size == 0)
  override def follow(id: String) = {
    val nextClazz = zeroArg find (m => m.getName == id) map (_.getReturnType)
    if (nextClazz.isDefined) nextClazz map (x => new InstanceCompletion(x))
    else instanceFields find (_.getName == id) map (x => new InstanceCompletion(x.getType))
  }
}

/** The complementary class to InstanceCompletion.  It has logic to deal with
 *  java static members and scala companion object members.
 */
class StaticCompletion(val clazz: JClass) extends ReflectionCompletion {
  protected def visibleMembers = whichMethods ::: whichFields
  lazy val completions = memberCompletions
  def completions(verbosity: Int) = completions

  def className = clazz.getName
  def isJava = !isScalaClazz(clazz)

  private def whichMethods = if (isJava) staticMethods else instanceMethods
  private def whichFields = if (isJava) staticFields else instanceFields
  val (zeroArg, otherArg) = whichMethods partition (_.getParameterTypes.size == 0)

  override def follow(id: String) = {
    val nextClazz = zeroArg find (m => m.getName == id) map (_.getReturnType)
    if (nextClazz.isDefined) nextClazz map (x => new InstanceCompletion(x))
    else staticFields find (_.getName == id) map (x => new InstanceCompletion(x.getType))
  }

  override def toString = "StaticCompletion(%s) => %s".format(clazz.getName, completions)
}

object ReflectionCompletion {
  import java.io.File
  import java.util.jar.{ JarEntry, JarFile }
  import scala.tools.nsc.io.Streamable

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
  private def getAnyClass(x: Any): JClass = x.asInstanceOf[AnyRef].getClass

  def methodsOf(target: Any): List[String] =
    getAnyClass(target).getMethods filter skipModifiers map (_.getName) toList
}

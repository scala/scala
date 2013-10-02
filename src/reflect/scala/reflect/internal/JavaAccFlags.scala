/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */
package scala
package reflect
package internal

import java.lang.{ Class => jClass }
import java.lang.reflect.{ Member => jMember, Constructor => jConstructor, Field => jField, Method => jMethod }
import JavaAccFlags._
import ClassfileConstants._

/** A value class which encodes the access_flags (JVMS 4.1)
 *  for a field, method, or class. The low 16 bits are the same
 *  as those returned by java.lang.reflect.Member#getModifiers
 *  and found in the bytecode.
 *
 *  The high bits encode whether the access flags are directly
 *  associated with a class, constructor, field, or method.
 */
final class JavaAccFlags private (val coded: Int) extends AnyVal {
  private def has(mask: Int) = (flags & mask) != 0
  private def flagCarrierId  = coded >>> 16
  private def flags          = coded & 0xFFFF

  def isAbstract     = has(JAVA_ACC_ABSTRACT)
  def isAnnotation   = has(JAVA_ACC_ANNOTATION)
  def isBridge       = has(JAVA_ACC_BRIDGE)
  def isEnum         = has(JAVA_ACC_ENUM)
  def isFinal        = has(JAVA_ACC_FINAL)
  def isInterface    = has(JAVA_ACC_INTERFACE)
  def isNative       = has(JAVA_ACC_NATIVE)
  def isPrivate      = has(JAVA_ACC_PRIVATE)
  def isProtected    = has(JAVA_ACC_PROTECTED)
  def isPublic       = has(JAVA_ACC_PUBLIC)
  def isStatic       = has(JAVA_ACC_STATIC)
  def isStrictFp     = has(JAVA_ACC_STRICT)
  def isSuper        = has(JAVA_ACC_SUPER)
  def isSynchronized = has(JAVA_ACC_SYNCHRONIZED)
  def isSynthetic    = has(JAVA_ACC_SYNTHETIC)
  def isTransient    = has(JAVA_ACC_TRANSIENT)
  def isVarargs      = has(JAVA_ACC_VARARGS)
  def isVolatile     = has(JAVA_ACC_VOLATILE)

  /** Do these flags describe a member which has either protected or package access?
   *  Such access in java is encoded in scala as protected[foo] or private[foo], where
   *  `foo` is the defining package.
   */
  def hasPackageAccessBoundary = !has(JAVA_ACC_PRIVATE | JAVA_ACC_PUBLIC) // equivalently, allows protected or package level access
  def isPackageProtected       = !has(JAVA_ACC_PRIVATE | JAVA_ACC_PROTECTED | JAVA_ACC_PUBLIC)

  def toJavaFlags: Int = flags
  def toScalaFlags: Long = flagCarrierId match {
    case Method | Constructor => FlagTranslation methodFlags flags
    case Class                => FlagTranslation classFlags flags
    case _                    => FlagTranslation fieldFlags flags
  }
}

object JavaAccFlags {
  private val Unknown     = 0
  private val Class       = 1
  private val Field       = 2
  private val Method      = 3
  private val Constructor = 4

  private def create(flagCarrier: Int, access_flags: Int): JavaAccFlags =
    new JavaAccFlags((flagCarrier << 16) | (access_flags & 0xFFFF))

  def classFlags(flags: Int): JavaAccFlags       = create(Class, flags)
  def methodFlags(flags: Int): JavaAccFlags      = create(Method, flags)
  def fieldFlags(flags: Int): JavaAccFlags       = create(Field, flags)
  def constructorFlags(flags: Int): JavaAccFlags = create(Constructor, flags)

  def apply(access_flags: Int): JavaAccFlags = create(Unknown, access_flags)
  def apply(clazz: jClass[_]): JavaAccFlags  = classFlags(clazz.getModifiers)
  def apply(member: jMember): JavaAccFlags   = member match {
    case x: jConstructor[_] => constructorFlags(x.getModifiers)
    case x: jMethod         => methodFlags(x.getModifiers)
    case x: jField          => fieldFlags(x.getModifiers)
    case _                  => apply(member.getModifiers)
  }
}

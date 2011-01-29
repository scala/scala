/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import java.lang.{ reflect => r }
import r.TypeVariable
import scala.reflect.NameTransformer

/** Logic for turning a type into a String.  The goal is to be
 *  able to take some arbitrary object 'x' and obtain the most precise
 *  String for which an injection of x.asInstanceOf[String] will
 *  be valid from both the JVM's and scala's perspectives.
 *
 *  "definition" is when you want strings like
 */
trait TypeStrings {
  private val ObjectClass = classOf[java.lang.Object]
  private val primitives = Set[String]("byte", "char", "short", "int", "long", "float", "double", "boolean")
  private def unbox(s: String): String = s.stripPrefix("java.lang.") match {
    case "Integer"    => "scala.Int"
    case "Character"  => "scala.Char"
    case "Void"       => "scala.Unit"
    case x @ ("Byte" | "Short" | "Long" | "Float" | "Double" | "Boolean") => "scala." + x
    case _            => NameTransformer.decode(s)
  }

  def scalaName(s: String): String = {
    if (s endsWith "$") (s dropRight 1) + ".type"
    else if (primitives(s)) "scala." + s.capitalize
    else if (s == "void") "scala.Unit"
    else unbox(s)
  }
  def scalaName(clazz: JClass): String       = scalaName(clazz.getName)
  def scalaName(m: ClassManifest[_]): String = scalaName(m.erasure)
  def anyClass(x: Any): JClass               = if (x == null) null else x.asInstanceOf[AnyRef].getClass

  private def tvarString(tvar: TypeVariable[_]): String = tvarString(tvar.getBounds.toList)
  private def tvarString(bounds: List[AnyRef]): String = {
    val xs = bounds filterNot (_ == ObjectClass) collect { case x: Class[_] => x }
    if (xs.isEmpty) "_"
    else scalaName(xs.head)
  }
  private def tparamString(clazz: JClass): String = {
    val tps = clazz.getTypeParameters.toList
    if (tps.isEmpty)
      return ""

    (tps map tvarString).mkString("[", ", ", "]")
  }
  private def tparamString[T: Manifest] : String = {
    val tps = manifest[T].typeArguments
    if (tps.isEmpty)
      return ""

    tps.map(m => tvarString(List(m.erasure))).mkString("[", ", ", "]")
  }
  /** Going for an overabundance of caution right now.
   */
  def fromTypedValue[T: Manifest](x: T): String = fromManifest[T]
  def fromValue(value: Any): String             = if (value == null) "Null" else fromClazz(anyClass(value))
  def fromClazz(clazz: JClass): String          = scalaName(clazz) + tparamString(clazz)
  def fromManifest[T: Manifest] : String        = scalaName(manifest[T].erasure) + tparamString[T]

  /** Reducing fully qualified noise for some common packages.
   */
  val typeTransforms = List(
    "java.lang." -> "",
    "scala.collection.immutable." -> "immutable.",
    "scala.collection.mutable." -> "mutable.",
    "scala.collection.generic." -> "generic."
  )
}

object TypeStrings extends TypeStrings { }
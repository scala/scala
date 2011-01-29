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
  private val primitives = Set[String]("byte", "char", "short", "int", "long", "float", "double", "boolean", "void")
  private val primitiveMap = primitives.toList map { x =>
    val key = x match {
      case "void" => "Void"
      case "int"  => "Integer"
      case "char" => "Character"
      case s      => s.capitalize
    }
    val value = x match {
      case "void" => "Unit"
      case s      => s.capitalize
    }

    ("java.lang." + key) -> ("scala." + value)
  } toMap

  def scalaName(s: String): String = {
    if (s endsWith "$") s.init + ".type"
    else if (s == "void") "scala.Unit"
    else if (primitives(s)) "scala." + s.capitalize
    else primitiveMap.getOrElse(s, NameTransformer decode s)
  }
  def scalaName(clazz: JClass): String       = scalaName(clazz.getName)
  def scalaName(m: ClassManifest[_]): String = scalaName(m.erasure)
  def anyClass(x: Any): JClass               = if (x == null) null else x.asInstanceOf[AnyRef].getClass

  private def brackets(tps: String*): String =
    if (tps.isEmpty) ""
    else tps.mkString("[", ", ", "]")

  private def tvarString(tvar: TypeVariable[_]): String = tvarString(tvar.getBounds.toList)
  private def tvarString(bounds: List[AnyRef]): String = {
    val xs = bounds filterNot (_ == ObjectClass) collect { case x: Class[_] => x }
    if (xs.isEmpty) "_"
    else scalaName(xs.head)
  }
  private def tparamString(clazz: JClass): String =
    brackets(clazz.getTypeParameters map tvarString: _*)

  private def tparamString[T: Manifest] : String =
    brackets(manifest[T].typeArguments map (m => tvarString(List(m.erasure))): _*)

  /** Going for an overabundance of caution right now.  Later these types
   *  can be a lot more precise, but right now the manifests have a habit of
   *  introducing material which is not syntactically valid as scala source.
   *  When this happens it breaks the repl.  It would be nice if we mandated
   *  that manifest toString methods (or some other method, since it's bad
   *  practice to rely on toString for correctness) generated the VALID string
   *  representation of the type.
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
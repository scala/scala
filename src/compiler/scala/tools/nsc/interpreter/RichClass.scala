/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

class RichClass[T](val clazz: Class[T]) {
  def toManifest: Manifest[T] = Manifest.classType(clazz)
  def toTypeString: String = TypeStrings.fromClazz(clazz)

  // Sadly isAnonymousClass does not return true for scala anonymous
  // classes because our naming scheme is not doing well against the
  // jvm's many assumptions.
  def isScalaAnonymous = clazz.isAnonymousClass || (clazz.getName contains "$anon$")

  /** It's not easy... to be... me... */
  def supermans: List[Manifest[_]] = supers map (_.toManifest)
  def superNames: List[String]     = supers map (_.getName)
  def interfaces: List[JClass]     = supers filter (_.isInterface)

  def hasAncestorName(f: String => Boolean) = superNames exists f
  def hasAncestor(f: JClass => Boolean) = supers exists f
  def hasAncestorInPackage(pkg: String) = hasAncestorName(_ startsWith (pkg + "."))

  def supers: List[JClass] = {
    def loop(x: JClass): List[JClass] = x.getSuperclass match {
      case null   => List(x)
      case sc     => x :: (x.getInterfaces.toList flatMap loop) ++ loop(sc)
    }
    loop(clazz).distinct
  }
}

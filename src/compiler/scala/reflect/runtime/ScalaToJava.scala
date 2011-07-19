package scala.reflect
package runtime

import java.lang.{Class => jClass, Package => jPackage}
import java.lang.reflect.{
  Method => jMethod, Constructor => jConstructor, Modifier => jModifier, Field => jField,
  Member => jMember, Type => jType, GenericDeclaration}

trait ScalaToJava extends ConversionUtil { self: Universe =>

  /** Optionally, the Java package corresponding to a given Scala package, or None if no such Java package exists.
   *  @param   pkg The Scala package
   */
  def packageToJava(pkg: Symbol): Option[jPackage] = packageCache.toJavaOption(pkg) {
    Option(jPackage.getPackage(pkg.fullName.toString))
  }

  /** The Java class corresponding to given Scala class.
   *  Note: This only works for
   *   - top-level classes
   *   - Scala classes that were generated via jclassToScala
   *   - classes that have a class owner that has a corresponding Java class
   *  @throws A `NoClassDefFoundError` for all Scala classes not in one of these categories.
   */
  def classToJava(clazz: Symbol): jClass[_] = classCache.toJava(clazz) {
    def noClass = throw new NoClassDefFoundError("no Java class corresponding to "+clazz+" found")
    if (clazz.owner.isPackageClass)
      jClass.forName(clazz.fullName)
    else if (clazz.owner.isClass)
      classToJava(clazz.owner)
        .getDeclaredClasses
        .find(_.getSimpleName == clazz.name.toString)
        .getOrElse(noClass)
    else
      noClass
  }

  def fieldToJava(fld: Symbol): jField = null // to be done
  def methodToJava(meth: Symbol): jMethod = null // to be done
  def constrToJava(constr: Symbol): jConstructor[_] = null // to be done

    /** The Java class corresponds to given Scala type (to be done)
   */
  def typeToJavaClass(tpe: Type): jClass[_] = null

}
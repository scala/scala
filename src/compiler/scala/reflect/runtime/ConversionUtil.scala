package scala.reflect
package runtime

import java.lang.{Class => jClass, Package => jPackage}
import java.lang.reflect.{
  Method => jMethod, Constructor => jConstructor, Modifier => jModifier, Field => jField,
  Member => jMember, Type => jType, GenericDeclaration}
import collection.mutable.HashMap

trait ConversionUtil extends internal.transform.Transforms { self: Universe =>

  /** A cache that maintains a bijection between Java reflection type `J`
   *  and Scala reflection type `S`.
   */
  protected class TwoWayCache[J, S] {
    private val toScalaMap = new HashMap[J, S]
    private val toJavaMap = new HashMap[S, J]

    def toScala(key: J)(body: => S): S = toScalaMap.getOrElseUpdate(key, body)

    def toJava(key: S)(body: => J): J = toJavaMap.getOrElseUpdate(key, body)

    def toJavaOption(key: S)(body: => Option[J]): Option[J] = toJavaMap get key match {
      case None =>
        val result = body
        for (value <- result) toJavaMap(key) = value
        result
      case some => some
    }
  }

  protected val classCache = new TwoWayCache[jClass[_], Symbol]
  protected val packageCache = new TwoWayCache[Package, Symbol]
  protected val methodCache = new TwoWayCache[jMethod, Symbol]
  protected val constructorCache = new TwoWayCache[jConstructor[_], Symbol]
  protected val fieldCache = new TwoWayCache[jField, Symbol]

  def typeToJavaClass(tpe: Type): jClass[_]

  /** Does method `meth` erase to Java method `jmeth`?
   *  This is true if the Java method type is the same as the Scala method type after performing
   *  all Scala-specific transformations in InfoTransformers. (to be done)
   */
  protected def erasesTo(meth: Symbol, jmeth: jMethod): Boolean = {
    val mtpe = javaType(meth)
    (mtpe.paramTypes map typeToJavaClass) == jmeth.getParameterTypes.toList &&
    typeToJavaClass(mtpe.resultType) == jmeth.getReturnType
  }

  /** Does constructor `meth` erase to Java method `jconstr`?
   *  This is true if the Java constructor type is the same as the Scala constructor type after performing
   *  all Scala-specific transformations in InfoTransformers. (to be done)
   */
  protected def erasesTo(meth: Symbol, jconstr: jConstructor[_]): Boolean = {
    val mtpe = javaType(meth)
    (mtpe.paramTypes map typeToJavaClass) == jconstr.getParameterTypes.toList
  }
}
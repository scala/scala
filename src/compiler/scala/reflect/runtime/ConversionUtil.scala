package scala.reflect
package runtime

import java.lang.{Class => jClass, Package => jPackage}
import java.lang.reflect.{
  Method => jMethod, Constructor => jConstructor, Modifier => jModifier, Field => jField,
  Member => jMember, Type => jType, TypeVariable => jTypeVariable, GenericDeclaration}
import collection.mutable.HashMap

trait ConversionUtil { self: SymbolTable =>

  /** A cache that maintains a bijection between Java reflection type `J`
   *  and Scala reflection type `S`.
   */
  protected class TwoWayCache[J, S] {

    private val toScalaMap = new HashMap[J, S]
    private val toJavaMap = new HashMap[S, J]

    def enter(j: J, s: S) = synchronized {
      debugInfo("cached: "+j+"/"+s)
      toScalaMap(j) = s
      toJavaMap(s) = j
    }

    def toScala(key: J)(body: => S): S = synchronized { 
      toScalaMap get key match {
        case Some(v) =>
          v
        case none =>
          val result = body
          enter(key, result)
          result
      }
    }

    def toJava(key: S)(body: => J): J = synchronized { 
      toJavaMap get key match {
        case Some(v) =>
          v
        case none =>
          val result = body
          enter(result, key)
          result
      }
    }

    def toJavaOption(key: S)(body: => Option[J]): Option[J] = synchronized {
      toJavaMap get key match {
        case None =>
          val result = body
          for (value <- result) enter(value, key)
          result
        case some => some
      }
    }
  }

  protected val classCache = new TwoWayCache[jClass[_], Symbol]
  protected val packageCache = new TwoWayCache[Package, Symbol]
  protected val methodCache = new TwoWayCache[jMethod, Symbol]
  protected val constructorCache = new TwoWayCache[jConstructor[_], Symbol]
  protected val fieldCache = new TwoWayCache[jField, Symbol]
  protected val tparamCache = new TwoWayCache[jTypeVariable[_], Symbol]

  /** the type of this symbol after Scala -> Java transformsi in refChecks, uncurry, erasure
   */
  def transformedType(sym: Symbol): Type

  /** The Java class thaty given type compiles to */
  def typeToJavaClass(tpe: Type): jClass[_]

  /** Does method `meth` erase to Java method `jmeth`?
   *  This is true if the Java method type is the same as the Scala method type after performing
   *  all Scala-specific transformations in InfoTransformers. (to be done)
   */
  protected def erasesTo(meth: Symbol, jmeth: jMethod): Boolean = {
    val mtpe = transformedType(meth)
    (mtpe.paramTypes map typeToJavaClass) == jmeth.getParameterTypes.toList &&
    typeToJavaClass(mtpe.resultType) == jmeth.getReturnType
  }

  /** Does constructor `meth` erase to Java method `jconstr`?
   *  This is true if the Java constructor type is the same as the Scala constructor type after performing
   *  all Scala-specific transformations in InfoTransformers. (to be done)
   */
  protected def erasesTo(meth: Symbol, jconstr: jConstructor[_]): Boolean = {
    val mtpe = transformedType(meth)
    (mtpe.paramTypes map typeToJavaClass) == jconstr.getParameterTypes.toList
  }
}
/* NSC -- new Scala compiler -- Copyright 2007-2010 LAMP/EPFL */

package scala.tools.nsc
package doc

import scala.collection._

/**
  * A class that provides comments for all symbols which pre-exist in Scala (Any, Nothing, ...)
  * It also contains a HashSet of the given symbols
  * The comments are to be added to a HashMap called comments, which resides in the Global.scala file
  * @author Manohar Jonnalagedda, Stephane Micheloud, Sean McDirmid, Geoffrey Washburn
  * @version 1.0 */
abstract class SourcelessComments {

  val global: Global

  import global._
  import definitions._

  lazy val comments = {

    val comment = mutable.HashMap.empty[Symbol, DocComment]

    comment(NothingClass) = new DocComment("""
      /** Class `Nothing` is - together with class [[scala.Null]] - at the bottom of Scala's type hierarchy.
        *
        * Type `Nothing` is a subtype of every other type (including [[scala.Null]]); there exist ''no instances'' of
        * this type. Even though type `Nothing` is empty, it is nevertheless useful as a type parameter. For instance,
        * the Scala library defines a value [[scala.collection.immutable.Nil]] of type `List[Nothing]`. Because lists
        * are covariant in Scala, this makes [[scala.collection.immutable.Nil]] an instance of `List[T]`, for any
        * element of type `T`. */
      """)

     comment(NullClass) = new DocComment("""
       /** Class `Null` is - together with class [[scala.Nothing]] - at the bottom of the Scala type hierarchy.
         *
         * Type `Null` is a subtype of all reference types; its only instance is the `null` reference. Since `Null` is
         * not a subtype of value types, `null` is not a member of any such type. For instance, it is not possible to
         * assign `null` to a variable of type [[scala.Int]]. */
       """)

     /*******************************************************************/
     /* Documentation for Any */

     comment(AnyClass) = new DocComment("""
      /** Class `Any` is the root of the Scala class hierarchy. Every class in a Scala execution environment inherits
        * directly or indirectly from this class. Class `Any` has two direct subclasses: [[scala.AnyRef]] and
        * [[scala.AnyVal]]. */
      """)

    comment(Any_equals) = new DocComment("""
      /** This method is used to compare the receiver object (`this`) with the argument object (`arg0`) for equivalence.
        *
        * The default implementations of this method is an [http://en.wikipedia.org/wiki/Equivalence_relation equivalence
        * relation]:
        *  * It is reflexive: for any instance `x` of type `Any`, `x.equals(x)` should return `true`.
        *  * It is symmetric: for any instances `x` and `y` of type `Any`, `x.equals(y)` should return `true` if and
        *    only if `y.equals(x)` returns `true`.
        *  * It is transitive: for any instances `x`, `y`, and `z` of type `AnyRef` if `x.equals(y)` returns `true` and
        *    `y.equals(z)` returns `true`, then `x.equals(z)` should return `true`.
        *
        * If you override this method, you should verify that your implementation remains an equivalence relation.
        * Additionally, when overriding this method it is often necessary to override `hashCode` to ensure that objects
        * that are "equal" (`o1.equals(o2)` returns `true`) hash to the same [[scala.Int]]
        * (`o1.hashCode.equals(o2.hashCode)`).
        *
        * @param arg0 the object to compare against this object for equality.
        * @return     `true` if the receiver object is equivalent to the argument; `false` otherwise. */
      """)

    comment(Any_==) = new DocComment("""
      /** `o == arg0` is the same as `o.equals(arg0)`.
        *
        * @param arg0 the object to compare against this object for equality.
        * @return     `true` if the receiver object is equivalent to the argument; `false` otherwise. */
      """)

    comment(Any_!=) = new DocComment("""
      /** `o != arg0` is the same as `!(o == (arg0))`.
        *
        * @param arg0 the object to compare against this object for dis-equality.
        * @return     `false` if the receiver object is equivalent to the argument; `true` otherwise. */
      """)

    comment(Any_toString) = new DocComment("""
      /** Returns a string representation of the object.
        *
        * The default representation is platform dependent.
        *
        * @return a string representation of the object. */
      """)

    comment(Any_asInstanceOf) = new DocComment("""
      /** This method is used to cast the receiver object to be of type `T0`.
        *
        * Note that the success of a cast at runtime is modulo Scala's erasure semantics.  Therefore the expression
        * `1.asInstanceOf[String]` will throw a `ClassCastException` at runtime, while the expression
        * `List(1).asInstanceOf[List[String]]` will not.  In the latter example, because the type argument is erased as
        * part of compilation it is not possible to check whether the contents of the list are of the requested typed.
        *
        * @throws ClassCastException if the receiver object is not an instance of erasure of type `T0`.
        * @return the receiver object. */
      """)

    comment(Any_isInstanceOf) = new DocComment("""
      /** This method is used to test whether the dynamic type of the receiver object is `T0`.
        *
        * Note that the test result of the test is modulo Scala's erasure semantics.  Therefore the expression
        * `1.isInstanceOf[String]` will return `false`, while the expression `List(1).isInstanceOf[List[String]]` will
        * return `true`.  In the latter example, because the type argument is erased as part of compilation it is not
        * possible to check whether the contents of the list are of the requested typed.
        *
        * @return `true` if the receiver object is an instance of erasure of type `T0`; `false` otherwise. */
      """)

    comment(Any_hashCode) = new DocComment("""
      /** Returns a hash code value for the object.
        *
        * The default hashing algorithm is platform dependent.
        *
        * Note that it is allowed for two objects to have identical hash codes (`o1.hashCode.equals(o2.hashCode)`) yet
        * not be equal (`o1.equals(o2)` returns `false`).  A degenerate implementation could always return `0`.
        * However, it is required that if two objects are equal (`o1.equals(o2)` returns `true`) that they have
        * identical hash codes (`o1.hashCode.equals(o2.hashCode)`).  Therefore, when overriding this method, be sure
        * to verify that the behavior is consistent with the `equals` method.
        *
        * @return the hash code value for the object. */
      """)

     /*******************************************************************/
     /* Documentation for AnyRef */

     comment(AnyRefClass) = new DocComment("""
       /** Class `AnyRef` is the root class of all ''reference types''. */
       """)

    comment(Object_==) = new DocComment("""
      /** `o == arg0` is the same as `if (o eq null) arg0 eq null else o.equals(arg0)`.
        *
        * @param arg0 the object to compare against this object for equality.
        * @return `true` if the receiver object is equivalent to the argument; `false` otherwise. */
      """)

    comment(Object_ne) = new DocComment("""
      /** `o.ne(arg0)` is the same as `!(o.eq(arg0))`.
        *
        * @param arg0 the object to compare against this object for reference dis-equality.
        * @return `false` if the argument is not a reference to the receiver object; `true` otherwise. */
      """)


    comment(Object_finalize) = new DocComment("""
      /** This method is called by the garbage collector on the receiver object when garbage collection determines that
        * there are no more references to the object.
        *
        * The details of when and if the `finalize` method are invoked, as well as the interaction between `finalize`
        * and non-local returns and exceptions, are all platform dependent. */
      """)

    comment(Object_clone) = new DocComment("""
      /** This method creates and returns a copy of the receiver object.
        *
        * The default implementation of the `clone` method is platform dependent.
        *
        * @return a copy of the receiver object. */
      """)

    comment(Object_getClass) = new DocComment("""
      /** Returns a representation that corresponds to the dynamic class of the receiver object.
        *
        * The nature of the representation is platform dependent.
        *
        * @return a representation that corresponds to the dynamic class of the receiver object. */
      """)

    comment(Object_notify) = new DocComment("""
      /** Wakes up a single thread that is waiting on the receiver object's monitor. */
      """)

    comment(Object_notifyAll) = new DocComment("""
      /** Wakes up all threads that are waiting on the receiver object's monitor. */
      """)

    comment(Object_eq) = new DocComment("""
      /** This method is used to test whether the argument (`arg0`) is a reference to the
        * receiver object (`this`).
        *
        * The `eq` method implements an [http://en.wikipedia.org/wiki/Equivalence_relation equivalence relation] on
        * non-null instances of `AnyRef`:
        *  * It is reflexive: for any non-null instance `x` of type `AnyRef`, `x.eq(x)` returns `true`.
        *  * It is symmetric: for any non-null instances `x` and `y` of type `AnyRef`, `x.eq(y)` returns `true` if and
        *    only if `y.eq(x)` returns `true`.
        *  * It is transitive: for any non-null instances `x`, `y`, and `z` of type `AnyRef` if `x.eq(y)` returns `true`
        *    and `y.eq(z)` returns `true`, then `x.eq(z)` returns `true`.
        *
        * Additionally, the `eq` method has three other properties.
        *  * It is consistent: for any non-null instances `x` and `y` of type `AnyRef`, multiple invocations of
        *    `x.eq(y)` consistently returns `true` or consistently returns `false`.
        *  * For any non-null instance `x` of type `AnyRef`, `x.eq(null)` and `null.eq(x)` returns `false`.
        *  * `null.eq(null)` returns `true`.
        *
        * When overriding the `equals` or `hashCode` methods, it is important to ensure that their behavior is
        * consistent with reference equality.  Therefore, if two objects are references to each other (`o1 eq o2`), they
        * should be equal to each other (`o1 == o2`) and they should hash to the same value (`o1.hashCode == o2.hashCode`).
        *
        * @param arg0 the object to compare against this object for reference equality.
        * @return `true` if the argument is a reference to the receiver object; `false` otherwise. */
      """)

    /*******************************************************************/

    comment(AnyValClass) = new DocComment("""
      /** Class `AnyVal` is the root class of all ''value types''.
        *
        * `AnyVal` has a fixed number of subclasses, which describe values which are not implemented as objects in the
        * underlying host sys.
        *
        * Classes [[scala.Double]], [[scala.Float]], [[scala.Long]], [[scala.Int]], [[scala.Char]], [[scala.Short]],
        * and [[scala.Byte]] are together called ''numeric value types''. Classes [[scala.Byte]], [[scala.Short]], and
        * [[scala.Char]] are called ''subrange types''. Subrange types, as well as [[scala.Int]] and [[scala.Long]] are
        * called ''integer types'', whereas [[scala.Float]] and [[scala.Double]] are called ''floating point types''. */
      """)

    comment(BooleanClass) = new DocComment("""
      /** Class `Boolean` has only two values: `true` and `false`. */
      """)

    comment(UnitClass) = new DocComment("""
      /** Class `Unit` has only one value: `()`. */
      """)

    List(ByteClass, CharClass, DoubleClass, LongClass, FloatClass, IntClass, ShortClass) foreach { sym =>
      val maxValue = "MAX_" + sym.name.toString().toUpperCase()
      val minValue = "MIN_" + sym.name.toString().toUpperCase()
      comment(sym) = new DocComment("""
        /** Class `""" + sym.name + """` belongs to the value classes whose instances are not represented as objects by
          * the underlying host sys.  There is an implicit conversion from instances of `""" + sym.name + """` to
          * instances of [[scala.runtime.Rich""" + sym.name + """]] which provides useful non-primitive operations.
          * All value classes inherit from class [[scala.AnyVal]].
          *
          * Values `""" + maxValue + """` and `""" + minValue + """` are defined in object [[scala.Math]]. */
        """)
    }

    comment
  }

}

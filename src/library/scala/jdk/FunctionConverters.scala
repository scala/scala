/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

// GENERATED CODE: DO NOT EDIT.


package scala.jdk

/** This object contains methods that convert between Scala and Java function types.
  *
  * The explicit conversion methods defined here are practical when writing Java code. For Scala
  * code, it is recommended to use the extension methods defined in [[FunctionConverters.Ops]].
  *
  * Using the `.asJava` extension method (from [[FunctionConverters.Ops]]) on a Scala function
  * produces the most specific possible Java function type:
  *
  * {{{
  *   scala> import scala.jdk.FunctionConverters; import FunctionConverters.Ops._
  *   scala> val f = (x: Int) => x + 1
  *
  *   scala> val jf1 = f.asJava
  *   jf1: java.util.function.IntUnaryOperator = ...
  * }}}
  *
  * More generic Java function types can be created using the corresponding `asJavaXYZ` extension
  * method:
  *
  * {{{
  *   scala> val jf2 = f.asJavaFunction
  *   jf2: java.util.function.Function[Int,Int] = ...
  *
  *   scala> val jf3 = f.asJavaUnaryOperator
  *   jf3: java.util.function.UnaryOperator[Int] = ...
  * }}}
  *
  * Converting a Java function to Scala is done using the `asScala` extension method:
  *
  * {{{
  *   scala> List(1,2,3).map(jf2.asScala)
  *   res1: List[Int] = List(2, 3, 4)
  * }}}
  */
object FunctionConverters {
  /** This object provides extension methods that convert between Scala and Java function types,
    * see [[FunctionConverters]].
    */
  object Ops extends Priority0FunctionExtensions

  import FunctionWrappers._

  @inline def asScalaFromBiConsumer[T, U](jf: java.util.function.BiConsumer[T, U]): scala.Function2[T, U, Unit] = new FromJavaBiConsumer[T, U](jf)
  
  @inline def asJavaBiConsumer[T, U](sf: scala.Function2[T, U, Unit]): java.util.function.BiConsumer[T, U] = new AsJavaBiConsumer[T, U](sf)
  
  
  @inline def asScalaFromBiFunction[T, U, R](jf: java.util.function.BiFunction[T, U, R]): scala.Function2[T, U, R] = new FromJavaBiFunction[T, U, R](jf)
  
  @inline def asJavaBiFunction[T, U, R](sf: scala.Function2[T, U, R]): java.util.function.BiFunction[T, U, R] = new AsJavaBiFunction[T, U, R](sf)
  
  
  @inline def asScalaFromBiPredicate[T, U](jf: java.util.function.BiPredicate[T, U]): scala.Function2[T, U, Boolean] = new FromJavaBiPredicate[T, U](jf)
  
  @inline def asJavaBiPredicate[T, U](sf: scala.Function2[T, U, Boolean]): java.util.function.BiPredicate[T, U] = new AsJavaBiPredicate[T, U](sf)
  
  
  @inline def asScalaFromBinaryOperator[T](jf: java.util.function.BinaryOperator[T]): scala.Function2[T, T, T] = new FromJavaBinaryOperator[T](jf)
  
  @inline def asJavaBinaryOperator[T](sf: scala.Function2[T, T, T]): java.util.function.BinaryOperator[T] = new AsJavaBinaryOperator[T](sf)
  
  
  @inline def asScalaFromBooleanSupplier(jf: java.util.function.BooleanSupplier): scala.Function0[Boolean] = new FromJavaBooleanSupplier(jf)
  
  @inline def asJavaBooleanSupplier(sf: scala.Function0[Boolean]): java.util.function.BooleanSupplier = new AsJavaBooleanSupplier(sf)
  
  
  @inline def asScalaFromConsumer[T](jf: java.util.function.Consumer[T]): scala.Function1[T, Unit] = new FromJavaConsumer[T](jf)
  
  @inline def asJavaConsumer[T](sf: scala.Function1[T, Unit]): java.util.function.Consumer[T] = new AsJavaConsumer[T](sf)
  
  
  @inline def asScalaFromDoubleBinaryOperator(jf: java.util.function.DoubleBinaryOperator): scala.Function2[Double, Double, Double] = new FromJavaDoubleBinaryOperator(jf)
  
  @inline def asJavaDoubleBinaryOperator(sf: scala.Function2[Double, Double, Double]): java.util.function.DoubleBinaryOperator = new AsJavaDoubleBinaryOperator(sf)
  
  
  @inline def asScalaFromDoubleConsumer(jf: java.util.function.DoubleConsumer): scala.Function1[Double, Unit] = new FromJavaDoubleConsumer(jf)
  
  @inline def asJavaDoubleConsumer(sf: scala.Function1[Double, Unit]): java.util.function.DoubleConsumer = new AsJavaDoubleConsumer(sf)
  
  
  @inline def asScalaFromDoubleFunction[R](jf: java.util.function.DoubleFunction[R]): scala.Function1[Double, R] = new FromJavaDoubleFunction[R](jf)
  
  @inline def asJavaDoubleFunction[R](sf: scala.Function1[Double, R]): java.util.function.DoubleFunction[R] = new AsJavaDoubleFunction[R](sf)
  
  
  @inline def asScalaFromDoublePredicate(jf: java.util.function.DoublePredicate): scala.Function1[Double, Boolean] = new FromJavaDoublePredicate(jf)
  
  @inline def asJavaDoublePredicate(sf: scala.Function1[Double, Boolean]): java.util.function.DoublePredicate = new AsJavaDoublePredicate(sf)
  
  
  @inline def asScalaFromDoubleSupplier(jf: java.util.function.DoubleSupplier): scala.Function0[Double] = new FromJavaDoubleSupplier(jf)
  
  @inline def asJavaDoubleSupplier(sf: scala.Function0[Double]): java.util.function.DoubleSupplier = new AsJavaDoubleSupplier(sf)
  
  
  @inline def asScalaFromDoubleToIntFunction(jf: java.util.function.DoubleToIntFunction): scala.Function1[Double, Int] = new FromJavaDoubleToIntFunction(jf)
  
  @inline def asJavaDoubleToIntFunction(sf: scala.Function1[Double, Int]): java.util.function.DoubleToIntFunction = new AsJavaDoubleToIntFunction(sf)
  
  
  @inline def asScalaFromDoubleToLongFunction(jf: java.util.function.DoubleToLongFunction): scala.Function1[Double, Long] = new FromJavaDoubleToLongFunction(jf)
  
  @inline def asJavaDoubleToLongFunction(sf: scala.Function1[Double, Long]): java.util.function.DoubleToLongFunction = new AsJavaDoubleToLongFunction(sf)
  
  
  @inline def asScalaFromDoubleUnaryOperator(jf: java.util.function.DoubleUnaryOperator): scala.Function1[Double, Double] = new FromJavaDoubleUnaryOperator(jf)
  
  @inline def asJavaDoubleUnaryOperator(sf: scala.Function1[Double, Double]): java.util.function.DoubleUnaryOperator = new AsJavaDoubleUnaryOperator(sf)
  
  
  @inline def asScalaFromFunction[T, R](jf: java.util.function.Function[T, R]): scala.Function1[T, R] = new FromJavaFunction[T, R](jf)
  
  @inline def asJavaFunction[T, R](sf: scala.Function1[T, R]): java.util.function.Function[T, R] = new AsJavaFunction[T, R](sf)
  
  
  @inline def asScalaFromIntBinaryOperator(jf: java.util.function.IntBinaryOperator): scala.Function2[Int, Int, Int] = new FromJavaIntBinaryOperator(jf)
  
  @inline def asJavaIntBinaryOperator(sf: scala.Function2[Int, Int, Int]): java.util.function.IntBinaryOperator = new AsJavaIntBinaryOperator(sf)
  
  
  @inline def asScalaFromIntConsumer(jf: java.util.function.IntConsumer): scala.Function1[Int, Unit] = new FromJavaIntConsumer(jf)
  
  @inline def asJavaIntConsumer(sf: scala.Function1[Int, Unit]): java.util.function.IntConsumer = new AsJavaIntConsumer(sf)
  
  
  @inline def asScalaFromIntFunction[R](jf: java.util.function.IntFunction[R]): scala.Function1[Int, R] = new FromJavaIntFunction[R](jf)
  
  @inline def asJavaIntFunction[R](sf: scala.Function1[Int, R]): java.util.function.IntFunction[R] = new AsJavaIntFunction[R](sf)
  
  
  @inline def asScalaFromIntPredicate(jf: java.util.function.IntPredicate): scala.Function1[Int, Boolean] = new FromJavaIntPredicate(jf)
  
  @inline def asJavaIntPredicate(sf: scala.Function1[Int, Boolean]): java.util.function.IntPredicate = new AsJavaIntPredicate(sf)
  
  
  @inline def asScalaFromIntSupplier(jf: java.util.function.IntSupplier): scala.Function0[Int] = new FromJavaIntSupplier(jf)
  
  @inline def asJavaIntSupplier(sf: scala.Function0[Int]): java.util.function.IntSupplier = new AsJavaIntSupplier(sf)
  
  
  @inline def asScalaFromIntToDoubleFunction(jf: java.util.function.IntToDoubleFunction): scala.Function1[Int, Double] = new FromJavaIntToDoubleFunction(jf)
  
  @inline def asJavaIntToDoubleFunction(sf: scala.Function1[Int, Double]): java.util.function.IntToDoubleFunction = new AsJavaIntToDoubleFunction(sf)
  
  
  @inline def asScalaFromIntToLongFunction(jf: java.util.function.IntToLongFunction): scala.Function1[Int, Long] = new FromJavaIntToLongFunction(jf)
  
  @inline def asJavaIntToLongFunction(sf: scala.Function1[Int, Long]): java.util.function.IntToLongFunction = new AsJavaIntToLongFunction(sf)
  
  
  @inline def asScalaFromIntUnaryOperator(jf: java.util.function.IntUnaryOperator): scala.Function1[Int, Int] = new FromJavaIntUnaryOperator(jf)
  
  @inline def asJavaIntUnaryOperator(sf: scala.Function1[Int, Int]): java.util.function.IntUnaryOperator = new AsJavaIntUnaryOperator(sf)
  
  
  @inline def asScalaFromLongBinaryOperator(jf: java.util.function.LongBinaryOperator): scala.Function2[Long, Long, Long] = new FromJavaLongBinaryOperator(jf)
  
  @inline def asJavaLongBinaryOperator(sf: scala.Function2[Long, Long, Long]): java.util.function.LongBinaryOperator = new AsJavaLongBinaryOperator(sf)
  
  
  @inline def asScalaFromLongConsumer(jf: java.util.function.LongConsumer): scala.Function1[Long, Unit] = new FromJavaLongConsumer(jf)
  
  @inline def asJavaLongConsumer(sf: scala.Function1[Long, Unit]): java.util.function.LongConsumer = new AsJavaLongConsumer(sf)
  
  
  @inline def asScalaFromLongFunction[R](jf: java.util.function.LongFunction[R]): scala.Function1[Long, R] = new FromJavaLongFunction[R](jf)
  
  @inline def asJavaLongFunction[R](sf: scala.Function1[Long, R]): java.util.function.LongFunction[R] = new AsJavaLongFunction[R](sf)
  
  
  @inline def asScalaFromLongPredicate(jf: java.util.function.LongPredicate): scala.Function1[Long, Boolean] = new FromJavaLongPredicate(jf)
  
  @inline def asJavaLongPredicate(sf: scala.Function1[Long, Boolean]): java.util.function.LongPredicate = new AsJavaLongPredicate(sf)
  
  
  @inline def asScalaFromLongSupplier(jf: java.util.function.LongSupplier): scala.Function0[Long] = new FromJavaLongSupplier(jf)
  
  @inline def asJavaLongSupplier(sf: scala.Function0[Long]): java.util.function.LongSupplier = new AsJavaLongSupplier(sf)
  
  
  @inline def asScalaFromLongToDoubleFunction(jf: java.util.function.LongToDoubleFunction): scala.Function1[Long, Double] = new FromJavaLongToDoubleFunction(jf)
  
  @inline def asJavaLongToDoubleFunction(sf: scala.Function1[Long, Double]): java.util.function.LongToDoubleFunction = new AsJavaLongToDoubleFunction(sf)
  
  
  @inline def asScalaFromLongToIntFunction(jf: java.util.function.LongToIntFunction): scala.Function1[Long, Int] = new FromJavaLongToIntFunction(jf)
  
  @inline def asJavaLongToIntFunction(sf: scala.Function1[Long, Int]): java.util.function.LongToIntFunction = new AsJavaLongToIntFunction(sf)
  
  
  @inline def asScalaFromLongUnaryOperator(jf: java.util.function.LongUnaryOperator): scala.Function1[Long, Long] = new FromJavaLongUnaryOperator(jf)
  
  @inline def asJavaLongUnaryOperator(sf: scala.Function1[Long, Long]): java.util.function.LongUnaryOperator = new AsJavaLongUnaryOperator(sf)
  
  
  @inline def asScalaFromObjDoubleConsumer[T](jf: java.util.function.ObjDoubleConsumer[T]): scala.Function2[T, Double, Unit] = new FromJavaObjDoubleConsumer[T](jf)
  
  @inline def asJavaObjDoubleConsumer[T](sf: scala.Function2[T, Double, Unit]): java.util.function.ObjDoubleConsumer[T] = new AsJavaObjDoubleConsumer[T](sf)
  
  
  @inline def asScalaFromObjIntConsumer[T](jf: java.util.function.ObjIntConsumer[T]): scala.Function2[T, Int, Unit] = new FromJavaObjIntConsumer[T](jf)
  
  @inline def asJavaObjIntConsumer[T](sf: scala.Function2[T, Int, Unit]): java.util.function.ObjIntConsumer[T] = new AsJavaObjIntConsumer[T](sf)
  
  
  @inline def asScalaFromObjLongConsumer[T](jf: java.util.function.ObjLongConsumer[T]): scala.Function2[T, Long, Unit] = new FromJavaObjLongConsumer[T](jf)
  
  @inline def asJavaObjLongConsumer[T](sf: scala.Function2[T, Long, Unit]): java.util.function.ObjLongConsumer[T] = new AsJavaObjLongConsumer[T](sf)
  
  
  @inline def asScalaFromPredicate[T](jf: java.util.function.Predicate[T]): scala.Function1[T, Boolean] = new FromJavaPredicate[T](jf)
  
  @inline def asJavaPredicate[T](sf: scala.Function1[T, Boolean]): java.util.function.Predicate[T] = new AsJavaPredicate[T](sf)
  
  
  @inline def asScalaFromSupplier[T](jf: java.util.function.Supplier[T]): scala.Function0[T] = new FromJavaSupplier[T](jf)
  
  @inline def asJavaSupplier[T](sf: scala.Function0[T]): java.util.function.Supplier[T] = new AsJavaSupplier[T](sf)
  
  
  @inline def asScalaFromToDoubleBiFunction[T, U](jf: java.util.function.ToDoubleBiFunction[T, U]): scala.Function2[T, U, Double] = new FromJavaToDoubleBiFunction[T, U](jf)
  
  @inline def asJavaToDoubleBiFunction[T, U](sf: scala.Function2[T, U, Double]): java.util.function.ToDoubleBiFunction[T, U] = new AsJavaToDoubleBiFunction[T, U](sf)
  
  
  @inline def asScalaFromToDoubleFunction[T](jf: java.util.function.ToDoubleFunction[T]): scala.Function1[T, Double] = new FromJavaToDoubleFunction[T](jf)
  
  @inline def asJavaToDoubleFunction[T](sf: scala.Function1[T, Double]): java.util.function.ToDoubleFunction[T] = new AsJavaToDoubleFunction[T](sf)
  
  
  @inline def asScalaFromToIntBiFunction[T, U](jf: java.util.function.ToIntBiFunction[T, U]): scala.Function2[T, U, Int] = new FromJavaToIntBiFunction[T, U](jf)
  
  @inline def asJavaToIntBiFunction[T, U](sf: scala.Function2[T, U, Int]): java.util.function.ToIntBiFunction[T, U] = new AsJavaToIntBiFunction[T, U](sf)
  
  
  @inline def asScalaFromToIntFunction[T](jf: java.util.function.ToIntFunction[T]): scala.Function1[T, Int] = new FromJavaToIntFunction[T](jf)
  
  @inline def asJavaToIntFunction[T](sf: scala.Function1[T, Int]): java.util.function.ToIntFunction[T] = new AsJavaToIntFunction[T](sf)
  
  
  @inline def asScalaFromToLongBiFunction[T, U](jf: java.util.function.ToLongBiFunction[T, U]): scala.Function2[T, U, Long] = new FromJavaToLongBiFunction[T, U](jf)
  
  @inline def asJavaToLongBiFunction[T, U](sf: scala.Function2[T, U, Long]): java.util.function.ToLongBiFunction[T, U] = new AsJavaToLongBiFunction[T, U](sf)
  
  
  @inline def asScalaFromToLongFunction[T](jf: java.util.function.ToLongFunction[T]): scala.Function1[T, Long] = new FromJavaToLongFunction[T](jf)
  
  @inline def asJavaToLongFunction[T](sf: scala.Function1[T, Long]): java.util.function.ToLongFunction[T] = new AsJavaToLongFunction[T](sf)
  
  
  @inline def asScalaFromUnaryOperator[T](jf: java.util.function.UnaryOperator[T]): scala.Function1[T, T] = new FromJavaUnaryOperator[T](jf)
  
  @inline def asJavaUnaryOperator[T](sf: scala.Function1[T, T]): java.util.function.UnaryOperator[T] = new AsJavaUnaryOperator[T](sf)
}
      

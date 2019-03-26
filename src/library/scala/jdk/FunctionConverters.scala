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

  @inline def asScalaFromBiConsumer[T, U](jf: java.util.function.BiConsumer[T, U]): scala.Function2[T, U, Unit] = jf match {
    case AsJavaBiConsumer((f @ _)) => f.asInstanceOf[scala.Function2[T, U, Unit]]
    case _ => new FromJavaBiConsumer[T, U](jf)
  }
  
  @inline def asJavaBiConsumer[T, U](sf: scala.Function2[T, U, Unit]): java.util.function.BiConsumer[T, U] = sf match {
    case FromJavaBiConsumer((f @ _)) => f.asInstanceOf[java.util.function.BiConsumer[T, U]]
    case _ => new AsJavaBiConsumer[T, U](sf)
  }
  
  
  @inline def asScalaFromBiFunction[T, U, R](jf: java.util.function.BiFunction[T, U, R]): scala.Function2[T, U, R] = jf match {
    case AsJavaBiFunction((f @ _)) => f.asInstanceOf[scala.Function2[T, U, R]]
    case _ => new FromJavaBiFunction[T, U, R](jf)
  }
  
  @inline def asJavaBiFunction[T, U, R](sf: scala.Function2[T, U, R]): java.util.function.BiFunction[T, U, R] = sf match {
    case FromJavaBiFunction((f @ _)) => f.asInstanceOf[java.util.function.BiFunction[T, U, R]]
    case _ => new AsJavaBiFunction[T, U, R](sf)
  }
  
  
  @inline def asScalaFromBiPredicate[T, U](jf: java.util.function.BiPredicate[T, U]): scala.Function2[T, U, Boolean] = jf match {
    case AsJavaBiPredicate((f @ _)) => f.asInstanceOf[scala.Function2[T, U, Boolean]]
    case _ => new FromJavaBiPredicate[T, U](jf)
  }
  
  @inline def asJavaBiPredicate[T, U](sf: scala.Function2[T, U, Boolean]): java.util.function.BiPredicate[T, U] = sf match {
    case FromJavaBiPredicate((f @ _)) => f.asInstanceOf[java.util.function.BiPredicate[T, U]]
    case _ => new AsJavaBiPredicate[T, U](sf)
  }
  
  
  @inline def asScalaFromBinaryOperator[T](jf: java.util.function.BinaryOperator[T]): scala.Function2[T, T, T] = jf match {
    case AsJavaBinaryOperator((f @ _)) => f.asInstanceOf[scala.Function2[T, T, T]]
    case _ => new FromJavaBinaryOperator[T](jf)
  }
  
  @inline def asJavaBinaryOperator[T](sf: scala.Function2[T, T, T]): java.util.function.BinaryOperator[T] = sf match {
    case FromJavaBinaryOperator((f @ _)) => f.asInstanceOf[java.util.function.BinaryOperator[T]]
    case _ => new AsJavaBinaryOperator[T](sf)
  }
  
  
  @inline def asScalaFromBooleanSupplier(jf: java.util.function.BooleanSupplier): scala.Function0[Boolean] = jf match {
    case AsJavaBooleanSupplier((f @ _)) => f.asInstanceOf[scala.Function0[Boolean]]
    case _ => new FromJavaBooleanSupplier(jf)
  }
  
  @inline def asJavaBooleanSupplier(sf: scala.Function0[Boolean]): java.util.function.BooleanSupplier = sf match {
    case FromJavaBooleanSupplier((f @ _)) => f.asInstanceOf[java.util.function.BooleanSupplier]
    case _ => new AsJavaBooleanSupplier(sf)
  }
  
  
  @inline def asScalaFromConsumer[T](jf: java.util.function.Consumer[T]): scala.Function1[T, Unit] = jf match {
    case AsJavaConsumer((f @ _)) => f.asInstanceOf[scala.Function1[T, Unit]]
    case _ => new FromJavaConsumer[T](jf)
  }
  
  @inline def asJavaConsumer[T](sf: scala.Function1[T, Unit]): java.util.function.Consumer[T] = sf match {
    case FromJavaConsumer((f @ _)) => f.asInstanceOf[java.util.function.Consumer[T]]
    case _ => new AsJavaConsumer[T](sf)
  }
  
  
  @inline def asScalaFromDoubleBinaryOperator(jf: java.util.function.DoubleBinaryOperator): scala.Function2[Double, Double, Double] = jf match {
    case AsJavaDoubleBinaryOperator((f @ _)) => f.asInstanceOf[scala.Function2[Double, Double, Double]]
    case _ => new FromJavaDoubleBinaryOperator(jf)
  }
  
  @inline def asJavaDoubleBinaryOperator(sf: scala.Function2[Double, Double, Double]): java.util.function.DoubleBinaryOperator = sf match {
    case FromJavaDoubleBinaryOperator((f @ _)) => f.asInstanceOf[java.util.function.DoubleBinaryOperator]
    case _ => new AsJavaDoubleBinaryOperator(sf)
  }
  
  
  @inline def asScalaFromDoubleConsumer(jf: java.util.function.DoubleConsumer): scala.Function1[Double, Unit] = jf match {
    case AsJavaDoubleConsumer((f @ _)) => f.asInstanceOf[scala.Function1[Double, Unit]]
    case _ => new FromJavaDoubleConsumer(jf)
  }
  
  @inline def asJavaDoubleConsumer(sf: scala.Function1[Double, Unit]): java.util.function.DoubleConsumer = sf match {
    case FromJavaDoubleConsumer((f @ _)) => f.asInstanceOf[java.util.function.DoubleConsumer]
    case _ => new AsJavaDoubleConsumer(sf)
  }
  
  
  @inline def asScalaFromDoubleFunction[R](jf: java.util.function.DoubleFunction[R]): scala.Function1[Double, R] = jf match {
    case AsJavaDoubleFunction((f @ _)) => f.asInstanceOf[scala.Function1[Double, R]]
    case _ => new FromJavaDoubleFunction[R](jf)
  }
  
  @inline def asJavaDoubleFunction[R](sf: scala.Function1[Double, R]): java.util.function.DoubleFunction[R] = sf match {
    case FromJavaDoubleFunction((f @ _)) => f.asInstanceOf[java.util.function.DoubleFunction[R]]
    case _ => new AsJavaDoubleFunction[R](sf)
  }
  
  
  @inline def asScalaFromDoublePredicate(jf: java.util.function.DoublePredicate): scala.Function1[Double, Boolean] = jf match {
    case AsJavaDoublePredicate((f @ _)) => f.asInstanceOf[scala.Function1[Double, Boolean]]
    case _ => new FromJavaDoublePredicate(jf)
  }
  
  @inline def asJavaDoublePredicate(sf: scala.Function1[Double, Boolean]): java.util.function.DoublePredicate = sf match {
    case FromJavaDoublePredicate((f @ _)) => f.asInstanceOf[java.util.function.DoublePredicate]
    case _ => new AsJavaDoublePredicate(sf)
  }
  
  
  @inline def asScalaFromDoubleSupplier(jf: java.util.function.DoubleSupplier): scala.Function0[Double] = jf match {
    case AsJavaDoubleSupplier((f @ _)) => f.asInstanceOf[scala.Function0[Double]]
    case _ => new FromJavaDoubleSupplier(jf)
  }
  
  @inline def asJavaDoubleSupplier(sf: scala.Function0[Double]): java.util.function.DoubleSupplier = sf match {
    case FromJavaDoubleSupplier((f @ _)) => f.asInstanceOf[java.util.function.DoubleSupplier]
    case _ => new AsJavaDoubleSupplier(sf)
  }
  
  
  @inline def asScalaFromDoubleToIntFunction(jf: java.util.function.DoubleToIntFunction): scala.Function1[Double, Int] = jf match {
    case AsJavaDoubleToIntFunction((f @ _)) => f.asInstanceOf[scala.Function1[Double, Int]]
    case _ => new FromJavaDoubleToIntFunction(jf)
  }
  
  @inline def asJavaDoubleToIntFunction(sf: scala.Function1[Double, Int]): java.util.function.DoubleToIntFunction = sf match {
    case FromJavaDoubleToIntFunction((f @ _)) => f.asInstanceOf[java.util.function.DoubleToIntFunction]
    case _ => new AsJavaDoubleToIntFunction(sf)
  }
  
  
  @inline def asScalaFromDoubleToLongFunction(jf: java.util.function.DoubleToLongFunction): scala.Function1[Double, Long] = jf match {
    case AsJavaDoubleToLongFunction((f @ _)) => f.asInstanceOf[scala.Function1[Double, Long]]
    case _ => new FromJavaDoubleToLongFunction(jf)
  }
  
  @inline def asJavaDoubleToLongFunction(sf: scala.Function1[Double, Long]): java.util.function.DoubleToLongFunction = sf match {
    case FromJavaDoubleToLongFunction((f @ _)) => f.asInstanceOf[java.util.function.DoubleToLongFunction]
    case _ => new AsJavaDoubleToLongFunction(sf)
  }
  
  
  @inline def asScalaFromDoubleUnaryOperator(jf: java.util.function.DoubleUnaryOperator): scala.Function1[Double, Double] = jf match {
    case AsJavaDoubleUnaryOperator((f @ _)) => f.asInstanceOf[scala.Function1[Double, Double]]
    case _ => new FromJavaDoubleUnaryOperator(jf)
  }
  
  @inline def asJavaDoubleUnaryOperator(sf: scala.Function1[Double, Double]): java.util.function.DoubleUnaryOperator = sf match {
    case FromJavaDoubleUnaryOperator((f @ _)) => f.asInstanceOf[java.util.function.DoubleUnaryOperator]
    case _ => new AsJavaDoubleUnaryOperator(sf)
  }
  
  
  @inline def asScalaFromFunction[T, R](jf: java.util.function.Function[T, R]): scala.Function1[T, R] = jf match {
    case AsJavaFunction((f @ _)) => f.asInstanceOf[scala.Function1[T, R]]
    case _ => new FromJavaFunction[T, R](jf)
  }
  
  @inline def asJavaFunction[T, R](sf: scala.Function1[T, R]): java.util.function.Function[T, R] = sf match {
    case FromJavaFunction((f @ _)) => f.asInstanceOf[java.util.function.Function[T, R]]
    case _ => new AsJavaFunction[T, R](sf)
  }
  
  
  @inline def asScalaFromIntBinaryOperator(jf: java.util.function.IntBinaryOperator): scala.Function2[Int, Int, Int] = jf match {
    case AsJavaIntBinaryOperator((f @ _)) => f.asInstanceOf[scala.Function2[Int, Int, Int]]
    case _ => new FromJavaIntBinaryOperator(jf)
  }
  
  @inline def asJavaIntBinaryOperator(sf: scala.Function2[Int, Int, Int]): java.util.function.IntBinaryOperator = sf match {
    case FromJavaIntBinaryOperator((f @ _)) => f.asInstanceOf[java.util.function.IntBinaryOperator]
    case _ => new AsJavaIntBinaryOperator(sf)
  }
  
  
  @inline def asScalaFromIntConsumer(jf: java.util.function.IntConsumer): scala.Function1[Int, Unit] = jf match {
    case AsJavaIntConsumer((f @ _)) => f.asInstanceOf[scala.Function1[Int, Unit]]
    case _ => new FromJavaIntConsumer(jf)
  }
  
  @inline def asJavaIntConsumer(sf: scala.Function1[Int, Unit]): java.util.function.IntConsumer = sf match {
    case FromJavaIntConsumer((f @ _)) => f.asInstanceOf[java.util.function.IntConsumer]
    case _ => new AsJavaIntConsumer(sf)
  }
  
  
  @inline def asScalaFromIntFunction[R](jf: java.util.function.IntFunction[R]): scala.Function1[Int, R] = jf match {
    case AsJavaIntFunction((f @ _)) => f.asInstanceOf[scala.Function1[Int, R]]
    case _ => new FromJavaIntFunction[R](jf)
  }
  
  @inline def asJavaIntFunction[R](sf: scala.Function1[Int, R]): java.util.function.IntFunction[R] = sf match {
    case FromJavaIntFunction((f @ _)) => f.asInstanceOf[java.util.function.IntFunction[R]]
    case _ => new AsJavaIntFunction[R](sf)
  }
  
  
  @inline def asScalaFromIntPredicate(jf: java.util.function.IntPredicate): scala.Function1[Int, Boolean] = jf match {
    case AsJavaIntPredicate((f @ _)) => f.asInstanceOf[scala.Function1[Int, Boolean]]
    case _ => new FromJavaIntPredicate(jf)
  }
  
  @inline def asJavaIntPredicate(sf: scala.Function1[Int, Boolean]): java.util.function.IntPredicate = sf match {
    case FromJavaIntPredicate((f @ _)) => f.asInstanceOf[java.util.function.IntPredicate]
    case _ => new AsJavaIntPredicate(sf)
  }
  
  
  @inline def asScalaFromIntSupplier(jf: java.util.function.IntSupplier): scala.Function0[Int] = jf match {
    case AsJavaIntSupplier((f @ _)) => f.asInstanceOf[scala.Function0[Int]]
    case _ => new FromJavaIntSupplier(jf)
  }
  
  @inline def asJavaIntSupplier(sf: scala.Function0[Int]): java.util.function.IntSupplier = sf match {
    case FromJavaIntSupplier((f @ _)) => f.asInstanceOf[java.util.function.IntSupplier]
    case _ => new AsJavaIntSupplier(sf)
  }
  
  
  @inline def asScalaFromIntToDoubleFunction(jf: java.util.function.IntToDoubleFunction): scala.Function1[Int, Double] = jf match {
    case AsJavaIntToDoubleFunction((f @ _)) => f.asInstanceOf[scala.Function1[Int, Double]]
    case _ => new FromJavaIntToDoubleFunction(jf)
  }
  
  @inline def asJavaIntToDoubleFunction(sf: scala.Function1[Int, Double]): java.util.function.IntToDoubleFunction = sf match {
    case FromJavaIntToDoubleFunction((f @ _)) => f.asInstanceOf[java.util.function.IntToDoubleFunction]
    case _ => new AsJavaIntToDoubleFunction(sf)
  }
  
  
  @inline def asScalaFromIntToLongFunction(jf: java.util.function.IntToLongFunction): scala.Function1[Int, Long] = jf match {
    case AsJavaIntToLongFunction((f @ _)) => f.asInstanceOf[scala.Function1[Int, Long]]
    case _ => new FromJavaIntToLongFunction(jf)
  }
  
  @inline def asJavaIntToLongFunction(sf: scala.Function1[Int, Long]): java.util.function.IntToLongFunction = sf match {
    case FromJavaIntToLongFunction((f @ _)) => f.asInstanceOf[java.util.function.IntToLongFunction]
    case _ => new AsJavaIntToLongFunction(sf)
  }
  
  
  @inline def asScalaFromIntUnaryOperator(jf: java.util.function.IntUnaryOperator): scala.Function1[Int, Int] = jf match {
    case AsJavaIntUnaryOperator((f @ _)) => f.asInstanceOf[scala.Function1[Int, Int]]
    case _ => new FromJavaIntUnaryOperator(jf)
  }
  
  @inline def asJavaIntUnaryOperator(sf: scala.Function1[Int, Int]): java.util.function.IntUnaryOperator = sf match {
    case FromJavaIntUnaryOperator((f @ _)) => f.asInstanceOf[java.util.function.IntUnaryOperator]
    case _ => new AsJavaIntUnaryOperator(sf)
  }
  
  
  @inline def asScalaFromLongBinaryOperator(jf: java.util.function.LongBinaryOperator): scala.Function2[Long, Long, Long] = jf match {
    case AsJavaLongBinaryOperator((f @ _)) => f.asInstanceOf[scala.Function2[Long, Long, Long]]
    case _ => new FromJavaLongBinaryOperator(jf)
  }
  
  @inline def asJavaLongBinaryOperator(sf: scala.Function2[Long, Long, Long]): java.util.function.LongBinaryOperator = sf match {
    case FromJavaLongBinaryOperator((f @ _)) => f.asInstanceOf[java.util.function.LongBinaryOperator]
    case _ => new AsJavaLongBinaryOperator(sf)
  }
  
  
  @inline def asScalaFromLongConsumer(jf: java.util.function.LongConsumer): scala.Function1[Long, Unit] = jf match {
    case AsJavaLongConsumer((f @ _)) => f.asInstanceOf[scala.Function1[Long, Unit]]
    case _ => new FromJavaLongConsumer(jf)
  }
  
  @inline def asJavaLongConsumer(sf: scala.Function1[Long, Unit]): java.util.function.LongConsumer = sf match {
    case FromJavaLongConsumer((f @ _)) => f.asInstanceOf[java.util.function.LongConsumer]
    case _ => new AsJavaLongConsumer(sf)
  }
  
  
  @inline def asScalaFromLongFunction[R](jf: java.util.function.LongFunction[R]): scala.Function1[Long, R] = jf match {
    case AsJavaLongFunction((f @ _)) => f.asInstanceOf[scala.Function1[Long, R]]
    case _ => new FromJavaLongFunction[R](jf)
  }
  
  @inline def asJavaLongFunction[R](sf: scala.Function1[Long, R]): java.util.function.LongFunction[R] = sf match {
    case FromJavaLongFunction((f @ _)) => f.asInstanceOf[java.util.function.LongFunction[R]]
    case _ => new AsJavaLongFunction[R](sf)
  }
  
  
  @inline def asScalaFromLongPredicate(jf: java.util.function.LongPredicate): scala.Function1[Long, Boolean] = jf match {
    case AsJavaLongPredicate((f @ _)) => f.asInstanceOf[scala.Function1[Long, Boolean]]
    case _ => new FromJavaLongPredicate(jf)
  }
  
  @inline def asJavaLongPredicate(sf: scala.Function1[Long, Boolean]): java.util.function.LongPredicate = sf match {
    case FromJavaLongPredicate((f @ _)) => f.asInstanceOf[java.util.function.LongPredicate]
    case _ => new AsJavaLongPredicate(sf)
  }
  
  
  @inline def asScalaFromLongSupplier(jf: java.util.function.LongSupplier): scala.Function0[Long] = jf match {
    case AsJavaLongSupplier((f @ _)) => f.asInstanceOf[scala.Function0[Long]]
    case _ => new FromJavaLongSupplier(jf)
  }
  
  @inline def asJavaLongSupplier(sf: scala.Function0[Long]): java.util.function.LongSupplier = sf match {
    case FromJavaLongSupplier((f @ _)) => f.asInstanceOf[java.util.function.LongSupplier]
    case _ => new AsJavaLongSupplier(sf)
  }
  
  
  @inline def asScalaFromLongToDoubleFunction(jf: java.util.function.LongToDoubleFunction): scala.Function1[Long, Double] = jf match {
    case AsJavaLongToDoubleFunction((f @ _)) => f.asInstanceOf[scala.Function1[Long, Double]]
    case _ => new FromJavaLongToDoubleFunction(jf)
  }
  
  @inline def asJavaLongToDoubleFunction(sf: scala.Function1[Long, Double]): java.util.function.LongToDoubleFunction = sf match {
    case FromJavaLongToDoubleFunction((f @ _)) => f.asInstanceOf[java.util.function.LongToDoubleFunction]
    case _ => new AsJavaLongToDoubleFunction(sf)
  }
  
  
  @inline def asScalaFromLongToIntFunction(jf: java.util.function.LongToIntFunction): scala.Function1[Long, Int] = jf match {
    case AsJavaLongToIntFunction((f @ _)) => f.asInstanceOf[scala.Function1[Long, Int]]
    case _ => new FromJavaLongToIntFunction(jf)
  }
  
  @inline def asJavaLongToIntFunction(sf: scala.Function1[Long, Int]): java.util.function.LongToIntFunction = sf match {
    case FromJavaLongToIntFunction((f @ _)) => f.asInstanceOf[java.util.function.LongToIntFunction]
    case _ => new AsJavaLongToIntFunction(sf)
  }
  
  
  @inline def asScalaFromLongUnaryOperator(jf: java.util.function.LongUnaryOperator): scala.Function1[Long, Long] = jf match {
    case AsJavaLongUnaryOperator((f @ _)) => f.asInstanceOf[scala.Function1[Long, Long]]
    case _ => new FromJavaLongUnaryOperator(jf)
  }
  
  @inline def asJavaLongUnaryOperator(sf: scala.Function1[Long, Long]): java.util.function.LongUnaryOperator = sf match {
    case FromJavaLongUnaryOperator((f @ _)) => f.asInstanceOf[java.util.function.LongUnaryOperator]
    case _ => new AsJavaLongUnaryOperator(sf)
  }
  
  
  @inline def asScalaFromObjDoubleConsumer[T](jf: java.util.function.ObjDoubleConsumer[T]): scala.Function2[T, Double, Unit] = jf match {
    case AsJavaObjDoubleConsumer((f @ _)) => f.asInstanceOf[scala.Function2[T, Double, Unit]]
    case _ => new FromJavaObjDoubleConsumer[T](jf)
  }
  
  @inline def asJavaObjDoubleConsumer[T](sf: scala.Function2[T, Double, Unit]): java.util.function.ObjDoubleConsumer[T] = sf match {
    case FromJavaObjDoubleConsumer((f @ _)) => f.asInstanceOf[java.util.function.ObjDoubleConsumer[T]]
    case _ => new AsJavaObjDoubleConsumer[T](sf)
  }
  
  
  @inline def asScalaFromObjIntConsumer[T](jf: java.util.function.ObjIntConsumer[T]): scala.Function2[T, Int, Unit] = jf match {
    case AsJavaObjIntConsumer((f @ _)) => f.asInstanceOf[scala.Function2[T, Int, Unit]]
    case _ => new FromJavaObjIntConsumer[T](jf)
  }
  
  @inline def asJavaObjIntConsumer[T](sf: scala.Function2[T, Int, Unit]): java.util.function.ObjIntConsumer[T] = sf match {
    case FromJavaObjIntConsumer((f @ _)) => f.asInstanceOf[java.util.function.ObjIntConsumer[T]]
    case _ => new AsJavaObjIntConsumer[T](sf)
  }
  
  
  @inline def asScalaFromObjLongConsumer[T](jf: java.util.function.ObjLongConsumer[T]): scala.Function2[T, Long, Unit] = jf match {
    case AsJavaObjLongConsumer((f @ _)) => f.asInstanceOf[scala.Function2[T, Long, Unit]]
    case _ => new FromJavaObjLongConsumer[T](jf)
  }
  
  @inline def asJavaObjLongConsumer[T](sf: scala.Function2[T, Long, Unit]): java.util.function.ObjLongConsumer[T] = sf match {
    case FromJavaObjLongConsumer((f @ _)) => f.asInstanceOf[java.util.function.ObjLongConsumer[T]]
    case _ => new AsJavaObjLongConsumer[T](sf)
  }
  
  
  @inline def asScalaFromPredicate[T](jf: java.util.function.Predicate[T]): scala.Function1[T, Boolean] = jf match {
    case AsJavaPredicate((f @ _)) => f.asInstanceOf[scala.Function1[T, Boolean]]
    case _ => new FromJavaPredicate[T](jf)
  }
  
  @inline def asJavaPredicate[T](sf: scala.Function1[T, Boolean]): java.util.function.Predicate[T] = sf match {
    case FromJavaPredicate((f @ _)) => f.asInstanceOf[java.util.function.Predicate[T]]
    case _ => new AsJavaPredicate[T](sf)
  }
  
  
  @inline def asScalaFromSupplier[T](jf: java.util.function.Supplier[T]): scala.Function0[T] = jf match {
    case AsJavaSupplier((f @ _)) => f.asInstanceOf[scala.Function0[T]]
    case _ => new FromJavaSupplier[T](jf)
  }
  
  @inline def asJavaSupplier[T](sf: scala.Function0[T]): java.util.function.Supplier[T] = sf match {
    case FromJavaSupplier((f @ _)) => f.asInstanceOf[java.util.function.Supplier[T]]
    case _ => new AsJavaSupplier[T](sf)
  }
  
  
  @inline def asScalaFromToDoubleBiFunction[T, U](jf: java.util.function.ToDoubleBiFunction[T, U]): scala.Function2[T, U, Double] = jf match {
    case AsJavaToDoubleBiFunction((f @ _)) => f.asInstanceOf[scala.Function2[T, U, Double]]
    case _ => new FromJavaToDoubleBiFunction[T, U](jf)
  }
  
  @inline def asJavaToDoubleBiFunction[T, U](sf: scala.Function2[T, U, Double]): java.util.function.ToDoubleBiFunction[T, U] = sf match {
    case FromJavaToDoubleBiFunction((f @ _)) => f.asInstanceOf[java.util.function.ToDoubleBiFunction[T, U]]
    case _ => new AsJavaToDoubleBiFunction[T, U](sf)
  }
  
  
  @inline def asScalaFromToDoubleFunction[T](jf: java.util.function.ToDoubleFunction[T]): scala.Function1[T, Double] = jf match {
    case AsJavaToDoubleFunction((f @ _)) => f.asInstanceOf[scala.Function1[T, Double]]
    case _ => new FromJavaToDoubleFunction[T](jf)
  }
  
  @inline def asJavaToDoubleFunction[T](sf: scala.Function1[T, Double]): java.util.function.ToDoubleFunction[T] = sf match {
    case FromJavaToDoubleFunction((f @ _)) => f.asInstanceOf[java.util.function.ToDoubleFunction[T]]
    case _ => new AsJavaToDoubleFunction[T](sf)
  }
  
  
  @inline def asScalaFromToIntBiFunction[T, U](jf: java.util.function.ToIntBiFunction[T, U]): scala.Function2[T, U, Int] = jf match {
    case AsJavaToIntBiFunction((f @ _)) => f.asInstanceOf[scala.Function2[T, U, Int]]
    case _ => new FromJavaToIntBiFunction[T, U](jf)
  }
  
  @inline def asJavaToIntBiFunction[T, U](sf: scala.Function2[T, U, Int]): java.util.function.ToIntBiFunction[T, U] = sf match {
    case FromJavaToIntBiFunction((f @ _)) => f.asInstanceOf[java.util.function.ToIntBiFunction[T, U]]
    case _ => new AsJavaToIntBiFunction[T, U](sf)
  }
  
  
  @inline def asScalaFromToIntFunction[T](jf: java.util.function.ToIntFunction[T]): scala.Function1[T, Int] = jf match {
    case AsJavaToIntFunction((f @ _)) => f.asInstanceOf[scala.Function1[T, Int]]
    case _ => new FromJavaToIntFunction[T](jf)
  }
  
  @inline def asJavaToIntFunction[T](sf: scala.Function1[T, Int]): java.util.function.ToIntFunction[T] = sf match {
    case FromJavaToIntFunction((f @ _)) => f.asInstanceOf[java.util.function.ToIntFunction[T]]
    case _ => new AsJavaToIntFunction[T](sf)
  }
  
  
  @inline def asScalaFromToLongBiFunction[T, U](jf: java.util.function.ToLongBiFunction[T, U]): scala.Function2[T, U, Long] = jf match {
    case AsJavaToLongBiFunction((f @ _)) => f.asInstanceOf[scala.Function2[T, U, Long]]
    case _ => new FromJavaToLongBiFunction[T, U](jf)
  }
  
  @inline def asJavaToLongBiFunction[T, U](sf: scala.Function2[T, U, Long]): java.util.function.ToLongBiFunction[T, U] = sf match {
    case FromJavaToLongBiFunction((f @ _)) => f.asInstanceOf[java.util.function.ToLongBiFunction[T, U]]
    case _ => new AsJavaToLongBiFunction[T, U](sf)
  }
  
  
  @inline def asScalaFromToLongFunction[T](jf: java.util.function.ToLongFunction[T]): scala.Function1[T, Long] = jf match {
    case AsJavaToLongFunction((f @ _)) => f.asInstanceOf[scala.Function1[T, Long]]
    case _ => new FromJavaToLongFunction[T](jf)
  }
  
  @inline def asJavaToLongFunction[T](sf: scala.Function1[T, Long]): java.util.function.ToLongFunction[T] = sf match {
    case FromJavaToLongFunction((f @ _)) => f.asInstanceOf[java.util.function.ToLongFunction[T]]
    case _ => new AsJavaToLongFunction[T](sf)
  }
  
  
  @inline def asScalaFromUnaryOperator[T](jf: java.util.function.UnaryOperator[T]): scala.Function1[T, T] = jf match {
    case AsJavaUnaryOperator((f @ _)) => f.asInstanceOf[scala.Function1[T, T]]
    case _ => new FromJavaUnaryOperator[T](jf)
  }
  
  @inline def asJavaUnaryOperator[T](sf: scala.Function1[T, T]): java.util.function.UnaryOperator[T] = sf match {
    case FromJavaUnaryOperator((f @ _)) => f.asInstanceOf[java.util.function.UnaryOperator[T]]
    case _ => new AsJavaUnaryOperator[T](sf)
  }
}
      

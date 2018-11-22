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


package scala.convert

import language.implicitConversions

trait Priority3FunctionConverters {
  import Wrappers._

  @inline implicit def enrichAsJavaBiFunction[T, U, R](sf: scala.Function2[T, U, R]): RichFunction2AsBiFunction[T, U, R] = new RichFunction2AsBiFunction[T, U, R](sf)
}



import language.implicitConversions

trait Priority2FunctionConverters extends Priority3FunctionConverters {
  import Wrappers._

  @inline implicit def enrichAsJavaBiConsumer[T, U](sf: scala.Function2[T, U, Unit]): RichFunction2AsBiConsumer[T, U] = new RichFunction2AsBiConsumer[T, U](sf)

  @inline implicit def enrichAsJavaBiPredicate[T, U](sf: scala.Function2[T, U, Boolean]): RichFunction2AsBiPredicate[T, U] = new RichFunction2AsBiPredicate[T, U](sf)

  @inline implicit def enrichAsJavaFunction[T, R](sf: scala.Function1[T, R]): RichFunction1AsFunction[T, R] = new RichFunction1AsFunction[T, R](sf)

  @inline implicit def enrichAsJavaToDoubleBiFunction[T, U](sf: scala.Function2[T, U, Double]): RichFunction2AsToDoubleBiFunction[T, U] = new RichFunction2AsToDoubleBiFunction[T, U](sf)

  @inline implicit def enrichAsJavaToIntBiFunction[T, U](sf: scala.Function2[T, U, Int]): RichFunction2AsToIntBiFunction[T, U] = new RichFunction2AsToIntBiFunction[T, U](sf)

  @inline implicit def enrichAsJavaToLongBiFunction[T, U](sf: scala.Function2[T, U, Long]): RichFunction2AsToLongBiFunction[T, U] = new RichFunction2AsToLongBiFunction[T, U](sf)
}



import language.implicitConversions

trait Priority1FunctionConverters extends Priority2FunctionConverters {
  import Wrappers._

  @inline implicit def enrichAsJavaBinaryOperator[T, A1, A2](sf: scala.Function2[T, A1, A2])(implicit evA1: =:=[A1, T], evA2: =:=[A2, T]): RichFunction2AsBinaryOperator[T] = new RichFunction2AsBinaryOperator[T](sf.asInstanceOf[scala.Function2[T, T, T]])

  @inline implicit def enrichAsJavaConsumer[T](sf: scala.Function1[T, Unit]): RichFunction1AsConsumer[T] = new RichFunction1AsConsumer[T](sf)

  @inline implicit def enrichAsJavaDoubleFunction[A0, R](sf: scala.Function1[A0, R])(implicit evA0: =:=[A0, Double]): RichFunction1AsDoubleFunction[R] = new RichFunction1AsDoubleFunction[R](sf.asInstanceOf[scala.Function1[Double, R]])

  @inline implicit def enrichAsJavaIntFunction[A0, R](sf: scala.Function1[A0, R])(implicit evA0: =:=[A0, Int]): RichFunction1AsIntFunction[R] = new RichFunction1AsIntFunction[R](sf.asInstanceOf[scala.Function1[Int, R]])

  @inline implicit def enrichAsJavaLongFunction[A0, R](sf: scala.Function1[A0, R])(implicit evA0: =:=[A0, Long]): RichFunction1AsLongFunction[R] = new RichFunction1AsLongFunction[R](sf.asInstanceOf[scala.Function1[Long, R]])

  @inline implicit def enrichAsJavaObjDoubleConsumer[T, A1](sf: scala.Function2[T, A1, Unit])(implicit evA1: =:=[A1, Double]): RichFunction2AsObjDoubleConsumer[T] = new RichFunction2AsObjDoubleConsumer[T](sf.asInstanceOf[scala.Function2[T, Double, Unit]])

  @inline implicit def enrichAsJavaObjIntConsumer[T, A1](sf: scala.Function2[T, A1, Unit])(implicit evA1: =:=[A1, Int]): RichFunction2AsObjIntConsumer[T] = new RichFunction2AsObjIntConsumer[T](sf.asInstanceOf[scala.Function2[T, Int, Unit]])

  @inline implicit def enrichAsJavaObjLongConsumer[T, A1](sf: scala.Function2[T, A1, Unit])(implicit evA1: =:=[A1, Long]): RichFunction2AsObjLongConsumer[T] = new RichFunction2AsObjLongConsumer[T](sf.asInstanceOf[scala.Function2[T, Long, Unit]])

  @inline implicit def enrichAsJavaPredicate[T](sf: scala.Function1[T, Boolean]): RichFunction1AsPredicate[T] = new RichFunction1AsPredicate[T](sf)

  @inline implicit def enrichAsJavaSupplier[T](sf: scala.Function0[T]): RichFunction0AsSupplier[T] = new RichFunction0AsSupplier[T](sf)

  @inline implicit def enrichAsJavaToDoubleFunction[T](sf: scala.Function1[T, Double]): RichFunction1AsToDoubleFunction[T] = new RichFunction1AsToDoubleFunction[T](sf)

  @inline implicit def enrichAsJavaToIntFunction[T](sf: scala.Function1[T, Int]): RichFunction1AsToIntFunction[T] = new RichFunction1AsToIntFunction[T](sf)

  @inline implicit def enrichAsJavaToLongFunction[T](sf: scala.Function1[T, Long]): RichFunction1AsToLongFunction[T] = new RichFunction1AsToLongFunction[T](sf)

  @inline implicit def enrichAsJavaUnaryOperator[T, A1](sf: scala.Function1[T, A1])(implicit evA1: =:=[A1, T]): RichFunction1AsUnaryOperator[T] = new RichFunction1AsUnaryOperator[T](sf.asInstanceOf[scala.Function1[T, T]])
}



import language.implicitConversions

trait Priority0FunctionConverters extends Priority1FunctionConverters {
  import Wrappers._

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



  @inline implicit def enrichAsJavaBooleanSupplier(sf: scala.Function0[Boolean]): RichFunction0AsBooleanSupplier = new RichFunction0AsBooleanSupplier(sf)

  @inline implicit def enrichAsJavaDoubleBinaryOperator[A0, A1](sf: scala.Function2[A0, A1, Double])(implicit evA0: =:=[A0, Double], evA1: =:=[A1, Double]): RichFunction2AsDoubleBinaryOperator = new RichFunction2AsDoubleBinaryOperator(sf.asInstanceOf[scala.Function2[Double, Double, Double]])

  @inline implicit def enrichAsJavaDoubleConsumer[A0](sf: scala.Function1[A0, Unit])(implicit evA0: =:=[A0, Double]): RichFunction1AsDoubleConsumer = new RichFunction1AsDoubleConsumer(sf.asInstanceOf[scala.Function1[Double, Unit]])

  @inline implicit def enrichAsJavaDoublePredicate[A0](sf: scala.Function1[A0, Boolean])(implicit evA0: =:=[A0, Double]): RichFunction1AsDoublePredicate = new RichFunction1AsDoublePredicate(sf.asInstanceOf[scala.Function1[Double, Boolean]])

  @inline implicit def enrichAsJavaDoubleSupplier(sf: scala.Function0[Double]): RichFunction0AsDoubleSupplier = new RichFunction0AsDoubleSupplier(sf)

  @inline implicit def enrichAsJavaDoubleToIntFunction[A0](sf: scala.Function1[A0, Int])(implicit evA0: =:=[A0, Double]): RichFunction1AsDoubleToIntFunction = new RichFunction1AsDoubleToIntFunction(sf.asInstanceOf[scala.Function1[Double, Int]])

  @inline implicit def enrichAsJavaDoubleToLongFunction[A0](sf: scala.Function1[A0, Long])(implicit evA0: =:=[A0, Double]): RichFunction1AsDoubleToLongFunction = new RichFunction1AsDoubleToLongFunction(sf.asInstanceOf[scala.Function1[Double, Long]])

  @inline implicit def enrichAsJavaDoubleUnaryOperator[A0](sf: scala.Function1[A0, Double])(implicit evA0: =:=[A0, Double]): RichFunction1AsDoubleUnaryOperator = new RichFunction1AsDoubleUnaryOperator(sf.asInstanceOf[scala.Function1[Double, Double]])

  @inline implicit def enrichAsJavaIntBinaryOperator[A0, A1](sf: scala.Function2[A0, A1, Int])(implicit evA0: =:=[A0, Int], evA1: =:=[A1, Int]): RichFunction2AsIntBinaryOperator = new RichFunction2AsIntBinaryOperator(sf.asInstanceOf[scala.Function2[Int, Int, Int]])

  @inline implicit def enrichAsJavaIntConsumer[A0](sf: scala.Function1[A0, Unit])(implicit evA0: =:=[A0, Int]): RichFunction1AsIntConsumer = new RichFunction1AsIntConsumer(sf.asInstanceOf[scala.Function1[Int, Unit]])

  @inline implicit def enrichAsJavaIntPredicate[A0](sf: scala.Function1[A0, Boolean])(implicit evA0: =:=[A0, Int]): RichFunction1AsIntPredicate = new RichFunction1AsIntPredicate(sf.asInstanceOf[scala.Function1[Int, Boolean]])

  @inline implicit def enrichAsJavaIntSupplier(sf: scala.Function0[Int]): RichFunction0AsIntSupplier = new RichFunction0AsIntSupplier(sf)

  @inline implicit def enrichAsJavaIntToDoubleFunction[A0](sf: scala.Function1[A0, Double])(implicit evA0: =:=[A0, Int]): RichFunction1AsIntToDoubleFunction = new RichFunction1AsIntToDoubleFunction(sf.asInstanceOf[scala.Function1[Int, Double]])

  @inline implicit def enrichAsJavaIntToLongFunction[A0](sf: scala.Function1[A0, Long])(implicit evA0: =:=[A0, Int]): RichFunction1AsIntToLongFunction = new RichFunction1AsIntToLongFunction(sf.asInstanceOf[scala.Function1[Int, Long]])

  @inline implicit def enrichAsJavaIntUnaryOperator[A0](sf: scala.Function1[A0, Int])(implicit evA0: =:=[A0, Int]): RichFunction1AsIntUnaryOperator = new RichFunction1AsIntUnaryOperator(sf.asInstanceOf[scala.Function1[Int, Int]])

  @inline implicit def enrichAsJavaLongBinaryOperator[A0, A1](sf: scala.Function2[A0, A1, Long])(implicit evA0: =:=[A0, Long], evA1: =:=[A1, Long]): RichFunction2AsLongBinaryOperator = new RichFunction2AsLongBinaryOperator(sf.asInstanceOf[scala.Function2[Long, Long, Long]])

  @inline implicit def enrichAsJavaLongConsumer[A0](sf: scala.Function1[A0, Unit])(implicit evA0: =:=[A0, Long]): RichFunction1AsLongConsumer = new RichFunction1AsLongConsumer(sf.asInstanceOf[scala.Function1[Long, Unit]])

  @inline implicit def enrichAsJavaLongPredicate[A0](sf: scala.Function1[A0, Boolean])(implicit evA0: =:=[A0, Long]): RichFunction1AsLongPredicate = new RichFunction1AsLongPredicate(sf.asInstanceOf[scala.Function1[Long, Boolean]])

  @inline implicit def enrichAsJavaLongSupplier(sf: scala.Function0[Long]): RichFunction0AsLongSupplier = new RichFunction0AsLongSupplier(sf)

  @inline implicit def enrichAsJavaLongToDoubleFunction[A0](sf: scala.Function1[A0, Double])(implicit evA0: =:=[A0, Long]): RichFunction1AsLongToDoubleFunction = new RichFunction1AsLongToDoubleFunction(sf.asInstanceOf[scala.Function1[Long, Double]])

  @inline implicit def enrichAsJavaLongToIntFunction[A0](sf: scala.Function1[A0, Int])(implicit evA0: =:=[A0, Long]): RichFunction1AsLongToIntFunction = new RichFunction1AsLongToIntFunction(sf.asInstanceOf[scala.Function1[Long, Int]])

  @inline implicit def enrichAsJavaLongUnaryOperator[A0](sf: scala.Function1[A0, Long])(implicit evA0: =:=[A0, Long]): RichFunction1AsLongUnaryOperator = new RichFunction1AsLongUnaryOperator(sf.asInstanceOf[scala.Function1[Long, Long]])



  @inline implicit def enrichAsScalaFromBiConsumer[T, U](jf: java.util.function.BiConsumer[T, U]): RichBiConsumerAsFunction2[T, U] = new RichBiConsumerAsFunction2[T, U](jf)

  @inline implicit def enrichAsScalaFromBiFunction[T, U, R](jf: java.util.function.BiFunction[T, U, R]): RichBiFunctionAsFunction2[T, U, R] = new RichBiFunctionAsFunction2[T, U, R](jf)

  @inline implicit def enrichAsScalaFromBiPredicate[T, U](jf: java.util.function.BiPredicate[T, U]): RichBiPredicateAsFunction2[T, U] = new RichBiPredicateAsFunction2[T, U](jf)

  @inline implicit def enrichAsScalaFromBinaryOperator[T](jf: java.util.function.BinaryOperator[T]): RichBinaryOperatorAsFunction2[T] = new RichBinaryOperatorAsFunction2[T](jf)

  @inline implicit def enrichAsScalaFromBooleanSupplier(jf: java.util.function.BooleanSupplier): RichBooleanSupplierAsFunction0 = new RichBooleanSupplierAsFunction0(jf)

  @inline implicit def enrichAsScalaFromConsumer[T](jf: java.util.function.Consumer[T]): RichConsumerAsFunction1[T] = new RichConsumerAsFunction1[T](jf)

  @inline implicit def enrichAsScalaFromDoubleBinaryOperator(jf: java.util.function.DoubleBinaryOperator): RichDoubleBinaryOperatorAsFunction2 = new RichDoubleBinaryOperatorAsFunction2(jf)

  @inline implicit def enrichAsScalaFromDoubleConsumer(jf: java.util.function.DoubleConsumer): RichDoubleConsumerAsFunction1 = new RichDoubleConsumerAsFunction1(jf)

  @inline implicit def enrichAsScalaFromDoubleFunction[R](jf: java.util.function.DoubleFunction[R]): RichDoubleFunctionAsFunction1[R] = new RichDoubleFunctionAsFunction1[R](jf)

  @inline implicit def enrichAsScalaFromDoublePredicate(jf: java.util.function.DoublePredicate): RichDoublePredicateAsFunction1 = new RichDoublePredicateAsFunction1(jf)

  @inline implicit def enrichAsScalaFromDoubleSupplier(jf: java.util.function.DoubleSupplier): RichDoubleSupplierAsFunction0 = new RichDoubleSupplierAsFunction0(jf)

  @inline implicit def enrichAsScalaFromDoubleToIntFunction(jf: java.util.function.DoubleToIntFunction): RichDoubleToIntFunctionAsFunction1 = new RichDoubleToIntFunctionAsFunction1(jf)

  @inline implicit def enrichAsScalaFromDoubleToLongFunction(jf: java.util.function.DoubleToLongFunction): RichDoubleToLongFunctionAsFunction1 = new RichDoubleToLongFunctionAsFunction1(jf)

  @inline implicit def enrichAsScalaFromDoubleUnaryOperator(jf: java.util.function.DoubleUnaryOperator): RichDoubleUnaryOperatorAsFunction1 = new RichDoubleUnaryOperatorAsFunction1(jf)

  @inline implicit def enrichAsScalaFromFunction[T, R](jf: java.util.function.Function[T, R]): RichFunctionAsFunction1[T, R] = new RichFunctionAsFunction1[T, R](jf)

  @inline implicit def enrichAsScalaFromIntBinaryOperator(jf: java.util.function.IntBinaryOperator): RichIntBinaryOperatorAsFunction2 = new RichIntBinaryOperatorAsFunction2(jf)

  @inline implicit def enrichAsScalaFromIntConsumer(jf: java.util.function.IntConsumer): RichIntConsumerAsFunction1 = new RichIntConsumerAsFunction1(jf)

  @inline implicit def enrichAsScalaFromIntFunction[R](jf: java.util.function.IntFunction[R]): RichIntFunctionAsFunction1[R] = new RichIntFunctionAsFunction1[R](jf)

  @inline implicit def enrichAsScalaFromIntPredicate(jf: java.util.function.IntPredicate): RichIntPredicateAsFunction1 = new RichIntPredicateAsFunction1(jf)

  @inline implicit def enrichAsScalaFromIntSupplier(jf: java.util.function.IntSupplier): RichIntSupplierAsFunction0 = new RichIntSupplierAsFunction0(jf)

  @inline implicit def enrichAsScalaFromIntToDoubleFunction(jf: java.util.function.IntToDoubleFunction): RichIntToDoubleFunctionAsFunction1 = new RichIntToDoubleFunctionAsFunction1(jf)

  @inline implicit def enrichAsScalaFromIntToLongFunction(jf: java.util.function.IntToLongFunction): RichIntToLongFunctionAsFunction1 = new RichIntToLongFunctionAsFunction1(jf)

  @inline implicit def enrichAsScalaFromIntUnaryOperator(jf: java.util.function.IntUnaryOperator): RichIntUnaryOperatorAsFunction1 = new RichIntUnaryOperatorAsFunction1(jf)

  @inline implicit def enrichAsScalaFromLongBinaryOperator(jf: java.util.function.LongBinaryOperator): RichLongBinaryOperatorAsFunction2 = new RichLongBinaryOperatorAsFunction2(jf)

  @inline implicit def enrichAsScalaFromLongConsumer(jf: java.util.function.LongConsumer): RichLongConsumerAsFunction1 = new RichLongConsumerAsFunction1(jf)

  @inline implicit def enrichAsScalaFromLongFunction[R](jf: java.util.function.LongFunction[R]): RichLongFunctionAsFunction1[R] = new RichLongFunctionAsFunction1[R](jf)

  @inline implicit def enrichAsScalaFromLongPredicate(jf: java.util.function.LongPredicate): RichLongPredicateAsFunction1 = new RichLongPredicateAsFunction1(jf)

  @inline implicit def enrichAsScalaFromLongSupplier(jf: java.util.function.LongSupplier): RichLongSupplierAsFunction0 = new RichLongSupplierAsFunction0(jf)

  @inline implicit def enrichAsScalaFromLongToDoubleFunction(jf: java.util.function.LongToDoubleFunction): RichLongToDoubleFunctionAsFunction1 = new RichLongToDoubleFunctionAsFunction1(jf)

  @inline implicit def enrichAsScalaFromLongToIntFunction(jf: java.util.function.LongToIntFunction): RichLongToIntFunctionAsFunction1 = new RichLongToIntFunctionAsFunction1(jf)

  @inline implicit def enrichAsScalaFromLongUnaryOperator(jf: java.util.function.LongUnaryOperator): RichLongUnaryOperatorAsFunction1 = new RichLongUnaryOperatorAsFunction1(jf)

  @inline implicit def enrichAsScalaFromObjDoubleConsumer[T](jf: java.util.function.ObjDoubleConsumer[T]): RichObjDoubleConsumerAsFunction2[T] = new RichObjDoubleConsumerAsFunction2[T](jf)

  @inline implicit def enrichAsScalaFromObjIntConsumer[T](jf: java.util.function.ObjIntConsumer[T]): RichObjIntConsumerAsFunction2[T] = new RichObjIntConsumerAsFunction2[T](jf)

  @inline implicit def enrichAsScalaFromObjLongConsumer[T](jf: java.util.function.ObjLongConsumer[T]): RichObjLongConsumerAsFunction2[T] = new RichObjLongConsumerAsFunction2[T](jf)

  @inline implicit def enrichAsScalaFromPredicate[T](jf: java.util.function.Predicate[T]): RichPredicateAsFunction1[T] = new RichPredicateAsFunction1[T](jf)

  @inline implicit def enrichAsScalaFromSupplier[T](jf: java.util.function.Supplier[T]): RichSupplierAsFunction0[T] = new RichSupplierAsFunction0[T](jf)

  @inline implicit def enrichAsScalaFromToDoubleBiFunction[T, U](jf: java.util.function.ToDoubleBiFunction[T, U]): RichToDoubleBiFunctionAsFunction2[T, U] = new RichToDoubleBiFunctionAsFunction2[T, U](jf)

  @inline implicit def enrichAsScalaFromToDoubleFunction[T](jf: java.util.function.ToDoubleFunction[T]): RichToDoubleFunctionAsFunction1[T] = new RichToDoubleFunctionAsFunction1[T](jf)

  @inline implicit def enrichAsScalaFromToIntBiFunction[T, U](jf: java.util.function.ToIntBiFunction[T, U]): RichToIntBiFunctionAsFunction2[T, U] = new RichToIntBiFunctionAsFunction2[T, U](jf)

  @inline implicit def enrichAsScalaFromToIntFunction[T](jf: java.util.function.ToIntFunction[T]): RichToIntFunctionAsFunction1[T] = new RichToIntFunctionAsFunction1[T](jf)

  @inline implicit def enrichAsScalaFromToLongBiFunction[T, U](jf: java.util.function.ToLongBiFunction[T, U]): RichToLongBiFunctionAsFunction2[T, U] = new RichToLongBiFunctionAsFunction2[T, U](jf)

  @inline implicit def enrichAsScalaFromToLongFunction[T](jf: java.util.function.ToLongFunction[T]): RichToLongFunctionAsFunction1[T] = new RichToLongFunctionAsFunction1[T](jf)

  @inline implicit def enrichAsScalaFromUnaryOperator[T](jf: java.util.function.UnaryOperator[T]): RichUnaryOperatorAsFunction1[T] = new RichUnaryOperatorAsFunction1[T](jf)
}

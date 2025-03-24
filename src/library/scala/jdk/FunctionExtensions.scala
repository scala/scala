/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

// GENERATED CODE: DO NOT EDIT.


package scala.jdk

import language.implicitConversions

trait Priority3FunctionExtensions {
  import FunctionWrappers._
  
  @inline implicit def enrichAsJavaBiFunction[T, U, R](sf: scala.Function2[T, U, R]): RichFunction2AsBiFunction[T, U, R] = new RichFunction2AsBiFunction[T, U, R](sf)
}



import language.implicitConversions

trait Priority2FunctionExtensions extends Priority3FunctionExtensions {
  import FunctionWrappers._
  
  @inline implicit def enrichAsJavaBiConsumer[T, U](sf: scala.Function2[T, U, Unit]): RichFunction2AsBiConsumer[T, U] = new RichFunction2AsBiConsumer[T, U](sf)
  
  @inline implicit def enrichAsJavaBiPredicate[T, U](sf: scala.Function2[T, U, Boolean]): RichFunction2AsBiPredicate[T, U] = new RichFunction2AsBiPredicate[T, U](sf)
  
  @inline implicit def enrichAsJavaFunction[T, R](sf: scala.Function1[T, R]): RichFunction1AsFunction[T, R] = new RichFunction1AsFunction[T, R](sf)
  
  @inline implicit def enrichAsJavaToDoubleBiFunction[T, U](sf: scala.Function2[T, U, Double]): RichFunction2AsToDoubleBiFunction[T, U] = new RichFunction2AsToDoubleBiFunction[T, U](sf)
  
  @inline implicit def enrichAsJavaToIntBiFunction[T, U](sf: scala.Function2[T, U, Int]): RichFunction2AsToIntBiFunction[T, U] = new RichFunction2AsToIntBiFunction[T, U](sf)
  
  @inline implicit def enrichAsJavaToLongBiFunction[T, U](sf: scala.Function2[T, U, Long]): RichFunction2AsToLongBiFunction[T, U] = new RichFunction2AsToLongBiFunction[T, U](sf)
}



import language.implicitConversions

trait Priority1FunctionExtensions extends Priority2FunctionExtensions {
  import FunctionWrappers._
  
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

trait Priority0FunctionExtensions extends Priority1FunctionExtensions {
  import FunctionWrappers._
  
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

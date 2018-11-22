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

object Wrappers {
  private[convert] class FromJavaBiConsumer[T, U](jf: java.util.function.BiConsumer[T, U]) extends scala.Function2[T, U, Unit] {
    def apply(x1: T, x2: U) = jf.accept(x1, x2)
  }

  private[convert] class RichBiConsumerAsFunction2[T, U](private val underlying: java.util.function.BiConsumer[T, U]) extends AnyVal {
    @inline def asScala: scala.Function2[T, U, Unit] = new FromJavaBiConsumer[T, U](underlying)
  }

  private[convert] class AsJavaBiConsumer[T, U](sf: scala.Function2[T, U, Unit]) extends java.util.function.BiConsumer[T, U] {
    def accept(x1: T, x2: U) = sf.apply(x1, x2)
  }

  private[convert] class RichFunction2AsBiConsumer[T, U](private val underlying: scala.Function2[T, U, Unit]) extends AnyVal {
    @inline def asJava: java.util.function.BiConsumer[T, U] = new AsJavaBiConsumer[T, U](underlying)
  }


  private[convert] class FromJavaBiFunction[T, U, R](jf: java.util.function.BiFunction[T, U, R]) extends scala.Function2[T, U, R] {
    def apply(x1: T, x2: U) = jf.apply(x1, x2)
  }

  private[convert] class RichBiFunctionAsFunction2[T, U, R](private val underlying: java.util.function.BiFunction[T, U, R]) extends AnyVal {
    @inline def asScala: scala.Function2[T, U, R] = new FromJavaBiFunction[T, U, R](underlying)
  }

  private[convert] class AsJavaBiFunction[T, U, R](sf: scala.Function2[T, U, R]) extends java.util.function.BiFunction[T, U, R] {
    def apply(x1: T, x2: U) = sf.apply(x1, x2)
  }

  private[convert] class RichFunction2AsBiFunction[T, U, R](private val underlying: scala.Function2[T, U, R]) extends AnyVal {
    @inline def asJava: java.util.function.BiFunction[T, U, R] = new AsJavaBiFunction[T, U, R](underlying)
  }


  private[convert] class FromJavaBiPredicate[T, U](jf: java.util.function.BiPredicate[T, U]) extends scala.Function2[T, U, Boolean] {
    def apply(x1: T, x2: U) = jf.test(x1, x2)
  }

  private[convert] class RichBiPredicateAsFunction2[T, U](private val underlying: java.util.function.BiPredicate[T, U]) extends AnyVal {
    @inline def asScala: scala.Function2[T, U, Boolean] = new FromJavaBiPredicate[T, U](underlying)
  }

  private[convert] class AsJavaBiPredicate[T, U](sf: scala.Function2[T, U, Boolean]) extends java.util.function.BiPredicate[T, U] {
    def test(x1: T, x2: U) = sf.apply(x1, x2)
  }

  private[convert] class RichFunction2AsBiPredicate[T, U](private val underlying: scala.Function2[T, U, Boolean]) extends AnyVal {
    @inline def asJava: java.util.function.BiPredicate[T, U] = new AsJavaBiPredicate[T, U](underlying)
  }


  private[convert] class FromJavaBinaryOperator[T](jf: java.util.function.BinaryOperator[T]) extends scala.Function2[T, T, T] {
    def apply(x1: T, x2: T) = jf.apply(x1, x2)
  }

  private[convert] class RichBinaryOperatorAsFunction2[T](private val underlying: java.util.function.BinaryOperator[T]) extends AnyVal {
    @inline def asScala: scala.Function2[T, T, T] = new FromJavaBinaryOperator[T](underlying)
  }

  private[convert] class AsJavaBinaryOperator[T](sf: scala.Function2[T, T, T]) extends java.util.function.BinaryOperator[T] {
    def apply(x1: T, x2: T) = sf.apply(x1, x2)
  }

  private[convert] class RichFunction2AsBinaryOperator[T](private val underlying: scala.Function2[T, T, T]) extends AnyVal {
    @inline def asJava: java.util.function.BinaryOperator[T] = new AsJavaBinaryOperator[T](underlying)
  }


  private[convert] class FromJavaBooleanSupplier(jf: java.util.function.BooleanSupplier) extends scala.Function0[Boolean] {
    def apply() = jf.getAsBoolean()
  }

  private[convert] class RichBooleanSupplierAsFunction0(private val underlying: java.util.function.BooleanSupplier) extends AnyVal {
    @inline def asScala: scala.Function0[Boolean] = new FromJavaBooleanSupplier(underlying)
  }

  private[convert] class AsJavaBooleanSupplier(sf: scala.Function0[Boolean]) extends java.util.function.BooleanSupplier {
    def getAsBoolean() = sf.apply()
  }

  private[convert] class RichFunction0AsBooleanSupplier(private val underlying: scala.Function0[Boolean]) extends AnyVal {
    @inline def asJava: java.util.function.BooleanSupplier = new AsJavaBooleanSupplier(underlying)
  }


  private[convert] class FromJavaConsumer[T](jf: java.util.function.Consumer[T]) extends scala.Function1[T, Unit] {
    def apply(x1: T) = jf.accept(x1)
  }

  private[convert] class RichConsumerAsFunction1[T](private val underlying: java.util.function.Consumer[T]) extends AnyVal {
    @inline def asScala: scala.Function1[T, Unit] = new FromJavaConsumer[T](underlying)
  }

  private[convert] class AsJavaConsumer[T](sf: scala.Function1[T, Unit]) extends java.util.function.Consumer[T] {
    def accept(x1: T) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsConsumer[T](private val underlying: scala.Function1[T, Unit]) extends AnyVal {
    @inline def asJava: java.util.function.Consumer[T] = new AsJavaConsumer[T](underlying)
  }


  private[convert] class FromJavaDoubleBinaryOperator(jf: java.util.function.DoubleBinaryOperator) extends scala.Function2[Double, Double, Double] {
    def apply(x1: scala.Double, x2: scala.Double) = jf.applyAsDouble(x1, x2)
  }

  private[convert] class RichDoubleBinaryOperatorAsFunction2(private val underlying: java.util.function.DoubleBinaryOperator) extends AnyVal {
    @inline def asScala: scala.Function2[Double, Double, Double] = new FromJavaDoubleBinaryOperator(underlying)
  }

  private[convert] class AsJavaDoubleBinaryOperator(sf: scala.Function2[Double, Double, Double]) extends java.util.function.DoubleBinaryOperator {
    def applyAsDouble(x1: scala.Double, x2: scala.Double) = sf.apply(x1, x2)
  }

  private[convert] class RichFunction2AsDoubleBinaryOperator(private val underlying: scala.Function2[Double, Double, Double]) extends AnyVal {
    @inline def asJava: java.util.function.DoubleBinaryOperator = new AsJavaDoubleBinaryOperator(underlying)
  }


  private[convert] class FromJavaDoubleConsumer(jf: java.util.function.DoubleConsumer) extends scala.Function1[Double, Unit] {
    def apply(x1: scala.Double) = jf.accept(x1)
  }

  private[convert] class RichDoubleConsumerAsFunction1(private val underlying: java.util.function.DoubleConsumer) extends AnyVal {
    @inline def asScala: scala.Function1[Double, Unit] = new FromJavaDoubleConsumer(underlying)
  }

  private[convert] class AsJavaDoubleConsumer(sf: scala.Function1[Double, Unit]) extends java.util.function.DoubleConsumer {
    def accept(x1: scala.Double) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsDoubleConsumer(private val underlying: scala.Function1[Double, Unit]) extends AnyVal {
    @inline def asJava: java.util.function.DoubleConsumer = new AsJavaDoubleConsumer(underlying)
  }


  private[convert] class FromJavaDoubleFunction[R](jf: java.util.function.DoubleFunction[R]) extends scala.Function1[Double, R] {
    def apply(x1: scala.Double) = jf.apply(x1)
  }

  private[convert] class RichDoubleFunctionAsFunction1[R](private val underlying: java.util.function.DoubleFunction[R]) extends AnyVal {
    @inline def asScala: scala.Function1[Double, R] = new FromJavaDoubleFunction[R](underlying)
  }

  private[convert] class AsJavaDoubleFunction[R](sf: scala.Function1[Double, R]) extends java.util.function.DoubleFunction[R] {
    def apply(x1: scala.Double) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsDoubleFunction[R](private val underlying: scala.Function1[Double, R]) extends AnyVal {
    @inline def asJava: java.util.function.DoubleFunction[R] = new AsJavaDoubleFunction[R](underlying)
  }


  private[convert] class FromJavaDoublePredicate(jf: java.util.function.DoublePredicate) extends scala.Function1[Double, Boolean] {
    def apply(x1: scala.Double) = jf.test(x1)
  }

  private[convert] class RichDoublePredicateAsFunction1(private val underlying: java.util.function.DoublePredicate) extends AnyVal {
    @inline def asScala: scala.Function1[Double, Boolean] = new FromJavaDoublePredicate(underlying)
  }

  private[convert] class AsJavaDoublePredicate(sf: scala.Function1[Double, Boolean]) extends java.util.function.DoublePredicate {
    def test(x1: scala.Double) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsDoublePredicate(private val underlying: scala.Function1[Double, Boolean]) extends AnyVal {
    @inline def asJava: java.util.function.DoublePredicate = new AsJavaDoublePredicate(underlying)
  }


  private[convert] class FromJavaDoubleSupplier(jf: java.util.function.DoubleSupplier) extends scala.Function0[Double] {
    def apply() = jf.getAsDouble()
  }

  private[convert] class RichDoubleSupplierAsFunction0(private val underlying: java.util.function.DoubleSupplier) extends AnyVal {
    @inline def asScala: scala.Function0[Double] = new FromJavaDoubleSupplier(underlying)
  }

  private[convert] class AsJavaDoubleSupplier(sf: scala.Function0[Double]) extends java.util.function.DoubleSupplier {
    def getAsDouble() = sf.apply()
  }

  private[convert] class RichFunction0AsDoubleSupplier(private val underlying: scala.Function0[Double]) extends AnyVal {
    @inline def asJava: java.util.function.DoubleSupplier = new AsJavaDoubleSupplier(underlying)
  }


  private[convert] class FromJavaDoubleToIntFunction(jf: java.util.function.DoubleToIntFunction) extends scala.Function1[Double, Int] {
    def apply(x1: scala.Double) = jf.applyAsInt(x1)
  }

  private[convert] class RichDoubleToIntFunctionAsFunction1(private val underlying: java.util.function.DoubleToIntFunction) extends AnyVal {
    @inline def asScala: scala.Function1[Double, Int] = new FromJavaDoubleToIntFunction(underlying)
  }

  private[convert] class AsJavaDoubleToIntFunction(sf: scala.Function1[Double, Int]) extends java.util.function.DoubleToIntFunction {
    def applyAsInt(x1: scala.Double) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsDoubleToIntFunction(private val underlying: scala.Function1[Double, Int]) extends AnyVal {
    @inline def asJava: java.util.function.DoubleToIntFunction = new AsJavaDoubleToIntFunction(underlying)
  }


  private[convert] class FromJavaDoubleToLongFunction(jf: java.util.function.DoubleToLongFunction) extends scala.Function1[Double, Long] {
    def apply(x1: scala.Double) = jf.applyAsLong(x1)
  }

  private[convert] class RichDoubleToLongFunctionAsFunction1(private val underlying: java.util.function.DoubleToLongFunction) extends AnyVal {
    @inline def asScala: scala.Function1[Double, Long] = new FromJavaDoubleToLongFunction(underlying)
  }

  private[convert] class AsJavaDoubleToLongFunction(sf: scala.Function1[Double, Long]) extends java.util.function.DoubleToLongFunction {
    def applyAsLong(x1: scala.Double) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsDoubleToLongFunction(private val underlying: scala.Function1[Double, Long]) extends AnyVal {
    @inline def asJava: java.util.function.DoubleToLongFunction = new AsJavaDoubleToLongFunction(underlying)
  }


  private[convert] class FromJavaDoubleUnaryOperator(jf: java.util.function.DoubleUnaryOperator) extends scala.Function1[Double, Double] {
    def apply(x1: scala.Double) = jf.applyAsDouble(x1)
  }

  private[convert] class RichDoubleUnaryOperatorAsFunction1(private val underlying: java.util.function.DoubleUnaryOperator) extends AnyVal {
    @inline def asScala: scala.Function1[Double, Double] = new FromJavaDoubleUnaryOperator(underlying)
  }

  private[convert] class AsJavaDoubleUnaryOperator(sf: scala.Function1[Double, Double]) extends java.util.function.DoubleUnaryOperator {
    def applyAsDouble(x1: scala.Double) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsDoubleUnaryOperator(private val underlying: scala.Function1[Double, Double]) extends AnyVal {
    @inline def asJava: java.util.function.DoubleUnaryOperator = new AsJavaDoubleUnaryOperator(underlying)
  }


  private[convert] class FromJavaFunction[T, R](jf: java.util.function.Function[T, R]) extends scala.Function1[T, R] {
    def apply(x1: T) = jf.apply(x1)
  }

  private[convert] class RichFunctionAsFunction1[T, R](private val underlying: java.util.function.Function[T, R]) extends AnyVal {
    @inline def asScala: scala.Function1[T, R] = new FromJavaFunction[T, R](underlying)
  }

  private[convert] class AsJavaFunction[T, R](sf: scala.Function1[T, R]) extends java.util.function.Function[T, R] {
    def apply(x1: T) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsFunction[T, R](private val underlying: scala.Function1[T, R]) extends AnyVal {
    @inline def asJava: java.util.function.Function[T, R] = new AsJavaFunction[T, R](underlying)
  }


  private[convert] class FromJavaIntBinaryOperator(jf: java.util.function.IntBinaryOperator) extends scala.Function2[Int, Int, Int] {
    def apply(x1: scala.Int, x2: scala.Int) = jf.applyAsInt(x1, x2)
  }

  private[convert] class RichIntBinaryOperatorAsFunction2(private val underlying: java.util.function.IntBinaryOperator) extends AnyVal {
    @inline def asScala: scala.Function2[Int, Int, Int] = new FromJavaIntBinaryOperator(underlying)
  }

  private[convert] class AsJavaIntBinaryOperator(sf: scala.Function2[Int, Int, Int]) extends java.util.function.IntBinaryOperator {
    def applyAsInt(x1: scala.Int, x2: scala.Int) = sf.apply(x1, x2)
  }

  private[convert] class RichFunction2AsIntBinaryOperator(private val underlying: scala.Function2[Int, Int, Int]) extends AnyVal {
    @inline def asJava: java.util.function.IntBinaryOperator = new AsJavaIntBinaryOperator(underlying)
  }


  private[convert] class FromJavaIntConsumer(jf: java.util.function.IntConsumer) extends scala.Function1[Int, Unit] {
    def apply(x1: scala.Int) = jf.accept(x1)
  }

  private[convert] class RichIntConsumerAsFunction1(private val underlying: java.util.function.IntConsumer) extends AnyVal {
    @inline def asScala: scala.Function1[Int, Unit] = new FromJavaIntConsumer(underlying)
  }

  private[convert] class AsJavaIntConsumer(sf: scala.Function1[Int, Unit]) extends java.util.function.IntConsumer {
    def accept(x1: scala.Int) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsIntConsumer(private val underlying: scala.Function1[Int, Unit]) extends AnyVal {
    @inline def asJava: java.util.function.IntConsumer = new AsJavaIntConsumer(underlying)
  }


  private[convert] class FromJavaIntFunction[R](jf: java.util.function.IntFunction[R]) extends scala.Function1[Int, R] {
    def apply(x1: scala.Int) = jf.apply(x1)
  }

  private[convert] class RichIntFunctionAsFunction1[R](private val underlying: java.util.function.IntFunction[R]) extends AnyVal {
    @inline def asScala: scala.Function1[Int, R] = new FromJavaIntFunction[R](underlying)
  }

  private[convert] class AsJavaIntFunction[R](sf: scala.Function1[Int, R]) extends java.util.function.IntFunction[R] {
    def apply(x1: scala.Int) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsIntFunction[R](private val underlying: scala.Function1[Int, R]) extends AnyVal {
    @inline def asJava: java.util.function.IntFunction[R] = new AsJavaIntFunction[R](underlying)
  }


  private[convert] class FromJavaIntPredicate(jf: java.util.function.IntPredicate) extends scala.Function1[Int, Boolean] {
    def apply(x1: scala.Int) = jf.test(x1)
  }

  private[convert] class RichIntPredicateAsFunction1(private val underlying: java.util.function.IntPredicate) extends AnyVal {
    @inline def asScala: scala.Function1[Int, Boolean] = new FromJavaIntPredicate(underlying)
  }

  private[convert] class AsJavaIntPredicate(sf: scala.Function1[Int, Boolean]) extends java.util.function.IntPredicate {
    def test(x1: scala.Int) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsIntPredicate(private val underlying: scala.Function1[Int, Boolean]) extends AnyVal {
    @inline def asJava: java.util.function.IntPredicate = new AsJavaIntPredicate(underlying)
  }


  private[convert] class FromJavaIntSupplier(jf: java.util.function.IntSupplier) extends scala.Function0[Int] {
    def apply() = jf.getAsInt()
  }

  private[convert] class RichIntSupplierAsFunction0(private val underlying: java.util.function.IntSupplier) extends AnyVal {
    @inline def asScala: scala.Function0[Int] = new FromJavaIntSupplier(underlying)
  }

  private[convert] class AsJavaIntSupplier(sf: scala.Function0[Int]) extends java.util.function.IntSupplier {
    def getAsInt() = sf.apply()
  }

  private[convert] class RichFunction0AsIntSupplier(private val underlying: scala.Function0[Int]) extends AnyVal {
    @inline def asJava: java.util.function.IntSupplier = new AsJavaIntSupplier(underlying)
  }


  private[convert] class FromJavaIntToDoubleFunction(jf: java.util.function.IntToDoubleFunction) extends scala.Function1[Int, Double] {
    def apply(x1: scala.Int) = jf.applyAsDouble(x1)
  }

  private[convert] class RichIntToDoubleFunctionAsFunction1(private val underlying: java.util.function.IntToDoubleFunction) extends AnyVal {
    @inline def asScala: scala.Function1[Int, Double] = new FromJavaIntToDoubleFunction(underlying)
  }

  private[convert] class AsJavaIntToDoubleFunction(sf: scala.Function1[Int, Double]) extends java.util.function.IntToDoubleFunction {
    def applyAsDouble(x1: scala.Int) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsIntToDoubleFunction(private val underlying: scala.Function1[Int, Double]) extends AnyVal {
    @inline def asJava: java.util.function.IntToDoubleFunction = new AsJavaIntToDoubleFunction(underlying)
  }


  private[convert] class FromJavaIntToLongFunction(jf: java.util.function.IntToLongFunction) extends scala.Function1[Int, Long] {
    def apply(x1: scala.Int) = jf.applyAsLong(x1)
  }

  private[convert] class RichIntToLongFunctionAsFunction1(private val underlying: java.util.function.IntToLongFunction) extends AnyVal {
    @inline def asScala: scala.Function1[Int, Long] = new FromJavaIntToLongFunction(underlying)
  }

  private[convert] class AsJavaIntToLongFunction(sf: scala.Function1[Int, Long]) extends java.util.function.IntToLongFunction {
    def applyAsLong(x1: scala.Int) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsIntToLongFunction(private val underlying: scala.Function1[Int, Long]) extends AnyVal {
    @inline def asJava: java.util.function.IntToLongFunction = new AsJavaIntToLongFunction(underlying)
  }


  private[convert] class FromJavaIntUnaryOperator(jf: java.util.function.IntUnaryOperator) extends scala.Function1[Int, Int] {
    def apply(x1: scala.Int) = jf.applyAsInt(x1)
  }

  private[convert] class RichIntUnaryOperatorAsFunction1(private val underlying: java.util.function.IntUnaryOperator) extends AnyVal {
    @inline def asScala: scala.Function1[Int, Int] = new FromJavaIntUnaryOperator(underlying)
  }

  private[convert] class AsJavaIntUnaryOperator(sf: scala.Function1[Int, Int]) extends java.util.function.IntUnaryOperator {
    def applyAsInt(x1: scala.Int) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsIntUnaryOperator(private val underlying: scala.Function1[Int, Int]) extends AnyVal {
    @inline def asJava: java.util.function.IntUnaryOperator = new AsJavaIntUnaryOperator(underlying)
  }


  private[convert] class FromJavaLongBinaryOperator(jf: java.util.function.LongBinaryOperator) extends scala.Function2[Long, Long, Long] {
    def apply(x1: scala.Long, x2: scala.Long) = jf.applyAsLong(x1, x2)
  }

  private[convert] class RichLongBinaryOperatorAsFunction2(private val underlying: java.util.function.LongBinaryOperator) extends AnyVal {
    @inline def asScala: scala.Function2[Long, Long, Long] = new FromJavaLongBinaryOperator(underlying)
  }

  private[convert] class AsJavaLongBinaryOperator(sf: scala.Function2[Long, Long, Long]) extends java.util.function.LongBinaryOperator {
    def applyAsLong(x1: scala.Long, x2: scala.Long) = sf.apply(x1, x2)
  }

  private[convert] class RichFunction2AsLongBinaryOperator(private val underlying: scala.Function2[Long, Long, Long]) extends AnyVal {
    @inline def asJava: java.util.function.LongBinaryOperator = new AsJavaLongBinaryOperator(underlying)
  }


  private[convert] class FromJavaLongConsumer(jf: java.util.function.LongConsumer) extends scala.Function1[Long, Unit] {
    def apply(x1: scala.Long) = jf.accept(x1)
  }

  private[convert] class RichLongConsumerAsFunction1(private val underlying: java.util.function.LongConsumer) extends AnyVal {
    @inline def asScala: scala.Function1[Long, Unit] = new FromJavaLongConsumer(underlying)
  }

  private[convert] class AsJavaLongConsumer(sf: scala.Function1[Long, Unit]) extends java.util.function.LongConsumer {
    def accept(x1: scala.Long) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsLongConsumer(private val underlying: scala.Function1[Long, Unit]) extends AnyVal {
    @inline def asJava: java.util.function.LongConsumer = new AsJavaLongConsumer(underlying)
  }


  private[convert] class FromJavaLongFunction[R](jf: java.util.function.LongFunction[R]) extends scala.Function1[Long, R] {
    def apply(x1: scala.Long) = jf.apply(x1)
  }

  private[convert] class RichLongFunctionAsFunction1[R](private val underlying: java.util.function.LongFunction[R]) extends AnyVal {
    @inline def asScala: scala.Function1[Long, R] = new FromJavaLongFunction[R](underlying)
  }

  private[convert] class AsJavaLongFunction[R](sf: scala.Function1[Long, R]) extends java.util.function.LongFunction[R] {
    def apply(x1: scala.Long) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsLongFunction[R](private val underlying: scala.Function1[Long, R]) extends AnyVal {
    @inline def asJava: java.util.function.LongFunction[R] = new AsJavaLongFunction[R](underlying)
  }


  private[convert] class FromJavaLongPredicate(jf: java.util.function.LongPredicate) extends scala.Function1[Long, Boolean] {
    def apply(x1: scala.Long) = jf.test(x1)
  }

  private[convert] class RichLongPredicateAsFunction1(private val underlying: java.util.function.LongPredicate) extends AnyVal {
    @inline def asScala: scala.Function1[Long, Boolean] = new FromJavaLongPredicate(underlying)
  }

  private[convert] class AsJavaLongPredicate(sf: scala.Function1[Long, Boolean]) extends java.util.function.LongPredicate {
    def test(x1: scala.Long) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsLongPredicate(private val underlying: scala.Function1[Long, Boolean]) extends AnyVal {
    @inline def asJava: java.util.function.LongPredicate = new AsJavaLongPredicate(underlying)
  }


  private[convert] class FromJavaLongSupplier(jf: java.util.function.LongSupplier) extends scala.Function0[Long] {
    def apply() = jf.getAsLong()
  }

  private[convert] class RichLongSupplierAsFunction0(private val underlying: java.util.function.LongSupplier) extends AnyVal {
    @inline def asScala: scala.Function0[Long] = new FromJavaLongSupplier(underlying)
  }

  private[convert] class AsJavaLongSupplier(sf: scala.Function0[Long]) extends java.util.function.LongSupplier {
    def getAsLong() = sf.apply()
  }

  private[convert] class RichFunction0AsLongSupplier(private val underlying: scala.Function0[Long]) extends AnyVal {
    @inline def asJava: java.util.function.LongSupplier = new AsJavaLongSupplier(underlying)
  }


  private[convert] class FromJavaLongToDoubleFunction(jf: java.util.function.LongToDoubleFunction) extends scala.Function1[Long, Double] {
    def apply(x1: scala.Long) = jf.applyAsDouble(x1)
  }

  private[convert] class RichLongToDoubleFunctionAsFunction1(private val underlying: java.util.function.LongToDoubleFunction) extends AnyVal {
    @inline def asScala: scala.Function1[Long, Double] = new FromJavaLongToDoubleFunction(underlying)
  }

  private[convert] class AsJavaLongToDoubleFunction(sf: scala.Function1[Long, Double]) extends java.util.function.LongToDoubleFunction {
    def applyAsDouble(x1: scala.Long) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsLongToDoubleFunction(private val underlying: scala.Function1[Long, Double]) extends AnyVal {
    @inline def asJava: java.util.function.LongToDoubleFunction = new AsJavaLongToDoubleFunction(underlying)
  }


  private[convert] class FromJavaLongToIntFunction(jf: java.util.function.LongToIntFunction) extends scala.Function1[Long, Int] {
    def apply(x1: scala.Long) = jf.applyAsInt(x1)
  }

  private[convert] class RichLongToIntFunctionAsFunction1(private val underlying: java.util.function.LongToIntFunction) extends AnyVal {
    @inline def asScala: scala.Function1[Long, Int] = new FromJavaLongToIntFunction(underlying)
  }

  private[convert] class AsJavaLongToIntFunction(sf: scala.Function1[Long, Int]) extends java.util.function.LongToIntFunction {
    def applyAsInt(x1: scala.Long) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsLongToIntFunction(private val underlying: scala.Function1[Long, Int]) extends AnyVal {
    @inline def asJava: java.util.function.LongToIntFunction = new AsJavaLongToIntFunction(underlying)
  }


  private[convert] class FromJavaLongUnaryOperator(jf: java.util.function.LongUnaryOperator) extends scala.Function1[Long, Long] {
    def apply(x1: scala.Long) = jf.applyAsLong(x1)
  }

  private[convert] class RichLongUnaryOperatorAsFunction1(private val underlying: java.util.function.LongUnaryOperator) extends AnyVal {
    @inline def asScala: scala.Function1[Long, Long] = new FromJavaLongUnaryOperator(underlying)
  }

  private[convert] class AsJavaLongUnaryOperator(sf: scala.Function1[Long, Long]) extends java.util.function.LongUnaryOperator {
    def applyAsLong(x1: scala.Long) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsLongUnaryOperator(private val underlying: scala.Function1[Long, Long]) extends AnyVal {
    @inline def asJava: java.util.function.LongUnaryOperator = new AsJavaLongUnaryOperator(underlying)
  }


  private[convert] class FromJavaObjDoubleConsumer[T](jf: java.util.function.ObjDoubleConsumer[T]) extends scala.Function2[T, Double, Unit] {
    def apply(x1: T, x2: scala.Double) = jf.accept(x1, x2)
  }

  private[convert] class RichObjDoubleConsumerAsFunction2[T](private val underlying: java.util.function.ObjDoubleConsumer[T]) extends AnyVal {
    @inline def asScala: scala.Function2[T, Double, Unit] = new FromJavaObjDoubleConsumer[T](underlying)
  }

  private[convert] class AsJavaObjDoubleConsumer[T](sf: scala.Function2[T, Double, Unit]) extends java.util.function.ObjDoubleConsumer[T] {
    def accept(x1: T, x2: scala.Double) = sf.apply(x1, x2)
  }

  private[convert] class RichFunction2AsObjDoubleConsumer[T](private val underlying: scala.Function2[T, Double, Unit]) extends AnyVal {
    @inline def asJava: java.util.function.ObjDoubleConsumer[T] = new AsJavaObjDoubleConsumer[T](underlying)
  }


  private[convert] class FromJavaObjIntConsumer[T](jf: java.util.function.ObjIntConsumer[T]) extends scala.Function2[T, Int, Unit] {
    def apply(x1: T, x2: scala.Int) = jf.accept(x1, x2)
  }

  private[convert] class RichObjIntConsumerAsFunction2[T](private val underlying: java.util.function.ObjIntConsumer[T]) extends AnyVal {
    @inline def asScala: scala.Function2[T, Int, Unit] = new FromJavaObjIntConsumer[T](underlying)
  }

  private[convert] class AsJavaObjIntConsumer[T](sf: scala.Function2[T, Int, Unit]) extends java.util.function.ObjIntConsumer[T] {
    def accept(x1: T, x2: scala.Int) = sf.apply(x1, x2)
  }

  private[convert] class RichFunction2AsObjIntConsumer[T](private val underlying: scala.Function2[T, Int, Unit]) extends AnyVal {
    @inline def asJava: java.util.function.ObjIntConsumer[T] = new AsJavaObjIntConsumer[T](underlying)
  }


  private[convert] class FromJavaObjLongConsumer[T](jf: java.util.function.ObjLongConsumer[T]) extends scala.Function2[T, Long, Unit] {
    def apply(x1: T, x2: scala.Long) = jf.accept(x1, x2)
  }

  private[convert] class RichObjLongConsumerAsFunction2[T](private val underlying: java.util.function.ObjLongConsumer[T]) extends AnyVal {
    @inline def asScala: scala.Function2[T, Long, Unit] = new FromJavaObjLongConsumer[T](underlying)
  }

  private[convert] class AsJavaObjLongConsumer[T](sf: scala.Function2[T, Long, Unit]) extends java.util.function.ObjLongConsumer[T] {
    def accept(x1: T, x2: scala.Long) = sf.apply(x1, x2)
  }

  private[convert] class RichFunction2AsObjLongConsumer[T](private val underlying: scala.Function2[T, Long, Unit]) extends AnyVal {
    @inline def asJava: java.util.function.ObjLongConsumer[T] = new AsJavaObjLongConsumer[T](underlying)
  }


  private[convert] class FromJavaPredicate[T](jf: java.util.function.Predicate[T]) extends scala.Function1[T, Boolean] {
    def apply(x1: T) = jf.test(x1)
  }

  private[convert] class RichPredicateAsFunction1[T](private val underlying: java.util.function.Predicate[T]) extends AnyVal {
    @inline def asScala: scala.Function1[T, Boolean] = new FromJavaPredicate[T](underlying)
  }

  private[convert] class AsJavaPredicate[T](sf: scala.Function1[T, Boolean]) extends java.util.function.Predicate[T] {
    def test(x1: T) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsPredicate[T](private val underlying: scala.Function1[T, Boolean]) extends AnyVal {
    @inline def asJava: java.util.function.Predicate[T] = new AsJavaPredicate[T](underlying)
  }


  private[convert] class FromJavaSupplier[T](jf: java.util.function.Supplier[T]) extends scala.Function0[T] {
    def apply() = jf.get()
  }

  private[convert] class RichSupplierAsFunction0[T](private val underlying: java.util.function.Supplier[T]) extends AnyVal {
    @inline def asScala: scala.Function0[T] = new FromJavaSupplier[T](underlying)
  }

  private[convert] class AsJavaSupplier[T](sf: scala.Function0[T]) extends java.util.function.Supplier[T] {
    def get() = sf.apply()
  }

  private[convert] class RichFunction0AsSupplier[T](private val underlying: scala.Function0[T]) extends AnyVal {
    @inline def asJava: java.util.function.Supplier[T] = new AsJavaSupplier[T](underlying)
  }


  private[convert] class FromJavaToDoubleBiFunction[T, U](jf: java.util.function.ToDoubleBiFunction[T, U]) extends scala.Function2[T, U, Double] {
    def apply(x1: T, x2: U) = jf.applyAsDouble(x1, x2)
  }

  private[convert] class RichToDoubleBiFunctionAsFunction2[T, U](private val underlying: java.util.function.ToDoubleBiFunction[T, U]) extends AnyVal {
    @inline def asScala: scala.Function2[T, U, Double] = new FromJavaToDoubleBiFunction[T, U](underlying)
  }

  private[convert] class AsJavaToDoubleBiFunction[T, U](sf: scala.Function2[T, U, Double]) extends java.util.function.ToDoubleBiFunction[T, U] {
    def applyAsDouble(x1: T, x2: U) = sf.apply(x1, x2)
  }

  private[convert] class RichFunction2AsToDoubleBiFunction[T, U](private val underlying: scala.Function2[T, U, Double]) extends AnyVal {
    @inline def asJava: java.util.function.ToDoubleBiFunction[T, U] = new AsJavaToDoubleBiFunction[T, U](underlying)
  }


  private[convert] class FromJavaToDoubleFunction[T](jf: java.util.function.ToDoubleFunction[T]) extends scala.Function1[T, Double] {
    def apply(x1: T) = jf.applyAsDouble(x1)
  }

  private[convert] class RichToDoubleFunctionAsFunction1[T](private val underlying: java.util.function.ToDoubleFunction[T]) extends AnyVal {
    @inline def asScala: scala.Function1[T, Double] = new FromJavaToDoubleFunction[T](underlying)
  }

  private[convert] class AsJavaToDoubleFunction[T](sf: scala.Function1[T, Double]) extends java.util.function.ToDoubleFunction[T] {
    def applyAsDouble(x1: T) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsToDoubleFunction[T](private val underlying: scala.Function1[T, Double]) extends AnyVal {
    @inline def asJava: java.util.function.ToDoubleFunction[T] = new AsJavaToDoubleFunction[T](underlying)
  }


  private[convert] class FromJavaToIntBiFunction[T, U](jf: java.util.function.ToIntBiFunction[T, U]) extends scala.Function2[T, U, Int] {
    def apply(x1: T, x2: U) = jf.applyAsInt(x1, x2)
  }

  private[convert] class RichToIntBiFunctionAsFunction2[T, U](private val underlying: java.util.function.ToIntBiFunction[T, U]) extends AnyVal {
    @inline def asScala: scala.Function2[T, U, Int] = new FromJavaToIntBiFunction[T, U](underlying)
  }

  private[convert] class AsJavaToIntBiFunction[T, U](sf: scala.Function2[T, U, Int]) extends java.util.function.ToIntBiFunction[T, U] {
    def applyAsInt(x1: T, x2: U) = sf.apply(x1, x2)
  }

  private[convert] class RichFunction2AsToIntBiFunction[T, U](private val underlying: scala.Function2[T, U, Int]) extends AnyVal {
    @inline def asJava: java.util.function.ToIntBiFunction[T, U] = new AsJavaToIntBiFunction[T, U](underlying)
  }


  private[convert] class FromJavaToIntFunction[T](jf: java.util.function.ToIntFunction[T]) extends scala.Function1[T, Int] {
    def apply(x1: T) = jf.applyAsInt(x1)
  }

  private[convert] class RichToIntFunctionAsFunction1[T](private val underlying: java.util.function.ToIntFunction[T]) extends AnyVal {
    @inline def asScala: scala.Function1[T, Int] = new FromJavaToIntFunction[T](underlying)
  }

  private[convert] class AsJavaToIntFunction[T](sf: scala.Function1[T, Int]) extends java.util.function.ToIntFunction[T] {
    def applyAsInt(x1: T) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsToIntFunction[T](private val underlying: scala.Function1[T, Int]) extends AnyVal {
    @inline def asJava: java.util.function.ToIntFunction[T] = new AsJavaToIntFunction[T](underlying)
  }


  private[convert] class FromJavaToLongBiFunction[T, U](jf: java.util.function.ToLongBiFunction[T, U]) extends scala.Function2[T, U, Long] {
    def apply(x1: T, x2: U) = jf.applyAsLong(x1, x2)
  }

  private[convert] class RichToLongBiFunctionAsFunction2[T, U](private val underlying: java.util.function.ToLongBiFunction[T, U]) extends AnyVal {
    @inline def asScala: scala.Function2[T, U, Long] = new FromJavaToLongBiFunction[T, U](underlying)
  }

  private[convert] class AsJavaToLongBiFunction[T, U](sf: scala.Function2[T, U, Long]) extends java.util.function.ToLongBiFunction[T, U] {
    def applyAsLong(x1: T, x2: U) = sf.apply(x1, x2)
  }

  private[convert] class RichFunction2AsToLongBiFunction[T, U](private val underlying: scala.Function2[T, U, Long]) extends AnyVal {
    @inline def asJava: java.util.function.ToLongBiFunction[T, U] = new AsJavaToLongBiFunction[T, U](underlying)
  }


  private[convert] class FromJavaToLongFunction[T](jf: java.util.function.ToLongFunction[T]) extends scala.Function1[T, Long] {
    def apply(x1: T) = jf.applyAsLong(x1)
  }

  private[convert] class RichToLongFunctionAsFunction1[T](private val underlying: java.util.function.ToLongFunction[T]) extends AnyVal {
    @inline def asScala: scala.Function1[T, Long] = new FromJavaToLongFunction[T](underlying)
  }

  private[convert] class AsJavaToLongFunction[T](sf: scala.Function1[T, Long]) extends java.util.function.ToLongFunction[T] {
    def applyAsLong(x1: T) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsToLongFunction[T](private val underlying: scala.Function1[T, Long]) extends AnyVal {
    @inline def asJava: java.util.function.ToLongFunction[T] = new AsJavaToLongFunction[T](underlying)
  }


  private[convert] class FromJavaUnaryOperator[T](jf: java.util.function.UnaryOperator[T]) extends scala.Function1[T, T] {
    def apply(x1: T) = jf.apply(x1)
  }

  private[convert] class RichUnaryOperatorAsFunction1[T](private val underlying: java.util.function.UnaryOperator[T]) extends AnyVal {
    @inline def asScala: scala.Function1[T, T] = new FromJavaUnaryOperator[T](underlying)
  }

  private[convert] class AsJavaUnaryOperator[T](sf: scala.Function1[T, T]) extends java.util.function.UnaryOperator[T] {
    def apply(x1: T) = sf.apply(x1)
  }

  private[convert] class RichFunction1AsUnaryOperator[T](private val underlying: scala.Function1[T, T]) extends AnyVal {
    @inline def asJava: java.util.function.UnaryOperator[T] = new AsJavaUnaryOperator[T](underlying)
  }
}

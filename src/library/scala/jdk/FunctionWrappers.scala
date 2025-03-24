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

object FunctionWrappers {
  case class FromJavaBiConsumer[T, U](jf: java.util.function.BiConsumer[T, U]) extends scala.Function2[T, U, Unit] {
    def apply(x1: T, x2: U) = jf.accept(x1, x2)
  }
  
  class RichBiConsumerAsFunction2[T, U](private val underlying: java.util.function.BiConsumer[T, U]) extends AnyVal {
    @inline def asScala: scala.Function2[T, U, Unit] = underlying match {
      case AsJavaBiConsumer((sf @ _)) => sf.asInstanceOf[scala.Function2[T, U, Unit]]
      case _ => new FromJavaBiConsumer[T, U](underlying)
    }
  }
  
  case class AsJavaBiConsumer[T, U](sf: scala.Function2[T, U, Unit]) extends java.util.function.BiConsumer[T, U] {
    def accept(x1: T, x2: U) = sf.apply(x1, x2)
  }
  
  class RichFunction2AsBiConsumer[T, U](private val underlying: scala.Function2[T, U, Unit]) extends AnyVal {
    @inline def asJava: java.util.function.BiConsumer[T, U] = underlying match {
      case FromJavaBiConsumer((jf @ _)) => jf.asInstanceOf[java.util.function.BiConsumer[T, U]]
      case _ => new AsJavaBiConsumer[T, U](underlying)
    };
    @inline def asJavaBiConsumer: java.util.function.BiConsumer[T, U] = underlying match {
      case FromJavaBiConsumer((sf @ _)) => sf.asInstanceOf[java.util.function.BiConsumer[T, U]]
      case _ => new AsJavaBiConsumer[T, U](underlying)
    }
  }
  
  
  case class FromJavaBiFunction[T, U, R](jf: java.util.function.BiFunction[T, U, R]) extends scala.Function2[T, U, R] {
    def apply(x1: T, x2: U) = jf.apply(x1, x2)
  }
  
  class RichBiFunctionAsFunction2[T, U, R](private val underlying: java.util.function.BiFunction[T, U, R]) extends AnyVal {
    @inline def asScala: scala.Function2[T, U, R] = underlying match {
      case AsJavaBiFunction((sf @ _)) => sf.asInstanceOf[scala.Function2[T, U, R]]
      case _ => new FromJavaBiFunction[T, U, R](underlying)
    }
  }
  
  case class AsJavaBiFunction[T, U, R](sf: scala.Function2[T, U, R]) extends java.util.function.BiFunction[T, U, R] {
    def apply(x1: T, x2: U) = sf.apply(x1, x2)
  }
  
  class RichFunction2AsBiFunction[T, U, R](private val underlying: scala.Function2[T, U, R]) extends AnyVal {
    @inline def asJava: java.util.function.BiFunction[T, U, R] = underlying match {
      case FromJavaBiFunction((jf @ _)) => jf.asInstanceOf[java.util.function.BiFunction[T, U, R]]
      case _ => new AsJavaBiFunction[T, U, R](underlying)
    };
    @inline def asJavaBiFunction: java.util.function.BiFunction[T, U, R] = underlying match {
      case FromJavaBiFunction((sf @ _)) => sf.asInstanceOf[java.util.function.BiFunction[T, U, R]]
      case _ => new AsJavaBiFunction[T, U, R](underlying)
    }
  }
  
  
  case class FromJavaBiPredicate[T, U](jf: java.util.function.BiPredicate[T, U]) extends scala.Function2[T, U, Boolean] {
    def apply(x1: T, x2: U) = jf.test(x1, x2)
  }
  
  class RichBiPredicateAsFunction2[T, U](private val underlying: java.util.function.BiPredicate[T, U]) extends AnyVal {
    @inline def asScala: scala.Function2[T, U, Boolean] = underlying match {
      case AsJavaBiPredicate((sf @ _)) => sf.asInstanceOf[scala.Function2[T, U, Boolean]]
      case _ => new FromJavaBiPredicate[T, U](underlying)
    }
  }
  
  case class AsJavaBiPredicate[T, U](sf: scala.Function2[T, U, Boolean]) extends java.util.function.BiPredicate[T, U] {
    def test(x1: T, x2: U) = sf.apply(x1, x2)
  }
  
  class RichFunction2AsBiPredicate[T, U](private val underlying: scala.Function2[T, U, Boolean]) extends AnyVal {
    @inline def asJava: java.util.function.BiPredicate[T, U] = underlying match {
      case FromJavaBiPredicate((jf @ _)) => jf.asInstanceOf[java.util.function.BiPredicate[T, U]]
      case _ => new AsJavaBiPredicate[T, U](underlying)
    };
    @inline def asJavaBiPredicate: java.util.function.BiPredicate[T, U] = underlying match {
      case FromJavaBiPredicate((sf @ _)) => sf.asInstanceOf[java.util.function.BiPredicate[T, U]]
      case _ => new AsJavaBiPredicate[T, U](underlying)
    }
  }
  
  
  case class FromJavaBinaryOperator[T](jf: java.util.function.BinaryOperator[T]) extends scala.Function2[T, T, T] {
    def apply(x1: T, x2: T) = jf.apply(x1, x2)
  }
  
  class RichBinaryOperatorAsFunction2[T](private val underlying: java.util.function.BinaryOperator[T]) extends AnyVal {
    @inline def asScala: scala.Function2[T, T, T] = underlying match {
      case AsJavaBinaryOperator((sf @ _)) => sf.asInstanceOf[scala.Function2[T, T, T]]
      case _ => new FromJavaBinaryOperator[T](underlying)
    }
  }
  
  case class AsJavaBinaryOperator[T](sf: scala.Function2[T, T, T]) extends java.util.function.BinaryOperator[T] {
    def apply(x1: T, x2: T) = sf.apply(x1, x2)
  }
  
  class RichFunction2AsBinaryOperator[T](private val underlying: scala.Function2[T, T, T]) extends AnyVal {
    @inline def asJava: java.util.function.BinaryOperator[T] = underlying match {
      case FromJavaBinaryOperator((jf @ _)) => jf.asInstanceOf[java.util.function.BinaryOperator[T]]
      case _ => new AsJavaBinaryOperator[T](underlying)
    };
    @inline def asJavaBinaryOperator: java.util.function.BinaryOperator[T] = underlying match {
      case FromJavaBinaryOperator((sf @ _)) => sf.asInstanceOf[java.util.function.BinaryOperator[T]]
      case _ => new AsJavaBinaryOperator[T](underlying)
    }
  }
  
  
  case class FromJavaBooleanSupplier(jf: java.util.function.BooleanSupplier) extends scala.Function0[Boolean] {
    def apply() = jf.getAsBoolean()
  }
  
  class RichBooleanSupplierAsFunction0(private val underlying: java.util.function.BooleanSupplier) extends AnyVal {
    @inline def asScala: scala.Function0[Boolean] = underlying match {
      case AsJavaBooleanSupplier((sf @ _)) => sf.asInstanceOf[scala.Function0[Boolean]]
      case _ => new FromJavaBooleanSupplier(underlying)
    }
  }
  
  case class AsJavaBooleanSupplier(sf: scala.Function0[Boolean]) extends java.util.function.BooleanSupplier {
    def getAsBoolean() = sf.apply()
  }
  
  class RichFunction0AsBooleanSupplier(private val underlying: scala.Function0[Boolean]) extends AnyVal {
    @inline def asJava: java.util.function.BooleanSupplier = underlying match {
      case FromJavaBooleanSupplier((jf @ _)) => jf.asInstanceOf[java.util.function.BooleanSupplier]
      case _ => new AsJavaBooleanSupplier(underlying)
    }
  }
  
  
  case class FromJavaConsumer[T](jf: java.util.function.Consumer[T]) extends scala.Function1[T, Unit] {
    def apply(x1: T) = jf.accept(x1)
  }
  
  class RichConsumerAsFunction1[T](private val underlying: java.util.function.Consumer[T]) extends AnyVal {
    @inline def asScala: scala.Function1[T, Unit] = underlying match {
      case AsJavaConsumer((sf @ _)) => sf.asInstanceOf[scala.Function1[T, Unit]]
      case _ => new FromJavaConsumer[T](underlying)
    }
  }
  
  case class AsJavaConsumer[T](sf: scala.Function1[T, Unit]) extends java.util.function.Consumer[T] {
    def accept(x1: T) = sf.apply(x1)
  }
  
  class RichFunction1AsConsumer[T](private val underlying: scala.Function1[T, Unit]) extends AnyVal {
    @inline def asJava: java.util.function.Consumer[T] = underlying match {
      case FromJavaConsumer((jf @ _)) => jf.asInstanceOf[java.util.function.Consumer[T]]
      case _ => new AsJavaConsumer[T](underlying)
    };
    @inline def asJavaConsumer: java.util.function.Consumer[T] = underlying match {
      case FromJavaConsumer((sf @ _)) => sf.asInstanceOf[java.util.function.Consumer[T]]
      case _ => new AsJavaConsumer[T](underlying)
    }
  }
  
  
  case class FromJavaDoubleBinaryOperator(jf: java.util.function.DoubleBinaryOperator) extends scala.Function2[Double, Double, Double] {
    def apply(x1: scala.Double, x2: scala.Double) = jf.applyAsDouble(x1, x2)
  }
  
  class RichDoubleBinaryOperatorAsFunction2(private val underlying: java.util.function.DoubleBinaryOperator) extends AnyVal {
    @inline def asScala: scala.Function2[Double, Double, Double] = underlying match {
      case AsJavaDoubleBinaryOperator((sf @ _)) => sf.asInstanceOf[scala.Function2[Double, Double, Double]]
      case _ => new FromJavaDoubleBinaryOperator(underlying)
    }
  }
  
  case class AsJavaDoubleBinaryOperator(sf: scala.Function2[Double, Double, Double]) extends java.util.function.DoubleBinaryOperator {
    def applyAsDouble(x1: scala.Double, x2: scala.Double) = sf.apply(x1, x2)
  }
  
  class RichFunction2AsDoubleBinaryOperator(private val underlying: scala.Function2[Double, Double, Double]) extends AnyVal {
    @inline def asJava: java.util.function.DoubleBinaryOperator = underlying match {
      case FromJavaDoubleBinaryOperator((jf @ _)) => jf.asInstanceOf[java.util.function.DoubleBinaryOperator]
      case _ => new AsJavaDoubleBinaryOperator(underlying)
    }
  }
  
  
  case class FromJavaDoubleConsumer(jf: java.util.function.DoubleConsumer) extends scala.Function1[Double, Unit] {
    def apply(x1: scala.Double) = jf.accept(x1)
  }
  
  class RichDoubleConsumerAsFunction1(private val underlying: java.util.function.DoubleConsumer) extends AnyVal {
    @inline def asScala: scala.Function1[Double, Unit] = underlying match {
      case AsJavaDoubleConsumer((sf @ _)) => sf.asInstanceOf[scala.Function1[Double, Unit]]
      case _ => new FromJavaDoubleConsumer(underlying)
    }
  }
  
  case class AsJavaDoubleConsumer(sf: scala.Function1[Double, Unit]) extends java.util.function.DoubleConsumer {
    def accept(x1: scala.Double) = sf.apply(x1)
  }
  
  class RichFunction1AsDoubleConsumer(private val underlying: scala.Function1[Double, Unit]) extends AnyVal {
    @inline def asJava: java.util.function.DoubleConsumer = underlying match {
      case FromJavaDoubleConsumer((jf @ _)) => jf.asInstanceOf[java.util.function.DoubleConsumer]
      case _ => new AsJavaDoubleConsumer(underlying)
    }
  }
  
  
  case class FromJavaDoubleFunction[R](jf: java.util.function.DoubleFunction[R]) extends scala.Function1[Double, R] {
    def apply(x1: scala.Double) = jf.apply(x1)
  }
  
  class RichDoubleFunctionAsFunction1[R](private val underlying: java.util.function.DoubleFunction[R]) extends AnyVal {
    @inline def asScala: scala.Function1[Double, R] = underlying match {
      case AsJavaDoubleFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[Double, R]]
      case _ => new FromJavaDoubleFunction[R](underlying)
    }
  }
  
  case class AsJavaDoubleFunction[R](sf: scala.Function1[Double, R]) extends java.util.function.DoubleFunction[R] {
    def apply(x1: scala.Double) = sf.apply(x1)
  }
  
  class RichFunction1AsDoubleFunction[R](private val underlying: scala.Function1[Double, R]) extends AnyVal {
    @inline def asJava: java.util.function.DoubleFunction[R] = underlying match {
      case FromJavaDoubleFunction((jf @ _)) => jf.asInstanceOf[java.util.function.DoubleFunction[R]]
      case _ => new AsJavaDoubleFunction[R](underlying)
    };
    @inline def asJavaDoubleFunction: java.util.function.DoubleFunction[R] = underlying match {
      case FromJavaDoubleFunction((sf @ _)) => sf.asInstanceOf[java.util.function.DoubleFunction[R]]
      case _ => new AsJavaDoubleFunction[R](underlying)
    }
  }
  
  
  case class FromJavaDoublePredicate(jf: java.util.function.DoublePredicate) extends scala.Function1[Double, Boolean] {
    def apply(x1: scala.Double) = jf.test(x1)
  }
  
  class RichDoublePredicateAsFunction1(private val underlying: java.util.function.DoublePredicate) extends AnyVal {
    @inline def asScala: scala.Function1[Double, Boolean] = underlying match {
      case AsJavaDoublePredicate((sf @ _)) => sf.asInstanceOf[scala.Function1[Double, Boolean]]
      case _ => new FromJavaDoublePredicate(underlying)
    }
  }
  
  case class AsJavaDoublePredicate(sf: scala.Function1[Double, Boolean]) extends java.util.function.DoublePredicate {
    def test(x1: scala.Double) = sf.apply(x1)
  }
  
  class RichFunction1AsDoublePredicate(private val underlying: scala.Function1[Double, Boolean]) extends AnyVal {
    @inline def asJava: java.util.function.DoublePredicate = underlying match {
      case FromJavaDoublePredicate((jf @ _)) => jf.asInstanceOf[java.util.function.DoublePredicate]
      case _ => new AsJavaDoublePredicate(underlying)
    }
  }
  
  
  case class FromJavaDoubleSupplier(jf: java.util.function.DoubleSupplier) extends scala.Function0[Double] {
    def apply() = jf.getAsDouble()
  }
  
  class RichDoubleSupplierAsFunction0(private val underlying: java.util.function.DoubleSupplier) extends AnyVal {
    @inline def asScala: scala.Function0[Double] = underlying match {
      case AsJavaDoubleSupplier((sf @ _)) => sf.asInstanceOf[scala.Function0[Double]]
      case _ => new FromJavaDoubleSupplier(underlying)
    }
  }
  
  case class AsJavaDoubleSupplier(sf: scala.Function0[Double]) extends java.util.function.DoubleSupplier {
    def getAsDouble() = sf.apply()
  }
  
  class RichFunction0AsDoubleSupplier(private val underlying: scala.Function0[Double]) extends AnyVal {
    @inline def asJava: java.util.function.DoubleSupplier = underlying match {
      case FromJavaDoubleSupplier((jf @ _)) => jf.asInstanceOf[java.util.function.DoubleSupplier]
      case _ => new AsJavaDoubleSupplier(underlying)
    }
  }
  
  
  case class FromJavaDoubleToIntFunction(jf: java.util.function.DoubleToIntFunction) extends scala.Function1[Double, Int] {
    def apply(x1: scala.Double) = jf.applyAsInt(x1)
  }
  
  class RichDoubleToIntFunctionAsFunction1(private val underlying: java.util.function.DoubleToIntFunction) extends AnyVal {
    @inline def asScala: scala.Function1[Double, Int] = underlying match {
      case AsJavaDoubleToIntFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[Double, Int]]
      case _ => new FromJavaDoubleToIntFunction(underlying)
    }
  }
  
  case class AsJavaDoubleToIntFunction(sf: scala.Function1[Double, Int]) extends java.util.function.DoubleToIntFunction {
    def applyAsInt(x1: scala.Double) = sf.apply(x1)
  }
  
  class RichFunction1AsDoubleToIntFunction(private val underlying: scala.Function1[Double, Int]) extends AnyVal {
    @inline def asJava: java.util.function.DoubleToIntFunction = underlying match {
      case FromJavaDoubleToIntFunction((jf @ _)) => jf.asInstanceOf[java.util.function.DoubleToIntFunction]
      case _ => new AsJavaDoubleToIntFunction(underlying)
    }
  }
  
  
  case class FromJavaDoubleToLongFunction(jf: java.util.function.DoubleToLongFunction) extends scala.Function1[Double, Long] {
    def apply(x1: scala.Double) = jf.applyAsLong(x1)
  }
  
  class RichDoubleToLongFunctionAsFunction1(private val underlying: java.util.function.DoubleToLongFunction) extends AnyVal {
    @inline def asScala: scala.Function1[Double, Long] = underlying match {
      case AsJavaDoubleToLongFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[Double, Long]]
      case _ => new FromJavaDoubleToLongFunction(underlying)
    }
  }
  
  case class AsJavaDoubleToLongFunction(sf: scala.Function1[Double, Long]) extends java.util.function.DoubleToLongFunction {
    def applyAsLong(x1: scala.Double) = sf.apply(x1)
  }
  
  class RichFunction1AsDoubleToLongFunction(private val underlying: scala.Function1[Double, Long]) extends AnyVal {
    @inline def asJava: java.util.function.DoubleToLongFunction = underlying match {
      case FromJavaDoubleToLongFunction((jf @ _)) => jf.asInstanceOf[java.util.function.DoubleToLongFunction]
      case _ => new AsJavaDoubleToLongFunction(underlying)
    }
  }
  
  
  case class FromJavaDoubleUnaryOperator(jf: java.util.function.DoubleUnaryOperator) extends scala.Function1[Double, Double] {
    def apply(x1: scala.Double) = jf.applyAsDouble(x1)
  }
  
  class RichDoubleUnaryOperatorAsFunction1(private val underlying: java.util.function.DoubleUnaryOperator) extends AnyVal {
    @inline def asScala: scala.Function1[Double, Double] = underlying match {
      case AsJavaDoubleUnaryOperator((sf @ _)) => sf.asInstanceOf[scala.Function1[Double, Double]]
      case _ => new FromJavaDoubleUnaryOperator(underlying)
    }
  }
  
  case class AsJavaDoubleUnaryOperator(sf: scala.Function1[Double, Double]) extends java.util.function.DoubleUnaryOperator {
    def applyAsDouble(x1: scala.Double) = sf.apply(x1)
  }
  
  class RichFunction1AsDoubleUnaryOperator(private val underlying: scala.Function1[Double, Double]) extends AnyVal {
    @inline def asJava: java.util.function.DoubleUnaryOperator = underlying match {
      case FromJavaDoubleUnaryOperator((jf @ _)) => jf.asInstanceOf[java.util.function.DoubleUnaryOperator]
      case _ => new AsJavaDoubleUnaryOperator(underlying)
    }
  }
  
  
  case class FromJavaFunction[T, R](jf: java.util.function.Function[T, R]) extends scala.Function1[T, R] {
    def apply(x1: T) = jf.apply(x1)
  }
  
  class RichFunctionAsFunction1[T, R](private val underlying: java.util.function.Function[T, R]) extends AnyVal {
    @inline def asScala: scala.Function1[T, R] = underlying match {
      case AsJavaFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[T, R]]
      case _ => new FromJavaFunction[T, R](underlying)
    }
  }
  
  case class AsJavaFunction[T, R](sf: scala.Function1[T, R]) extends java.util.function.Function[T, R] {
    def apply(x1: T) = sf.apply(x1)
  }
  
  class RichFunction1AsFunction[T, R](private val underlying: scala.Function1[T, R]) extends AnyVal {
    @inline def asJava: java.util.function.Function[T, R] = underlying match {
      case FromJavaFunction((jf @ _)) => jf.asInstanceOf[java.util.function.Function[T, R]]
      case _ => new AsJavaFunction[T, R](underlying)
    };
    @inline def asJavaFunction: java.util.function.Function[T, R] = underlying match {
      case FromJavaFunction((sf @ _)) => sf.asInstanceOf[java.util.function.Function[T, R]]
      case _ => new AsJavaFunction[T, R](underlying)
    }
  }
  
  
  case class FromJavaIntBinaryOperator(jf: java.util.function.IntBinaryOperator) extends scala.Function2[Int, Int, Int] {
    def apply(x1: scala.Int, x2: scala.Int) = jf.applyAsInt(x1, x2)
  }
  
  class RichIntBinaryOperatorAsFunction2(private val underlying: java.util.function.IntBinaryOperator) extends AnyVal {
    @inline def asScala: scala.Function2[Int, Int, Int] = underlying match {
      case AsJavaIntBinaryOperator((sf @ _)) => sf.asInstanceOf[scala.Function2[Int, Int, Int]]
      case _ => new FromJavaIntBinaryOperator(underlying)
    }
  }
  
  case class AsJavaIntBinaryOperator(sf: scala.Function2[Int, Int, Int]) extends java.util.function.IntBinaryOperator {
    def applyAsInt(x1: scala.Int, x2: scala.Int) = sf.apply(x1, x2)
  }
  
  class RichFunction2AsIntBinaryOperator(private val underlying: scala.Function2[Int, Int, Int]) extends AnyVal {
    @inline def asJava: java.util.function.IntBinaryOperator = underlying match {
      case FromJavaIntBinaryOperator((jf @ _)) => jf.asInstanceOf[java.util.function.IntBinaryOperator]
      case _ => new AsJavaIntBinaryOperator(underlying)
    }
  }
  
  
  case class FromJavaIntConsumer(jf: java.util.function.IntConsumer) extends scala.Function1[Int, Unit] {
    def apply(x1: scala.Int) = jf.accept(x1)
  }
  
  class RichIntConsumerAsFunction1(private val underlying: java.util.function.IntConsumer) extends AnyVal {
    @inline def asScala: scala.Function1[Int, Unit] = underlying match {
      case AsJavaIntConsumer((sf @ _)) => sf.asInstanceOf[scala.Function1[Int, Unit]]
      case _ => new FromJavaIntConsumer(underlying)
    }
  }
  
  case class AsJavaIntConsumer(sf: scala.Function1[Int, Unit]) extends java.util.function.IntConsumer {
    def accept(x1: scala.Int) = sf.apply(x1)
  }
  
  class RichFunction1AsIntConsumer(private val underlying: scala.Function1[Int, Unit]) extends AnyVal {
    @inline def asJava: java.util.function.IntConsumer = underlying match {
      case FromJavaIntConsumer((jf @ _)) => jf.asInstanceOf[java.util.function.IntConsumer]
      case _ => new AsJavaIntConsumer(underlying)
    }
  }
  
  
  case class FromJavaIntFunction[R](jf: java.util.function.IntFunction[R]) extends scala.Function1[Int, R] {
    def apply(x1: scala.Int) = jf.apply(x1)
  }
  
  class RichIntFunctionAsFunction1[R](private val underlying: java.util.function.IntFunction[R]) extends AnyVal {
    @inline def asScala: scala.Function1[Int, R] = underlying match {
      case AsJavaIntFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[Int, R]]
      case _ => new FromJavaIntFunction[R](underlying)
    }
  }
  
  case class AsJavaIntFunction[R](sf: scala.Function1[Int, R]) extends java.util.function.IntFunction[R] {
    def apply(x1: scala.Int) = sf.apply(x1)
  }
  
  class RichFunction1AsIntFunction[R](private val underlying: scala.Function1[Int, R]) extends AnyVal {
    @inline def asJava: java.util.function.IntFunction[R] = underlying match {
      case FromJavaIntFunction((jf @ _)) => jf.asInstanceOf[java.util.function.IntFunction[R]]
      case _ => new AsJavaIntFunction[R](underlying)
    };
    @inline def asJavaIntFunction: java.util.function.IntFunction[R] = underlying match {
      case FromJavaIntFunction((sf @ _)) => sf.asInstanceOf[java.util.function.IntFunction[R]]
      case _ => new AsJavaIntFunction[R](underlying)
    }
  }
  
  
  case class FromJavaIntPredicate(jf: java.util.function.IntPredicate) extends scala.Function1[Int, Boolean] {
    def apply(x1: scala.Int) = jf.test(x1)
  }
  
  class RichIntPredicateAsFunction1(private val underlying: java.util.function.IntPredicate) extends AnyVal {
    @inline def asScala: scala.Function1[Int, Boolean] = underlying match {
      case AsJavaIntPredicate((sf @ _)) => sf.asInstanceOf[scala.Function1[Int, Boolean]]
      case _ => new FromJavaIntPredicate(underlying)
    }
  }
  
  case class AsJavaIntPredicate(sf: scala.Function1[Int, Boolean]) extends java.util.function.IntPredicate {
    def test(x1: scala.Int) = sf.apply(x1)
  }
  
  class RichFunction1AsIntPredicate(private val underlying: scala.Function1[Int, Boolean]) extends AnyVal {
    @inline def asJava: java.util.function.IntPredicate = underlying match {
      case FromJavaIntPredicate((jf @ _)) => jf.asInstanceOf[java.util.function.IntPredicate]
      case _ => new AsJavaIntPredicate(underlying)
    }
  }
  
  
  case class FromJavaIntSupplier(jf: java.util.function.IntSupplier) extends scala.Function0[Int] {
    def apply() = jf.getAsInt()
  }
  
  class RichIntSupplierAsFunction0(private val underlying: java.util.function.IntSupplier) extends AnyVal {
    @inline def asScala: scala.Function0[Int] = underlying match {
      case AsJavaIntSupplier((sf @ _)) => sf.asInstanceOf[scala.Function0[Int]]
      case _ => new FromJavaIntSupplier(underlying)
    }
  }
  
  case class AsJavaIntSupplier(sf: scala.Function0[Int]) extends java.util.function.IntSupplier {
    def getAsInt() = sf.apply()
  }
  
  class RichFunction0AsIntSupplier(private val underlying: scala.Function0[Int]) extends AnyVal {
    @inline def asJava: java.util.function.IntSupplier = underlying match {
      case FromJavaIntSupplier((jf @ _)) => jf.asInstanceOf[java.util.function.IntSupplier]
      case _ => new AsJavaIntSupplier(underlying)
    }
  }
  
  
  case class FromJavaIntToDoubleFunction(jf: java.util.function.IntToDoubleFunction) extends scala.Function1[Int, Double] {
    def apply(x1: scala.Int) = jf.applyAsDouble(x1)
  }
  
  class RichIntToDoubleFunctionAsFunction1(private val underlying: java.util.function.IntToDoubleFunction) extends AnyVal {
    @inline def asScala: scala.Function1[Int, Double] = underlying match {
      case AsJavaIntToDoubleFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[Int, Double]]
      case _ => new FromJavaIntToDoubleFunction(underlying)
    }
  }
  
  case class AsJavaIntToDoubleFunction(sf: scala.Function1[Int, Double]) extends java.util.function.IntToDoubleFunction {
    def applyAsDouble(x1: scala.Int) = sf.apply(x1)
  }
  
  class RichFunction1AsIntToDoubleFunction(private val underlying: scala.Function1[Int, Double]) extends AnyVal {
    @inline def asJava: java.util.function.IntToDoubleFunction = underlying match {
      case FromJavaIntToDoubleFunction((jf @ _)) => jf.asInstanceOf[java.util.function.IntToDoubleFunction]
      case _ => new AsJavaIntToDoubleFunction(underlying)
    }
  }
  
  
  case class FromJavaIntToLongFunction(jf: java.util.function.IntToLongFunction) extends scala.Function1[Int, Long] {
    def apply(x1: scala.Int) = jf.applyAsLong(x1)
  }
  
  class RichIntToLongFunctionAsFunction1(private val underlying: java.util.function.IntToLongFunction) extends AnyVal {
    @inline def asScala: scala.Function1[Int, Long] = underlying match {
      case AsJavaIntToLongFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[Int, Long]]
      case _ => new FromJavaIntToLongFunction(underlying)
    }
  }
  
  case class AsJavaIntToLongFunction(sf: scala.Function1[Int, Long]) extends java.util.function.IntToLongFunction {
    def applyAsLong(x1: scala.Int) = sf.apply(x1)
  }
  
  class RichFunction1AsIntToLongFunction(private val underlying: scala.Function1[Int, Long]) extends AnyVal {
    @inline def asJava: java.util.function.IntToLongFunction = underlying match {
      case FromJavaIntToLongFunction((jf @ _)) => jf.asInstanceOf[java.util.function.IntToLongFunction]
      case _ => new AsJavaIntToLongFunction(underlying)
    }
  }
  
  
  case class FromJavaIntUnaryOperator(jf: java.util.function.IntUnaryOperator) extends scala.Function1[Int, Int] {
    def apply(x1: scala.Int) = jf.applyAsInt(x1)
  }
  
  class RichIntUnaryOperatorAsFunction1(private val underlying: java.util.function.IntUnaryOperator) extends AnyVal {
    @inline def asScala: scala.Function1[Int, Int] = underlying match {
      case AsJavaIntUnaryOperator((sf @ _)) => sf.asInstanceOf[scala.Function1[Int, Int]]
      case _ => new FromJavaIntUnaryOperator(underlying)
    }
  }
  
  case class AsJavaIntUnaryOperator(sf: scala.Function1[Int, Int]) extends java.util.function.IntUnaryOperator {
    def applyAsInt(x1: scala.Int) = sf.apply(x1)
  }
  
  class RichFunction1AsIntUnaryOperator(private val underlying: scala.Function1[Int, Int]) extends AnyVal {
    @inline def asJava: java.util.function.IntUnaryOperator = underlying match {
      case FromJavaIntUnaryOperator((jf @ _)) => jf.asInstanceOf[java.util.function.IntUnaryOperator]
      case _ => new AsJavaIntUnaryOperator(underlying)
    }
  }
  
  
  case class FromJavaLongBinaryOperator(jf: java.util.function.LongBinaryOperator) extends scala.Function2[Long, Long, Long] {
    def apply(x1: scala.Long, x2: scala.Long) = jf.applyAsLong(x1, x2)
  }
  
  class RichLongBinaryOperatorAsFunction2(private val underlying: java.util.function.LongBinaryOperator) extends AnyVal {
    @inline def asScala: scala.Function2[Long, Long, Long] = underlying match {
      case AsJavaLongBinaryOperator((sf @ _)) => sf.asInstanceOf[scala.Function2[Long, Long, Long]]
      case _ => new FromJavaLongBinaryOperator(underlying)
    }
  }
  
  case class AsJavaLongBinaryOperator(sf: scala.Function2[Long, Long, Long]) extends java.util.function.LongBinaryOperator {
    def applyAsLong(x1: scala.Long, x2: scala.Long) = sf.apply(x1, x2)
  }
  
  class RichFunction2AsLongBinaryOperator(private val underlying: scala.Function2[Long, Long, Long]) extends AnyVal {
    @inline def asJava: java.util.function.LongBinaryOperator = underlying match {
      case FromJavaLongBinaryOperator((jf @ _)) => jf.asInstanceOf[java.util.function.LongBinaryOperator]
      case _ => new AsJavaLongBinaryOperator(underlying)
    }
  }
  
  
  case class FromJavaLongConsumer(jf: java.util.function.LongConsumer) extends scala.Function1[Long, Unit] {
    def apply(x1: scala.Long) = jf.accept(x1)
  }
  
  class RichLongConsumerAsFunction1(private val underlying: java.util.function.LongConsumer) extends AnyVal {
    @inline def asScala: scala.Function1[Long, Unit] = underlying match {
      case AsJavaLongConsumer((sf @ _)) => sf.asInstanceOf[scala.Function1[Long, Unit]]
      case _ => new FromJavaLongConsumer(underlying)
    }
  }
  
  case class AsJavaLongConsumer(sf: scala.Function1[Long, Unit]) extends java.util.function.LongConsumer {
    def accept(x1: scala.Long) = sf.apply(x1)
  }
  
  class RichFunction1AsLongConsumer(private val underlying: scala.Function1[Long, Unit]) extends AnyVal {
    @inline def asJava: java.util.function.LongConsumer = underlying match {
      case FromJavaLongConsumer((jf @ _)) => jf.asInstanceOf[java.util.function.LongConsumer]
      case _ => new AsJavaLongConsumer(underlying)
    }
  }
  
  
  case class FromJavaLongFunction[R](jf: java.util.function.LongFunction[R]) extends scala.Function1[Long, R] {
    def apply(x1: scala.Long) = jf.apply(x1)
  }
  
  class RichLongFunctionAsFunction1[R](private val underlying: java.util.function.LongFunction[R]) extends AnyVal {
    @inline def asScala: scala.Function1[Long, R] = underlying match {
      case AsJavaLongFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[Long, R]]
      case _ => new FromJavaLongFunction[R](underlying)
    }
  }
  
  case class AsJavaLongFunction[R](sf: scala.Function1[Long, R]) extends java.util.function.LongFunction[R] {
    def apply(x1: scala.Long) = sf.apply(x1)
  }
  
  class RichFunction1AsLongFunction[R](private val underlying: scala.Function1[Long, R]) extends AnyVal {
    @inline def asJava: java.util.function.LongFunction[R] = underlying match {
      case FromJavaLongFunction((jf @ _)) => jf.asInstanceOf[java.util.function.LongFunction[R]]
      case _ => new AsJavaLongFunction[R](underlying)
    };
    @inline def asJavaLongFunction: java.util.function.LongFunction[R] = underlying match {
      case FromJavaLongFunction((sf @ _)) => sf.asInstanceOf[java.util.function.LongFunction[R]]
      case _ => new AsJavaLongFunction[R](underlying)
    }
  }
  
  
  case class FromJavaLongPredicate(jf: java.util.function.LongPredicate) extends scala.Function1[Long, Boolean] {
    def apply(x1: scala.Long) = jf.test(x1)
  }
  
  class RichLongPredicateAsFunction1(private val underlying: java.util.function.LongPredicate) extends AnyVal {
    @inline def asScala: scala.Function1[Long, Boolean] = underlying match {
      case AsJavaLongPredicate((sf @ _)) => sf.asInstanceOf[scala.Function1[Long, Boolean]]
      case _ => new FromJavaLongPredicate(underlying)
    }
  }
  
  case class AsJavaLongPredicate(sf: scala.Function1[Long, Boolean]) extends java.util.function.LongPredicate {
    def test(x1: scala.Long) = sf.apply(x1)
  }
  
  class RichFunction1AsLongPredicate(private val underlying: scala.Function1[Long, Boolean]) extends AnyVal {
    @inline def asJava: java.util.function.LongPredicate = underlying match {
      case FromJavaLongPredicate((jf @ _)) => jf.asInstanceOf[java.util.function.LongPredicate]
      case _ => new AsJavaLongPredicate(underlying)
    }
  }
  
  
  case class FromJavaLongSupplier(jf: java.util.function.LongSupplier) extends scala.Function0[Long] {
    def apply() = jf.getAsLong()
  }
  
  class RichLongSupplierAsFunction0(private val underlying: java.util.function.LongSupplier) extends AnyVal {
    @inline def asScala: scala.Function0[Long] = underlying match {
      case AsJavaLongSupplier((sf @ _)) => sf.asInstanceOf[scala.Function0[Long]]
      case _ => new FromJavaLongSupplier(underlying)
    }
  }
  
  case class AsJavaLongSupplier(sf: scala.Function0[Long]) extends java.util.function.LongSupplier {
    def getAsLong() = sf.apply()
  }
  
  class RichFunction0AsLongSupplier(private val underlying: scala.Function0[Long]) extends AnyVal {
    @inline def asJava: java.util.function.LongSupplier = underlying match {
      case FromJavaLongSupplier((jf @ _)) => jf.asInstanceOf[java.util.function.LongSupplier]
      case _ => new AsJavaLongSupplier(underlying)
    }
  }
  
  
  case class FromJavaLongToDoubleFunction(jf: java.util.function.LongToDoubleFunction) extends scala.Function1[Long, Double] {
    def apply(x1: scala.Long) = jf.applyAsDouble(x1)
  }
  
  class RichLongToDoubleFunctionAsFunction1(private val underlying: java.util.function.LongToDoubleFunction) extends AnyVal {
    @inline def asScala: scala.Function1[Long, Double] = underlying match {
      case AsJavaLongToDoubleFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[Long, Double]]
      case _ => new FromJavaLongToDoubleFunction(underlying)
    }
  }
  
  case class AsJavaLongToDoubleFunction(sf: scala.Function1[Long, Double]) extends java.util.function.LongToDoubleFunction {
    def applyAsDouble(x1: scala.Long) = sf.apply(x1)
  }
  
  class RichFunction1AsLongToDoubleFunction(private val underlying: scala.Function1[Long, Double]) extends AnyVal {
    @inline def asJava: java.util.function.LongToDoubleFunction = underlying match {
      case FromJavaLongToDoubleFunction((jf @ _)) => jf.asInstanceOf[java.util.function.LongToDoubleFunction]
      case _ => new AsJavaLongToDoubleFunction(underlying)
    }
  }
  
  
  case class FromJavaLongToIntFunction(jf: java.util.function.LongToIntFunction) extends scala.Function1[Long, Int] {
    def apply(x1: scala.Long) = jf.applyAsInt(x1)
  }
  
  class RichLongToIntFunctionAsFunction1(private val underlying: java.util.function.LongToIntFunction) extends AnyVal {
    @inline def asScala: scala.Function1[Long, Int] = underlying match {
      case AsJavaLongToIntFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[Long, Int]]
      case _ => new FromJavaLongToIntFunction(underlying)
    }
  }
  
  case class AsJavaLongToIntFunction(sf: scala.Function1[Long, Int]) extends java.util.function.LongToIntFunction {
    def applyAsInt(x1: scala.Long) = sf.apply(x1)
  }
  
  class RichFunction1AsLongToIntFunction(private val underlying: scala.Function1[Long, Int]) extends AnyVal {
    @inline def asJava: java.util.function.LongToIntFunction = underlying match {
      case FromJavaLongToIntFunction((jf @ _)) => jf.asInstanceOf[java.util.function.LongToIntFunction]
      case _ => new AsJavaLongToIntFunction(underlying)
    }
  }
  
  
  case class FromJavaLongUnaryOperator(jf: java.util.function.LongUnaryOperator) extends scala.Function1[Long, Long] {
    def apply(x1: scala.Long) = jf.applyAsLong(x1)
  }
  
  class RichLongUnaryOperatorAsFunction1(private val underlying: java.util.function.LongUnaryOperator) extends AnyVal {
    @inline def asScala: scala.Function1[Long, Long] = underlying match {
      case AsJavaLongUnaryOperator((sf @ _)) => sf.asInstanceOf[scala.Function1[Long, Long]]
      case _ => new FromJavaLongUnaryOperator(underlying)
    }
  }
  
  case class AsJavaLongUnaryOperator(sf: scala.Function1[Long, Long]) extends java.util.function.LongUnaryOperator {
    def applyAsLong(x1: scala.Long) = sf.apply(x1)
  }
  
  class RichFunction1AsLongUnaryOperator(private val underlying: scala.Function1[Long, Long]) extends AnyVal {
    @inline def asJava: java.util.function.LongUnaryOperator = underlying match {
      case FromJavaLongUnaryOperator((jf @ _)) => jf.asInstanceOf[java.util.function.LongUnaryOperator]
      case _ => new AsJavaLongUnaryOperator(underlying)
    }
  }
  
  
  case class FromJavaObjDoubleConsumer[T](jf: java.util.function.ObjDoubleConsumer[T]) extends scala.Function2[T, Double, Unit] {
    def apply(x1: T, x2: scala.Double) = jf.accept(x1, x2)
  }
  
  class RichObjDoubleConsumerAsFunction2[T](private val underlying: java.util.function.ObjDoubleConsumer[T]) extends AnyVal {
    @inline def asScala: scala.Function2[T, Double, Unit] = underlying match {
      case AsJavaObjDoubleConsumer((sf @ _)) => sf.asInstanceOf[scala.Function2[T, Double, Unit]]
      case _ => new FromJavaObjDoubleConsumer[T](underlying)
    }
  }
  
  case class AsJavaObjDoubleConsumer[T](sf: scala.Function2[T, Double, Unit]) extends java.util.function.ObjDoubleConsumer[T] {
    def accept(x1: T, x2: scala.Double) = sf.apply(x1, x2)
  }
  
  class RichFunction2AsObjDoubleConsumer[T](private val underlying: scala.Function2[T, Double, Unit]) extends AnyVal {
    @inline def asJava: java.util.function.ObjDoubleConsumer[T] = underlying match {
      case FromJavaObjDoubleConsumer((jf @ _)) => jf.asInstanceOf[java.util.function.ObjDoubleConsumer[T]]
      case _ => new AsJavaObjDoubleConsumer[T](underlying)
    };
    @inline def asJavaObjDoubleConsumer: java.util.function.ObjDoubleConsumer[T] = underlying match {
      case FromJavaObjDoubleConsumer((sf @ _)) => sf.asInstanceOf[java.util.function.ObjDoubleConsumer[T]]
      case _ => new AsJavaObjDoubleConsumer[T](underlying)
    }
  }
  
  
  case class FromJavaObjIntConsumer[T](jf: java.util.function.ObjIntConsumer[T]) extends scala.Function2[T, Int, Unit] {
    def apply(x1: T, x2: scala.Int) = jf.accept(x1, x2)
  }
  
  class RichObjIntConsumerAsFunction2[T](private val underlying: java.util.function.ObjIntConsumer[T]) extends AnyVal {
    @inline def asScala: scala.Function2[T, Int, Unit] = underlying match {
      case AsJavaObjIntConsumer((sf @ _)) => sf.asInstanceOf[scala.Function2[T, Int, Unit]]
      case _ => new FromJavaObjIntConsumer[T](underlying)
    }
  }
  
  case class AsJavaObjIntConsumer[T](sf: scala.Function2[T, Int, Unit]) extends java.util.function.ObjIntConsumer[T] {
    def accept(x1: T, x2: scala.Int) = sf.apply(x1, x2)
  }
  
  class RichFunction2AsObjIntConsumer[T](private val underlying: scala.Function2[T, Int, Unit]) extends AnyVal {
    @inline def asJava: java.util.function.ObjIntConsumer[T] = underlying match {
      case FromJavaObjIntConsumer((jf @ _)) => jf.asInstanceOf[java.util.function.ObjIntConsumer[T]]
      case _ => new AsJavaObjIntConsumer[T](underlying)
    };
    @inline def asJavaObjIntConsumer: java.util.function.ObjIntConsumer[T] = underlying match {
      case FromJavaObjIntConsumer((sf @ _)) => sf.asInstanceOf[java.util.function.ObjIntConsumer[T]]
      case _ => new AsJavaObjIntConsumer[T](underlying)
    }
  }
  
  
  case class FromJavaObjLongConsumer[T](jf: java.util.function.ObjLongConsumer[T]) extends scala.Function2[T, Long, Unit] {
    def apply(x1: T, x2: scala.Long) = jf.accept(x1, x2)
  }
  
  class RichObjLongConsumerAsFunction2[T](private val underlying: java.util.function.ObjLongConsumer[T]) extends AnyVal {
    @inline def asScala: scala.Function2[T, Long, Unit] = underlying match {
      case AsJavaObjLongConsumer((sf @ _)) => sf.asInstanceOf[scala.Function2[T, Long, Unit]]
      case _ => new FromJavaObjLongConsumer[T](underlying)
    }
  }
  
  case class AsJavaObjLongConsumer[T](sf: scala.Function2[T, Long, Unit]) extends java.util.function.ObjLongConsumer[T] {
    def accept(x1: T, x2: scala.Long) = sf.apply(x1, x2)
  }
  
  class RichFunction2AsObjLongConsumer[T](private val underlying: scala.Function2[T, Long, Unit]) extends AnyVal {
    @inline def asJava: java.util.function.ObjLongConsumer[T] = underlying match {
      case FromJavaObjLongConsumer((jf @ _)) => jf.asInstanceOf[java.util.function.ObjLongConsumer[T]]
      case _ => new AsJavaObjLongConsumer[T](underlying)
    };
    @inline def asJavaObjLongConsumer: java.util.function.ObjLongConsumer[T] = underlying match {
      case FromJavaObjLongConsumer((sf @ _)) => sf.asInstanceOf[java.util.function.ObjLongConsumer[T]]
      case _ => new AsJavaObjLongConsumer[T](underlying)
    }
  }
  
  
  case class FromJavaPredicate[T](jf: java.util.function.Predicate[T]) extends scala.Function1[T, Boolean] {
    def apply(x1: T) = jf.test(x1)
  }
  
  class RichPredicateAsFunction1[T](private val underlying: java.util.function.Predicate[T]) extends AnyVal {
    @inline def asScala: scala.Function1[T, Boolean] = underlying match {
      case AsJavaPredicate((sf @ _)) => sf.asInstanceOf[scala.Function1[T, Boolean]]
      case _ => new FromJavaPredicate[T](underlying)
    }
  }
  
  case class AsJavaPredicate[T](sf: scala.Function1[T, Boolean]) extends java.util.function.Predicate[T] {
    def test(x1: T) = sf.apply(x1)
  }
  
  class RichFunction1AsPredicate[T](private val underlying: scala.Function1[T, Boolean]) extends AnyVal {
    @inline def asJava: java.util.function.Predicate[T] = underlying match {
      case FromJavaPredicate((jf @ _)) => jf.asInstanceOf[java.util.function.Predicate[T]]
      case _ => new AsJavaPredicate[T](underlying)
    };
    @inline def asJavaPredicate: java.util.function.Predicate[T] = underlying match {
      case FromJavaPredicate((sf @ _)) => sf.asInstanceOf[java.util.function.Predicate[T]]
      case _ => new AsJavaPredicate[T](underlying)
    }
  }
  
  
  case class FromJavaSupplier[T](jf: java.util.function.Supplier[T]) extends scala.Function0[T] {
    def apply() = jf.get()
  }
  
  class RichSupplierAsFunction0[T](private val underlying: java.util.function.Supplier[T]) extends AnyVal {
    @inline def asScala: scala.Function0[T] = underlying match {
      case AsJavaSupplier((sf @ _)) => sf.asInstanceOf[scala.Function0[T]]
      case _ => new FromJavaSupplier[T](underlying)
    }
  }
  
  case class AsJavaSupplier[T](sf: scala.Function0[T]) extends java.util.function.Supplier[T] {
    def get() = sf.apply()
  }
  
  class RichFunction0AsSupplier[T](private val underlying: scala.Function0[T]) extends AnyVal {
    @inline def asJava: java.util.function.Supplier[T] = underlying match {
      case FromJavaSupplier((jf @ _)) => jf.asInstanceOf[java.util.function.Supplier[T]]
      case _ => new AsJavaSupplier[T](underlying)
    };
    @inline def asJavaSupplier: java.util.function.Supplier[T] = underlying match {
      case FromJavaSupplier((sf @ _)) => sf.asInstanceOf[java.util.function.Supplier[T]]
      case _ => new AsJavaSupplier[T](underlying)
    }
  }
  
  
  case class FromJavaToDoubleBiFunction[T, U](jf: java.util.function.ToDoubleBiFunction[T, U]) extends scala.Function2[T, U, Double] {
    def apply(x1: T, x2: U) = jf.applyAsDouble(x1, x2)
  }
  
  class RichToDoubleBiFunctionAsFunction2[T, U](private val underlying: java.util.function.ToDoubleBiFunction[T, U]) extends AnyVal {
    @inline def asScala: scala.Function2[T, U, Double] = underlying match {
      case AsJavaToDoubleBiFunction((sf @ _)) => sf.asInstanceOf[scala.Function2[T, U, Double]]
      case _ => new FromJavaToDoubleBiFunction[T, U](underlying)
    }
  }
  
  case class AsJavaToDoubleBiFunction[T, U](sf: scala.Function2[T, U, Double]) extends java.util.function.ToDoubleBiFunction[T, U] {
    def applyAsDouble(x1: T, x2: U) = sf.apply(x1, x2)
  }
  
  class RichFunction2AsToDoubleBiFunction[T, U](private val underlying: scala.Function2[T, U, Double]) extends AnyVal {
    @inline def asJava: java.util.function.ToDoubleBiFunction[T, U] = underlying match {
      case FromJavaToDoubleBiFunction((jf @ _)) => jf.asInstanceOf[java.util.function.ToDoubleBiFunction[T, U]]
      case _ => new AsJavaToDoubleBiFunction[T, U](underlying)
    };
    @inline def asJavaToDoubleBiFunction: java.util.function.ToDoubleBiFunction[T, U] = underlying match {
      case FromJavaToDoubleBiFunction((sf @ _)) => sf.asInstanceOf[java.util.function.ToDoubleBiFunction[T, U]]
      case _ => new AsJavaToDoubleBiFunction[T, U](underlying)
    }
  }
  
  
  case class FromJavaToDoubleFunction[T](jf: java.util.function.ToDoubleFunction[T]) extends scala.Function1[T, Double] {
    def apply(x1: T) = jf.applyAsDouble(x1)
  }
  
  class RichToDoubleFunctionAsFunction1[T](private val underlying: java.util.function.ToDoubleFunction[T]) extends AnyVal {
    @inline def asScala: scala.Function1[T, Double] = underlying match {
      case AsJavaToDoubleFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[T, Double]]
      case _ => new FromJavaToDoubleFunction[T](underlying)
    }
  }
  
  case class AsJavaToDoubleFunction[T](sf: scala.Function1[T, Double]) extends java.util.function.ToDoubleFunction[T] {
    def applyAsDouble(x1: T) = sf.apply(x1)
  }
  
  class RichFunction1AsToDoubleFunction[T](private val underlying: scala.Function1[T, Double]) extends AnyVal {
    @inline def asJava: java.util.function.ToDoubleFunction[T] = underlying match {
      case FromJavaToDoubleFunction((jf @ _)) => jf.asInstanceOf[java.util.function.ToDoubleFunction[T]]
      case _ => new AsJavaToDoubleFunction[T](underlying)
    };
    @inline def asJavaToDoubleFunction: java.util.function.ToDoubleFunction[T] = underlying match {
      case FromJavaToDoubleFunction((sf @ _)) => sf.asInstanceOf[java.util.function.ToDoubleFunction[T]]
      case _ => new AsJavaToDoubleFunction[T](underlying)
    }
  }
  
  
  case class FromJavaToIntBiFunction[T, U](jf: java.util.function.ToIntBiFunction[T, U]) extends scala.Function2[T, U, Int] {
    def apply(x1: T, x2: U) = jf.applyAsInt(x1, x2)
  }
  
  class RichToIntBiFunctionAsFunction2[T, U](private val underlying: java.util.function.ToIntBiFunction[T, U]) extends AnyVal {
    @inline def asScala: scala.Function2[T, U, Int] = underlying match {
      case AsJavaToIntBiFunction((sf @ _)) => sf.asInstanceOf[scala.Function2[T, U, Int]]
      case _ => new FromJavaToIntBiFunction[T, U](underlying)
    }
  }
  
  case class AsJavaToIntBiFunction[T, U](sf: scala.Function2[T, U, Int]) extends java.util.function.ToIntBiFunction[T, U] {
    def applyAsInt(x1: T, x2: U) = sf.apply(x1, x2)
  }
  
  class RichFunction2AsToIntBiFunction[T, U](private val underlying: scala.Function2[T, U, Int]) extends AnyVal {
    @inline def asJava: java.util.function.ToIntBiFunction[T, U] = underlying match {
      case FromJavaToIntBiFunction((jf @ _)) => jf.asInstanceOf[java.util.function.ToIntBiFunction[T, U]]
      case _ => new AsJavaToIntBiFunction[T, U](underlying)
    };
    @inline def asJavaToIntBiFunction: java.util.function.ToIntBiFunction[T, U] = underlying match {
      case FromJavaToIntBiFunction((sf @ _)) => sf.asInstanceOf[java.util.function.ToIntBiFunction[T, U]]
      case _ => new AsJavaToIntBiFunction[T, U](underlying)
    }
  }
  
  
  case class FromJavaToIntFunction[T](jf: java.util.function.ToIntFunction[T]) extends scala.Function1[T, Int] {
    def apply(x1: T) = jf.applyAsInt(x1)
  }
  
  class RichToIntFunctionAsFunction1[T](private val underlying: java.util.function.ToIntFunction[T]) extends AnyVal {
    @inline def asScala: scala.Function1[T, Int] = underlying match {
      case AsJavaToIntFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[T, Int]]
      case _ => new FromJavaToIntFunction[T](underlying)
    }
  }
  
  case class AsJavaToIntFunction[T](sf: scala.Function1[T, Int]) extends java.util.function.ToIntFunction[T] {
    def applyAsInt(x1: T) = sf.apply(x1)
  }
  
  class RichFunction1AsToIntFunction[T](private val underlying: scala.Function1[T, Int]) extends AnyVal {
    @inline def asJava: java.util.function.ToIntFunction[T] = underlying match {
      case FromJavaToIntFunction((jf @ _)) => jf.asInstanceOf[java.util.function.ToIntFunction[T]]
      case _ => new AsJavaToIntFunction[T](underlying)
    };
    @inline def asJavaToIntFunction: java.util.function.ToIntFunction[T] = underlying match {
      case FromJavaToIntFunction((sf @ _)) => sf.asInstanceOf[java.util.function.ToIntFunction[T]]
      case _ => new AsJavaToIntFunction[T](underlying)
    }
  }
  
  
  case class FromJavaToLongBiFunction[T, U](jf: java.util.function.ToLongBiFunction[T, U]) extends scala.Function2[T, U, Long] {
    def apply(x1: T, x2: U) = jf.applyAsLong(x1, x2)
  }
  
  class RichToLongBiFunctionAsFunction2[T, U](private val underlying: java.util.function.ToLongBiFunction[T, U]) extends AnyVal {
    @inline def asScala: scala.Function2[T, U, Long] = underlying match {
      case AsJavaToLongBiFunction((sf @ _)) => sf.asInstanceOf[scala.Function2[T, U, Long]]
      case _ => new FromJavaToLongBiFunction[T, U](underlying)
    }
  }
  
  case class AsJavaToLongBiFunction[T, U](sf: scala.Function2[T, U, Long]) extends java.util.function.ToLongBiFunction[T, U] {
    def applyAsLong(x1: T, x2: U) = sf.apply(x1, x2)
  }
  
  class RichFunction2AsToLongBiFunction[T, U](private val underlying: scala.Function2[T, U, Long]) extends AnyVal {
    @inline def asJava: java.util.function.ToLongBiFunction[T, U] = underlying match {
      case FromJavaToLongBiFunction((jf @ _)) => jf.asInstanceOf[java.util.function.ToLongBiFunction[T, U]]
      case _ => new AsJavaToLongBiFunction[T, U](underlying)
    };
    @inline def asJavaToLongBiFunction: java.util.function.ToLongBiFunction[T, U] = underlying match {
      case FromJavaToLongBiFunction((sf @ _)) => sf.asInstanceOf[java.util.function.ToLongBiFunction[T, U]]
      case _ => new AsJavaToLongBiFunction[T, U](underlying)
    }
  }
  
  
  case class FromJavaToLongFunction[T](jf: java.util.function.ToLongFunction[T]) extends scala.Function1[T, Long] {
    def apply(x1: T) = jf.applyAsLong(x1)
  }
  
  class RichToLongFunctionAsFunction1[T](private val underlying: java.util.function.ToLongFunction[T]) extends AnyVal {
    @inline def asScala: scala.Function1[T, Long] = underlying match {
      case AsJavaToLongFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[T, Long]]
      case _ => new FromJavaToLongFunction[T](underlying)
    }
  }
  
  case class AsJavaToLongFunction[T](sf: scala.Function1[T, Long]) extends java.util.function.ToLongFunction[T] {
    def applyAsLong(x1: T) = sf.apply(x1)
  }
  
  class RichFunction1AsToLongFunction[T](private val underlying: scala.Function1[T, Long]) extends AnyVal {
    @inline def asJava: java.util.function.ToLongFunction[T] = underlying match {
      case FromJavaToLongFunction((jf @ _)) => jf.asInstanceOf[java.util.function.ToLongFunction[T]]
      case _ => new AsJavaToLongFunction[T](underlying)
    };
    @inline def asJavaToLongFunction: java.util.function.ToLongFunction[T] = underlying match {
      case FromJavaToLongFunction((sf @ _)) => sf.asInstanceOf[java.util.function.ToLongFunction[T]]
      case _ => new AsJavaToLongFunction[T](underlying)
    }
  }
  
  
  case class FromJavaUnaryOperator[T](jf: java.util.function.UnaryOperator[T]) extends scala.Function1[T, T] {
    def apply(x1: T) = jf.apply(x1)
  }
  
  class RichUnaryOperatorAsFunction1[T](private val underlying: java.util.function.UnaryOperator[T]) extends AnyVal {
    @inline def asScala: scala.Function1[T, T] = underlying match {
      case AsJavaUnaryOperator((sf @ _)) => sf.asInstanceOf[scala.Function1[T, T]]
      case _ => new FromJavaUnaryOperator[T](underlying)
    }
  }
  
  case class AsJavaUnaryOperator[T](sf: scala.Function1[T, T]) extends java.util.function.UnaryOperator[T] {
    def apply(x1: T) = sf.apply(x1)
  }
  
  class RichFunction1AsUnaryOperator[T](private val underlying: scala.Function1[T, T]) extends AnyVal {
    @inline def asJava: java.util.function.UnaryOperator[T] = underlying match {
      case FromJavaUnaryOperator((jf @ _)) => jf.asInstanceOf[java.util.function.UnaryOperator[T]]
      case _ => new AsJavaUnaryOperator[T](underlying)
    };
    @inline def asJavaUnaryOperator: java.util.function.UnaryOperator[T] = underlying match {
      case FromJavaUnaryOperator((sf @ _)) => sf.asInstanceOf[java.util.function.UnaryOperator[T]]
      case _ => new AsJavaUnaryOperator[T](underlying)
    }
  }
}

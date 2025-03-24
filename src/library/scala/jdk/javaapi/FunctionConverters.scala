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


package scala.jdk.javaapi

/** This object contains methods that convert between Scala and Java function types.
  *
  * The explicit conversion methods defined here are intended to be used in Java code. For Scala
  * code, it is recommended to use the extension methods defined in [[scala.jdk.FunctionConverters]].
  *
  * For details how the function converters work, see [[scala.jdk.FunctionConverters]].
  *
  */
object FunctionConverters {
  import scala.jdk.FunctionWrappers._

  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromBiConsumer[T, U](jf: java.util.function.BiConsumer[T, U]): scala.Function2[T, U, scala.runtime.BoxedUnit] = jf match {
    case AsJavaBiConsumer((f @ _)) => f.asInstanceOf[scala.Function2[T, U, scala.runtime.BoxedUnit]]
    case _ => new FromJavaBiConsumer[T, U](jf).asInstanceOf[scala.Function2[T, U, scala.runtime.BoxedUnit]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaBiConsumer[T, U](sf: scala.Function2[T, U, scala.runtime.BoxedUnit]): java.util.function.BiConsumer[T, U] = ((sf): AnyRef) match {
    case FromJavaBiConsumer((f @ _)) => f.asInstanceOf[java.util.function.BiConsumer[T, U]]
    case _ => new AsJavaBiConsumer[T, U](sf.asInstanceOf[scala.Function2[T, U, Unit]])
  }
  
  
  @inline def asScalaFromBiFunction[T, U, R](jf: java.util.function.BiFunction[T, U, R]): scala.Function2[T, U, R] = jf match {
    case AsJavaBiFunction((f @ _)) => f.asInstanceOf[scala.Function2[T, U, R]]
    case _ => new FromJavaBiFunction[T, U, R](jf).asInstanceOf[scala.Function2[T, U, R]]
  }
  
  @inline def asJavaBiFunction[T, U, R](sf: scala.Function2[T, U, R]): java.util.function.BiFunction[T, U, R] = ((sf): AnyRef) match {
    case FromJavaBiFunction((f @ _)) => f.asInstanceOf[java.util.function.BiFunction[T, U, R]]
    case _ => new AsJavaBiFunction[T, U, R](sf.asInstanceOf[scala.Function2[T, U, R]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromBiPredicate[T, U](jf: java.util.function.BiPredicate[T, U]): scala.Function2[T, U, java.lang.Boolean] = jf match {
    case AsJavaBiPredicate((f @ _)) => f.asInstanceOf[scala.Function2[T, U, java.lang.Boolean]]
    case _ => new FromJavaBiPredicate[T, U](jf).asInstanceOf[scala.Function2[T, U, java.lang.Boolean]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaBiPredicate[T, U](sf: scala.Function2[T, U, java.lang.Boolean]): java.util.function.BiPredicate[T, U] = ((sf): AnyRef) match {
    case FromJavaBiPredicate((f @ _)) => f.asInstanceOf[java.util.function.BiPredicate[T, U]]
    case _ => new AsJavaBiPredicate[T, U](sf.asInstanceOf[scala.Function2[T, U, Boolean]])
  }
  
  
  @inline def asScalaFromBinaryOperator[T](jf: java.util.function.BinaryOperator[T]): scala.Function2[T, T, T] = jf match {
    case AsJavaBinaryOperator((f @ _)) => f.asInstanceOf[scala.Function2[T, T, T]]
    case _ => new FromJavaBinaryOperator[T](jf).asInstanceOf[scala.Function2[T, T, T]]
  }
  
  @inline def asJavaBinaryOperator[T](sf: scala.Function2[T, T, T]): java.util.function.BinaryOperator[T] = ((sf): AnyRef) match {
    case FromJavaBinaryOperator((f @ _)) => f.asInstanceOf[java.util.function.BinaryOperator[T]]
    case _ => new AsJavaBinaryOperator[T](sf.asInstanceOf[scala.Function2[T, T, T]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromBooleanSupplier(jf: java.util.function.BooleanSupplier): scala.Function0[java.lang.Boolean] = jf match {
    case AsJavaBooleanSupplier((f @ _)) => f.asInstanceOf[scala.Function0[java.lang.Boolean]]
    case _ => new FromJavaBooleanSupplier(jf).asInstanceOf[scala.Function0[java.lang.Boolean]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaBooleanSupplier(sf: scala.Function0[java.lang.Boolean]): java.util.function.BooleanSupplier = ((sf): AnyRef) match {
    case FromJavaBooleanSupplier((f @ _)) => f.asInstanceOf[java.util.function.BooleanSupplier]
    case _ => new AsJavaBooleanSupplier(sf.asInstanceOf[scala.Function0[Boolean]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromConsumer[T](jf: java.util.function.Consumer[T]): scala.Function1[T, scala.runtime.BoxedUnit] = jf match {
    case AsJavaConsumer((f @ _)) => f.asInstanceOf[scala.Function1[T, scala.runtime.BoxedUnit]]
    case _ => new FromJavaConsumer[T](jf).asInstanceOf[scala.Function1[T, scala.runtime.BoxedUnit]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaConsumer[T](sf: scala.Function1[T, scala.runtime.BoxedUnit]): java.util.function.Consumer[T] = ((sf): AnyRef) match {
    case FromJavaConsumer((f @ _)) => f.asInstanceOf[java.util.function.Consumer[T]]
    case _ => new AsJavaConsumer[T](sf.asInstanceOf[scala.Function1[T, Unit]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromDoubleBinaryOperator(jf: java.util.function.DoubleBinaryOperator): scala.Function2[java.lang.Double, java.lang.Double, java.lang.Double] = jf match {
    case AsJavaDoubleBinaryOperator((f @ _)) => f.asInstanceOf[scala.Function2[java.lang.Double, java.lang.Double, java.lang.Double]]
    case _ => new FromJavaDoubleBinaryOperator(jf).asInstanceOf[scala.Function2[java.lang.Double, java.lang.Double, java.lang.Double]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaDoubleBinaryOperator(sf: scala.Function2[java.lang.Double, java.lang.Double, java.lang.Double]): java.util.function.DoubleBinaryOperator = ((sf): AnyRef) match {
    case FromJavaDoubleBinaryOperator((f @ _)) => f.asInstanceOf[java.util.function.DoubleBinaryOperator]
    case _ => new AsJavaDoubleBinaryOperator(sf.asInstanceOf[scala.Function2[Double, Double, Double]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromDoubleConsumer(jf: java.util.function.DoubleConsumer): scala.Function1[java.lang.Double, scala.runtime.BoxedUnit] = jf match {
    case AsJavaDoubleConsumer((f @ _)) => f.asInstanceOf[scala.Function1[java.lang.Double, scala.runtime.BoxedUnit]]
    case _ => new FromJavaDoubleConsumer(jf).asInstanceOf[scala.Function1[java.lang.Double, scala.runtime.BoxedUnit]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaDoubleConsumer(sf: scala.Function1[java.lang.Double, scala.runtime.BoxedUnit]): java.util.function.DoubleConsumer = ((sf): AnyRef) match {
    case FromJavaDoubleConsumer((f @ _)) => f.asInstanceOf[java.util.function.DoubleConsumer]
    case _ => new AsJavaDoubleConsumer(sf.asInstanceOf[scala.Function1[Double, Unit]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromDoubleFunction[R](jf: java.util.function.DoubleFunction[R]): scala.Function1[java.lang.Double, R] = jf match {
    case AsJavaDoubleFunction((f @ _)) => f.asInstanceOf[scala.Function1[java.lang.Double, R]]
    case _ => new FromJavaDoubleFunction[R](jf).asInstanceOf[scala.Function1[java.lang.Double, R]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaDoubleFunction[R](sf: scala.Function1[java.lang.Double, R]): java.util.function.DoubleFunction[R] = ((sf): AnyRef) match {
    case FromJavaDoubleFunction((f @ _)) => f.asInstanceOf[java.util.function.DoubleFunction[R]]
    case _ => new AsJavaDoubleFunction[R](sf.asInstanceOf[scala.Function1[Double, R]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromDoublePredicate(jf: java.util.function.DoublePredicate): scala.Function1[java.lang.Double, java.lang.Boolean] = jf match {
    case AsJavaDoublePredicate((f @ _)) => f.asInstanceOf[scala.Function1[java.lang.Double, java.lang.Boolean]]
    case _ => new FromJavaDoublePredicate(jf).asInstanceOf[scala.Function1[java.lang.Double, java.lang.Boolean]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaDoublePredicate(sf: scala.Function1[java.lang.Double, java.lang.Boolean]): java.util.function.DoublePredicate = ((sf): AnyRef) match {
    case FromJavaDoublePredicate((f @ _)) => f.asInstanceOf[java.util.function.DoublePredicate]
    case _ => new AsJavaDoublePredicate(sf.asInstanceOf[scala.Function1[Double, Boolean]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromDoubleSupplier(jf: java.util.function.DoubleSupplier): scala.Function0[java.lang.Double] = jf match {
    case AsJavaDoubleSupplier((f @ _)) => f.asInstanceOf[scala.Function0[java.lang.Double]]
    case _ => new FromJavaDoubleSupplier(jf).asInstanceOf[scala.Function0[java.lang.Double]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaDoubleSupplier(sf: scala.Function0[java.lang.Double]): java.util.function.DoubleSupplier = ((sf): AnyRef) match {
    case FromJavaDoubleSupplier((f @ _)) => f.asInstanceOf[java.util.function.DoubleSupplier]
    case _ => new AsJavaDoubleSupplier(sf.asInstanceOf[scala.Function0[Double]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromDoubleToIntFunction(jf: java.util.function.DoubleToIntFunction): scala.Function1[java.lang.Double, java.lang.Integer] = jf match {
    case AsJavaDoubleToIntFunction((f @ _)) => f.asInstanceOf[scala.Function1[java.lang.Double, java.lang.Integer]]
    case _ => new FromJavaDoubleToIntFunction(jf).asInstanceOf[scala.Function1[java.lang.Double, java.lang.Integer]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaDoubleToIntFunction(sf: scala.Function1[java.lang.Double, java.lang.Integer]): java.util.function.DoubleToIntFunction = ((sf): AnyRef) match {
    case FromJavaDoubleToIntFunction((f @ _)) => f.asInstanceOf[java.util.function.DoubleToIntFunction]
    case _ => new AsJavaDoubleToIntFunction(sf.asInstanceOf[scala.Function1[Double, Int]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromDoubleToLongFunction(jf: java.util.function.DoubleToLongFunction): scala.Function1[java.lang.Double, java.lang.Long] = jf match {
    case AsJavaDoubleToLongFunction((f @ _)) => f.asInstanceOf[scala.Function1[java.lang.Double, java.lang.Long]]
    case _ => new FromJavaDoubleToLongFunction(jf).asInstanceOf[scala.Function1[java.lang.Double, java.lang.Long]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaDoubleToLongFunction(sf: scala.Function1[java.lang.Double, java.lang.Long]): java.util.function.DoubleToLongFunction = ((sf): AnyRef) match {
    case FromJavaDoubleToLongFunction((f @ _)) => f.asInstanceOf[java.util.function.DoubleToLongFunction]
    case _ => new AsJavaDoubleToLongFunction(sf.asInstanceOf[scala.Function1[Double, Long]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromDoubleUnaryOperator(jf: java.util.function.DoubleUnaryOperator): scala.Function1[java.lang.Double, java.lang.Double] = jf match {
    case AsJavaDoubleUnaryOperator((f @ _)) => f.asInstanceOf[scala.Function1[java.lang.Double, java.lang.Double]]
    case _ => new FromJavaDoubleUnaryOperator(jf).asInstanceOf[scala.Function1[java.lang.Double, java.lang.Double]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaDoubleUnaryOperator(sf: scala.Function1[java.lang.Double, java.lang.Double]): java.util.function.DoubleUnaryOperator = ((sf): AnyRef) match {
    case FromJavaDoubleUnaryOperator((f @ _)) => f.asInstanceOf[java.util.function.DoubleUnaryOperator]
    case _ => new AsJavaDoubleUnaryOperator(sf.asInstanceOf[scala.Function1[Double, Double]])
  }
  
  
  @inline def asScalaFromFunction[T, R](jf: java.util.function.Function[T, R]): scala.Function1[T, R] = jf match {
    case AsJavaFunction((f @ _)) => f.asInstanceOf[scala.Function1[T, R]]
    case _ => new FromJavaFunction[T, R](jf).asInstanceOf[scala.Function1[T, R]]
  }
  
  @inline def asJavaFunction[T, R](sf: scala.Function1[T, R]): java.util.function.Function[T, R] = ((sf): AnyRef) match {
    case FromJavaFunction((f @ _)) => f.asInstanceOf[java.util.function.Function[T, R]]
    case _ => new AsJavaFunction[T, R](sf.asInstanceOf[scala.Function1[T, R]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromIntBinaryOperator(jf: java.util.function.IntBinaryOperator): scala.Function2[java.lang.Integer, java.lang.Integer, java.lang.Integer] = jf match {
    case AsJavaIntBinaryOperator((f @ _)) => f.asInstanceOf[scala.Function2[java.lang.Integer, java.lang.Integer, java.lang.Integer]]
    case _ => new FromJavaIntBinaryOperator(jf).asInstanceOf[scala.Function2[java.lang.Integer, java.lang.Integer, java.lang.Integer]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaIntBinaryOperator(sf: scala.Function2[java.lang.Integer, java.lang.Integer, java.lang.Integer]): java.util.function.IntBinaryOperator = ((sf): AnyRef) match {
    case FromJavaIntBinaryOperator((f @ _)) => f.asInstanceOf[java.util.function.IntBinaryOperator]
    case _ => new AsJavaIntBinaryOperator(sf.asInstanceOf[scala.Function2[Int, Int, Int]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromIntConsumer(jf: java.util.function.IntConsumer): scala.Function1[java.lang.Integer, scala.runtime.BoxedUnit] = jf match {
    case AsJavaIntConsumer((f @ _)) => f.asInstanceOf[scala.Function1[java.lang.Integer, scala.runtime.BoxedUnit]]
    case _ => new FromJavaIntConsumer(jf).asInstanceOf[scala.Function1[java.lang.Integer, scala.runtime.BoxedUnit]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaIntConsumer(sf: scala.Function1[java.lang.Integer, scala.runtime.BoxedUnit]): java.util.function.IntConsumer = ((sf): AnyRef) match {
    case FromJavaIntConsumer((f @ _)) => f.asInstanceOf[java.util.function.IntConsumer]
    case _ => new AsJavaIntConsumer(sf.asInstanceOf[scala.Function1[Int, Unit]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromIntFunction[R](jf: java.util.function.IntFunction[R]): scala.Function1[java.lang.Integer, R] = jf match {
    case AsJavaIntFunction((f @ _)) => f.asInstanceOf[scala.Function1[java.lang.Integer, R]]
    case _ => new FromJavaIntFunction[R](jf).asInstanceOf[scala.Function1[java.lang.Integer, R]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaIntFunction[R](sf: scala.Function1[java.lang.Integer, R]): java.util.function.IntFunction[R] = ((sf): AnyRef) match {
    case FromJavaIntFunction((f @ _)) => f.asInstanceOf[java.util.function.IntFunction[R]]
    case _ => new AsJavaIntFunction[R](sf.asInstanceOf[scala.Function1[Int, R]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromIntPredicate(jf: java.util.function.IntPredicate): scala.Function1[java.lang.Integer, java.lang.Boolean] = jf match {
    case AsJavaIntPredicate((f @ _)) => f.asInstanceOf[scala.Function1[java.lang.Integer, java.lang.Boolean]]
    case _ => new FromJavaIntPredicate(jf).asInstanceOf[scala.Function1[java.lang.Integer, java.lang.Boolean]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaIntPredicate(sf: scala.Function1[java.lang.Integer, java.lang.Boolean]): java.util.function.IntPredicate = ((sf): AnyRef) match {
    case FromJavaIntPredicate((f @ _)) => f.asInstanceOf[java.util.function.IntPredicate]
    case _ => new AsJavaIntPredicate(sf.asInstanceOf[scala.Function1[Int, Boolean]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromIntSupplier(jf: java.util.function.IntSupplier): scala.Function0[java.lang.Integer] = jf match {
    case AsJavaIntSupplier((f @ _)) => f.asInstanceOf[scala.Function0[java.lang.Integer]]
    case _ => new FromJavaIntSupplier(jf).asInstanceOf[scala.Function0[java.lang.Integer]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaIntSupplier(sf: scala.Function0[java.lang.Integer]): java.util.function.IntSupplier = ((sf): AnyRef) match {
    case FromJavaIntSupplier((f @ _)) => f.asInstanceOf[java.util.function.IntSupplier]
    case _ => new AsJavaIntSupplier(sf.asInstanceOf[scala.Function0[Int]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromIntToDoubleFunction(jf: java.util.function.IntToDoubleFunction): scala.Function1[java.lang.Integer, java.lang.Double] = jf match {
    case AsJavaIntToDoubleFunction((f @ _)) => f.asInstanceOf[scala.Function1[java.lang.Integer, java.lang.Double]]
    case _ => new FromJavaIntToDoubleFunction(jf).asInstanceOf[scala.Function1[java.lang.Integer, java.lang.Double]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaIntToDoubleFunction(sf: scala.Function1[java.lang.Integer, java.lang.Double]): java.util.function.IntToDoubleFunction = ((sf): AnyRef) match {
    case FromJavaIntToDoubleFunction((f @ _)) => f.asInstanceOf[java.util.function.IntToDoubleFunction]
    case _ => new AsJavaIntToDoubleFunction(sf.asInstanceOf[scala.Function1[Int, Double]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromIntToLongFunction(jf: java.util.function.IntToLongFunction): scala.Function1[java.lang.Integer, java.lang.Long] = jf match {
    case AsJavaIntToLongFunction((f @ _)) => f.asInstanceOf[scala.Function1[java.lang.Integer, java.lang.Long]]
    case _ => new FromJavaIntToLongFunction(jf).asInstanceOf[scala.Function1[java.lang.Integer, java.lang.Long]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaIntToLongFunction(sf: scala.Function1[java.lang.Integer, java.lang.Long]): java.util.function.IntToLongFunction = ((sf): AnyRef) match {
    case FromJavaIntToLongFunction((f @ _)) => f.asInstanceOf[java.util.function.IntToLongFunction]
    case _ => new AsJavaIntToLongFunction(sf.asInstanceOf[scala.Function1[Int, Long]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromIntUnaryOperator(jf: java.util.function.IntUnaryOperator): scala.Function1[java.lang.Integer, java.lang.Integer] = jf match {
    case AsJavaIntUnaryOperator((f @ _)) => f.asInstanceOf[scala.Function1[java.lang.Integer, java.lang.Integer]]
    case _ => new FromJavaIntUnaryOperator(jf).asInstanceOf[scala.Function1[java.lang.Integer, java.lang.Integer]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaIntUnaryOperator(sf: scala.Function1[java.lang.Integer, java.lang.Integer]): java.util.function.IntUnaryOperator = ((sf): AnyRef) match {
    case FromJavaIntUnaryOperator((f @ _)) => f.asInstanceOf[java.util.function.IntUnaryOperator]
    case _ => new AsJavaIntUnaryOperator(sf.asInstanceOf[scala.Function1[Int, Int]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromLongBinaryOperator(jf: java.util.function.LongBinaryOperator): scala.Function2[java.lang.Long, java.lang.Long, java.lang.Long] = jf match {
    case AsJavaLongBinaryOperator((f @ _)) => f.asInstanceOf[scala.Function2[java.lang.Long, java.lang.Long, java.lang.Long]]
    case _ => new FromJavaLongBinaryOperator(jf).asInstanceOf[scala.Function2[java.lang.Long, java.lang.Long, java.lang.Long]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaLongBinaryOperator(sf: scala.Function2[java.lang.Long, java.lang.Long, java.lang.Long]): java.util.function.LongBinaryOperator = ((sf): AnyRef) match {
    case FromJavaLongBinaryOperator((f @ _)) => f.asInstanceOf[java.util.function.LongBinaryOperator]
    case _ => new AsJavaLongBinaryOperator(sf.asInstanceOf[scala.Function2[Long, Long, Long]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromLongConsumer(jf: java.util.function.LongConsumer): scala.Function1[java.lang.Long, scala.runtime.BoxedUnit] = jf match {
    case AsJavaLongConsumer((f @ _)) => f.asInstanceOf[scala.Function1[java.lang.Long, scala.runtime.BoxedUnit]]
    case _ => new FromJavaLongConsumer(jf).asInstanceOf[scala.Function1[java.lang.Long, scala.runtime.BoxedUnit]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaLongConsumer(sf: scala.Function1[java.lang.Long, scala.runtime.BoxedUnit]): java.util.function.LongConsumer = ((sf): AnyRef) match {
    case FromJavaLongConsumer((f @ _)) => f.asInstanceOf[java.util.function.LongConsumer]
    case _ => new AsJavaLongConsumer(sf.asInstanceOf[scala.Function1[Long, Unit]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromLongFunction[R](jf: java.util.function.LongFunction[R]): scala.Function1[java.lang.Long, R] = jf match {
    case AsJavaLongFunction((f @ _)) => f.asInstanceOf[scala.Function1[java.lang.Long, R]]
    case _ => new FromJavaLongFunction[R](jf).asInstanceOf[scala.Function1[java.lang.Long, R]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaLongFunction[R](sf: scala.Function1[java.lang.Long, R]): java.util.function.LongFunction[R] = ((sf): AnyRef) match {
    case FromJavaLongFunction((f @ _)) => f.asInstanceOf[java.util.function.LongFunction[R]]
    case _ => new AsJavaLongFunction[R](sf.asInstanceOf[scala.Function1[Long, R]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromLongPredicate(jf: java.util.function.LongPredicate): scala.Function1[java.lang.Long, java.lang.Boolean] = jf match {
    case AsJavaLongPredicate((f @ _)) => f.asInstanceOf[scala.Function1[java.lang.Long, java.lang.Boolean]]
    case _ => new FromJavaLongPredicate(jf).asInstanceOf[scala.Function1[java.lang.Long, java.lang.Boolean]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaLongPredicate(sf: scala.Function1[java.lang.Long, java.lang.Boolean]): java.util.function.LongPredicate = ((sf): AnyRef) match {
    case FromJavaLongPredicate((f @ _)) => f.asInstanceOf[java.util.function.LongPredicate]
    case _ => new AsJavaLongPredicate(sf.asInstanceOf[scala.Function1[Long, Boolean]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromLongSupplier(jf: java.util.function.LongSupplier): scala.Function0[java.lang.Long] = jf match {
    case AsJavaLongSupplier((f @ _)) => f.asInstanceOf[scala.Function0[java.lang.Long]]
    case _ => new FromJavaLongSupplier(jf).asInstanceOf[scala.Function0[java.lang.Long]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaLongSupplier(sf: scala.Function0[java.lang.Long]): java.util.function.LongSupplier = ((sf): AnyRef) match {
    case FromJavaLongSupplier((f @ _)) => f.asInstanceOf[java.util.function.LongSupplier]
    case _ => new AsJavaLongSupplier(sf.asInstanceOf[scala.Function0[Long]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromLongToDoubleFunction(jf: java.util.function.LongToDoubleFunction): scala.Function1[java.lang.Long, java.lang.Double] = jf match {
    case AsJavaLongToDoubleFunction((f @ _)) => f.asInstanceOf[scala.Function1[java.lang.Long, java.lang.Double]]
    case _ => new FromJavaLongToDoubleFunction(jf).asInstanceOf[scala.Function1[java.lang.Long, java.lang.Double]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaLongToDoubleFunction(sf: scala.Function1[java.lang.Long, java.lang.Double]): java.util.function.LongToDoubleFunction = ((sf): AnyRef) match {
    case FromJavaLongToDoubleFunction((f @ _)) => f.asInstanceOf[java.util.function.LongToDoubleFunction]
    case _ => new AsJavaLongToDoubleFunction(sf.asInstanceOf[scala.Function1[Long, Double]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromLongToIntFunction(jf: java.util.function.LongToIntFunction): scala.Function1[java.lang.Long, java.lang.Integer] = jf match {
    case AsJavaLongToIntFunction((f @ _)) => f.asInstanceOf[scala.Function1[java.lang.Long, java.lang.Integer]]
    case _ => new FromJavaLongToIntFunction(jf).asInstanceOf[scala.Function1[java.lang.Long, java.lang.Integer]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaLongToIntFunction(sf: scala.Function1[java.lang.Long, java.lang.Integer]): java.util.function.LongToIntFunction = ((sf): AnyRef) match {
    case FromJavaLongToIntFunction((f @ _)) => f.asInstanceOf[java.util.function.LongToIntFunction]
    case _ => new AsJavaLongToIntFunction(sf.asInstanceOf[scala.Function1[Long, Int]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromLongUnaryOperator(jf: java.util.function.LongUnaryOperator): scala.Function1[java.lang.Long, java.lang.Long] = jf match {
    case AsJavaLongUnaryOperator((f @ _)) => f.asInstanceOf[scala.Function1[java.lang.Long, java.lang.Long]]
    case _ => new FromJavaLongUnaryOperator(jf).asInstanceOf[scala.Function1[java.lang.Long, java.lang.Long]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaLongUnaryOperator(sf: scala.Function1[java.lang.Long, java.lang.Long]): java.util.function.LongUnaryOperator = ((sf): AnyRef) match {
    case FromJavaLongUnaryOperator((f @ _)) => f.asInstanceOf[java.util.function.LongUnaryOperator]
    case _ => new AsJavaLongUnaryOperator(sf.asInstanceOf[scala.Function1[Long, Long]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromObjDoubleConsumer[T](jf: java.util.function.ObjDoubleConsumer[T]): scala.Function2[T, java.lang.Double, scala.runtime.BoxedUnit] = jf match {
    case AsJavaObjDoubleConsumer((f @ _)) => f.asInstanceOf[scala.Function2[T, java.lang.Double, scala.runtime.BoxedUnit]]
    case _ => new FromJavaObjDoubleConsumer[T](jf).asInstanceOf[scala.Function2[T, java.lang.Double, scala.runtime.BoxedUnit]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaObjDoubleConsumer[T](sf: scala.Function2[T, java.lang.Double, scala.runtime.BoxedUnit]): java.util.function.ObjDoubleConsumer[T] = ((sf): AnyRef) match {
    case FromJavaObjDoubleConsumer((f @ _)) => f.asInstanceOf[java.util.function.ObjDoubleConsumer[T]]
    case _ => new AsJavaObjDoubleConsumer[T](sf.asInstanceOf[scala.Function2[T, Double, Unit]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromObjIntConsumer[T](jf: java.util.function.ObjIntConsumer[T]): scala.Function2[T, java.lang.Integer, scala.runtime.BoxedUnit] = jf match {
    case AsJavaObjIntConsumer((f @ _)) => f.asInstanceOf[scala.Function2[T, java.lang.Integer, scala.runtime.BoxedUnit]]
    case _ => new FromJavaObjIntConsumer[T](jf).asInstanceOf[scala.Function2[T, java.lang.Integer, scala.runtime.BoxedUnit]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaObjIntConsumer[T](sf: scala.Function2[T, java.lang.Integer, scala.runtime.BoxedUnit]): java.util.function.ObjIntConsumer[T] = ((sf): AnyRef) match {
    case FromJavaObjIntConsumer((f @ _)) => f.asInstanceOf[java.util.function.ObjIntConsumer[T]]
    case _ => new AsJavaObjIntConsumer[T](sf.asInstanceOf[scala.Function2[T, Int, Unit]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromObjLongConsumer[T](jf: java.util.function.ObjLongConsumer[T]): scala.Function2[T, java.lang.Long, scala.runtime.BoxedUnit] = jf match {
    case AsJavaObjLongConsumer((f @ _)) => f.asInstanceOf[scala.Function2[T, java.lang.Long, scala.runtime.BoxedUnit]]
    case _ => new FromJavaObjLongConsumer[T](jf).asInstanceOf[scala.Function2[T, java.lang.Long, scala.runtime.BoxedUnit]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaObjLongConsumer[T](sf: scala.Function2[T, java.lang.Long, scala.runtime.BoxedUnit]): java.util.function.ObjLongConsumer[T] = ((sf): AnyRef) match {
    case FromJavaObjLongConsumer((f @ _)) => f.asInstanceOf[java.util.function.ObjLongConsumer[T]]
    case _ => new AsJavaObjLongConsumer[T](sf.asInstanceOf[scala.Function2[T, Long, Unit]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromPredicate[T](jf: java.util.function.Predicate[T]): scala.Function1[T, java.lang.Boolean] = jf match {
    case AsJavaPredicate((f @ _)) => f.asInstanceOf[scala.Function1[T, java.lang.Boolean]]
    case _ => new FromJavaPredicate[T](jf).asInstanceOf[scala.Function1[T, java.lang.Boolean]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaPredicate[T](sf: scala.Function1[T, java.lang.Boolean]): java.util.function.Predicate[T] = ((sf): AnyRef) match {
    case FromJavaPredicate((f @ _)) => f.asInstanceOf[java.util.function.Predicate[T]]
    case _ => new AsJavaPredicate[T](sf.asInstanceOf[scala.Function1[T, Boolean]])
  }
  
  
  @inline def asScalaFromSupplier[T](jf: java.util.function.Supplier[T]): scala.Function0[T] = jf match {
    case AsJavaSupplier((f @ _)) => f.asInstanceOf[scala.Function0[T]]
    case _ => new FromJavaSupplier[T](jf).asInstanceOf[scala.Function0[T]]
  }
  
  @inline def asJavaSupplier[T](sf: scala.Function0[T]): java.util.function.Supplier[T] = ((sf): AnyRef) match {
    case FromJavaSupplier((f @ _)) => f.asInstanceOf[java.util.function.Supplier[T]]
    case _ => new AsJavaSupplier[T](sf.asInstanceOf[scala.Function0[T]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromToDoubleBiFunction[T, U](jf: java.util.function.ToDoubleBiFunction[T, U]): scala.Function2[T, U, java.lang.Double] = jf match {
    case AsJavaToDoubleBiFunction((f @ _)) => f.asInstanceOf[scala.Function2[T, U, java.lang.Double]]
    case _ => new FromJavaToDoubleBiFunction[T, U](jf).asInstanceOf[scala.Function2[T, U, java.lang.Double]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaToDoubleBiFunction[T, U](sf: scala.Function2[T, U, java.lang.Double]): java.util.function.ToDoubleBiFunction[T, U] = ((sf): AnyRef) match {
    case FromJavaToDoubleBiFunction((f @ _)) => f.asInstanceOf[java.util.function.ToDoubleBiFunction[T, U]]
    case _ => new AsJavaToDoubleBiFunction[T, U](sf.asInstanceOf[scala.Function2[T, U, Double]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromToDoubleFunction[T](jf: java.util.function.ToDoubleFunction[T]): scala.Function1[T, java.lang.Double] = jf match {
    case AsJavaToDoubleFunction((f @ _)) => f.asInstanceOf[scala.Function1[T, java.lang.Double]]
    case _ => new FromJavaToDoubleFunction[T](jf).asInstanceOf[scala.Function1[T, java.lang.Double]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaToDoubleFunction[T](sf: scala.Function1[T, java.lang.Double]): java.util.function.ToDoubleFunction[T] = ((sf): AnyRef) match {
    case FromJavaToDoubleFunction((f @ _)) => f.asInstanceOf[java.util.function.ToDoubleFunction[T]]
    case _ => new AsJavaToDoubleFunction[T](sf.asInstanceOf[scala.Function1[T, Double]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromToIntBiFunction[T, U](jf: java.util.function.ToIntBiFunction[T, U]): scala.Function2[T, U, java.lang.Integer] = jf match {
    case AsJavaToIntBiFunction((f @ _)) => f.asInstanceOf[scala.Function2[T, U, java.lang.Integer]]
    case _ => new FromJavaToIntBiFunction[T, U](jf).asInstanceOf[scala.Function2[T, U, java.lang.Integer]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaToIntBiFunction[T, U](sf: scala.Function2[T, U, java.lang.Integer]): java.util.function.ToIntBiFunction[T, U] = ((sf): AnyRef) match {
    case FromJavaToIntBiFunction((f @ _)) => f.asInstanceOf[java.util.function.ToIntBiFunction[T, U]]
    case _ => new AsJavaToIntBiFunction[T, U](sf.asInstanceOf[scala.Function2[T, U, Int]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromToIntFunction[T](jf: java.util.function.ToIntFunction[T]): scala.Function1[T, java.lang.Integer] = jf match {
    case AsJavaToIntFunction((f @ _)) => f.asInstanceOf[scala.Function1[T, java.lang.Integer]]
    case _ => new FromJavaToIntFunction[T](jf).asInstanceOf[scala.Function1[T, java.lang.Integer]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaToIntFunction[T](sf: scala.Function1[T, java.lang.Integer]): java.util.function.ToIntFunction[T] = ((sf): AnyRef) match {
    case FromJavaToIntFunction((f @ _)) => f.asInstanceOf[java.util.function.ToIntFunction[T]]
    case _ => new AsJavaToIntFunction[T](sf.asInstanceOf[scala.Function1[T, Int]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromToLongBiFunction[T, U](jf: java.util.function.ToLongBiFunction[T, U]): scala.Function2[T, U, java.lang.Long] = jf match {
    case AsJavaToLongBiFunction((f @ _)) => f.asInstanceOf[scala.Function2[T, U, java.lang.Long]]
    case _ => new FromJavaToLongBiFunction[T, U](jf).asInstanceOf[scala.Function2[T, U, java.lang.Long]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaToLongBiFunction[T, U](sf: scala.Function2[T, U, java.lang.Long]): java.util.function.ToLongBiFunction[T, U] = ((sf): AnyRef) match {
    case FromJavaToLongBiFunction((f @ _)) => f.asInstanceOf[java.util.function.ToLongBiFunction[T, U]]
    case _ => new AsJavaToLongBiFunction[T, U](sf.asInstanceOf[scala.Function2[T, U, Long]])
  }
  
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asScalaFromToLongFunction[T](jf: java.util.function.ToLongFunction[T]): scala.Function1[T, java.lang.Long] = jf match {
    case AsJavaToLongFunction((f @ _)) => f.asInstanceOf[scala.Function1[T, java.lang.Long]]
    case _ => new FromJavaToLongFunction[T](jf).asInstanceOf[scala.Function1[T, java.lang.Long]]
  }
  
  /** Note: this method uses the boxed type `java.lang.X` (or `BoxedUnit`) instead of the
    * primitive type `scala.X` to improve compatibility when using it in Java code (the
    * Scala compiler emits `C[Int]` as `C[Object]` in bytecode due to
    * [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In Scala code, add
    * `import scala.jdk.FunctionConverters._` and use the extension methods instead.
    */
  @inline def asJavaToLongFunction[T](sf: scala.Function1[T, java.lang.Long]): java.util.function.ToLongFunction[T] = ((sf): AnyRef) match {
    case FromJavaToLongFunction((f @ _)) => f.asInstanceOf[java.util.function.ToLongFunction[T]]
    case _ => new AsJavaToLongFunction[T](sf.asInstanceOf[scala.Function1[T, Long]])
  }
  
  
  @inline def asScalaFromUnaryOperator[T](jf: java.util.function.UnaryOperator[T]): scala.Function1[T, T] = jf match {
    case AsJavaUnaryOperator((f @ _)) => f.asInstanceOf[scala.Function1[T, T]]
    case _ => new FromJavaUnaryOperator[T](jf).asInstanceOf[scala.Function1[T, T]]
  }
  
  @inline def asJavaUnaryOperator[T](sf: scala.Function1[T, T]): java.util.function.UnaryOperator[T] = ((sf): AnyRef) match {
    case FromJavaUnaryOperator((f @ _)) => f.asInstanceOf[java.util.function.UnaryOperator[T]]
    case _ => new AsJavaUnaryOperator[T](sf.asInstanceOf[scala.Function1[T, T]])
  }
}

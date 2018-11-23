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

package scala

import java.util.{Optional, OptionalDouble, OptionalInt, OptionalLong}

/** This class enables bidirectional conversion between `scala.Option` and the
 * set of `java.util.Optional` classes.
 *
 * The Scala `Option` is generic; its generic counterpart in Java is
 * `java.util.Optional`.  `Option` is enriched with an `asJava` method, while
 * `Optional` is enriched with `asScala` to perform conversions.
 *
 * In addition, both `Option` and `Optional` are enriched with `asPrimitive`
 * methods that will convert generically contained primitives to the manually
 * specialized Java versions for primitives, `OptionalDouble`, `OptionalInt`,
 * and `OptionalLong`.  The primitive versions can be converted to the Scala
 * generic `Option` with `asScala` and to the Java generic `Optional` with
 * `asGeneric`.
 *
 * When calling from Java, methods are more convenient than extension methods,
 * so `toJava` and `toScala` methods are provided that convert to and from
 * Scala's `Option`.  Note that `toJava(toScala(x))` will result in a generic
 * `Optional` even if `x` was one of the primitive versons.
 *
 * Example usage:
 *
 * {{{
 * import scala.compat.java8.OptionConverters._
 * val a = Option("example").asJava      // Creates java.util.Optional[String] containing "example"
 * val b = (None: Option[String]).asJava // Creates an empty java.util.Optional[String]
 * val c = a.asScala                     // Back to Option("example")
 * val d = b.asScala                     // Back to None typed as Option[String]
 * val e = Option(2.7).asJava            // java.util.Optional[Double] containing boxed 2.7
 * val f = Option(2.7).asPrimitive       // java.util.OptionalDouble containing 2.7 (not boxed)
 * val g = f.asScala                     // Back to Option(2.7)
 * val h = f.asGeneric                   // Same as e
 * val i = e.asPrimitive                 // Same as f
 * val j = toJava(Option("example"))     // Same as a
 * val k = toScala(a)                    // Same as c
 * }}}
 */
object JavaConvertersOption {
  /** Type class implementing conversion from generic `Option` or `Optional` to manually specialized variants. */
  sealed abstract class SpecializerOfOptions[A, That] {
    /** Converts from `Optional` to a manually specialized variant `That` */
    def fromJava(o: Optional[A]): That
    /** Converts from `Option` to a manually specialized variant `That` */
    def fromScala(o: Option[A]): That
  }

  /** Implementation of creation of `OptionalDouble` from `Option[Double]` or `Optional[Double]`*/
  implicit val specializer_OptionalDouble = new SpecializerOfOptions[Double, OptionalDouble] {
    /** Creates an `OptionalDouble` from `Optional[Double]` */
    def fromJava(o: Optional[Double]): OptionalDouble = if (o.isPresent) OptionalDouble.of(o.get) else OptionalDouble.empty
    /** Creates an `OptionalDouble` from `Option[Double]` */
    def fromScala(o: Option[Double]): OptionalDouble = o match { case Some(d) => OptionalDouble.of(d); case _ => OptionalDouble.empty }
  }

  /** Implementation of creation of `OptionalInt` from `Option[Int]` or `Optional[Int]`*/
  implicit val specializer_OptionalInt = new SpecializerOfOptions[Int, OptionalInt] {
    /** Creates an `OptionalInt` from `Optional[Int]` */
    def fromJava(o: Optional[Int]): OptionalInt = if (o.isPresent) OptionalInt.of(o.get) else OptionalInt.empty
    /** Creates an `OptionalInt` from `Option[Int]` */
    def fromScala(o: Option[Int]): OptionalInt = o match { case Some(d) => OptionalInt.of(d); case _ => OptionalInt.empty }
  }

  /** Implementation of creation of `OptionalLong` from `Option[Long]` or `Optional[Long]`*/
  implicit val specializer_OptionalLong = new SpecializerOfOptions[Long, OptionalLong] {
    /** Creates an `OptionalLong` from `Optional[Long]` */
    def fromJava(o: Optional[Long]): OptionalLong = if (o.isPresent) OptionalLong.of(o.get) else OptionalLong.empty
    /** Creates an `OptionalLong` from `Option[Long]` */
    def fromScala(o: Option[Long]): OptionalLong = o match { case Some(d) => OptionalLong.of(d); case _ => OptionalLong.empty }
  }

  /** Provides conversions from `java.util.Optional` to Scala `Option` or primitive `java.util.Optional` types */
  implicit class RichOptionalGeneric[A](val underlying: java.util.Optional[A]) extends AnyVal {
    /** Create a `scala.Option` version of this `Optional` */
    def asScala: Option[A] = if (underlying.isPresent) Some(underlying.get) else None
    /** Create a specialized primitive variant of this generic `Optional`, if an appropriate one exists */
    def asPrimitive[That](implicit specOp: SpecializerOfOptions[A, That]): That = specOp.fromJava(underlying)
  }

  /** Provides conversions from `scala.Option` to Java `Optional` types, either generic or primitive */
  implicit class RichOptionForJava8[A](val underlying: Option[A]) extends AnyVal {
    /** Create a `java.util.Optional` version of this `Option` (not specialized) */
    def asJava: Optional[A] = underlying match { case Some(a) => Optional.ofNullable(a); case _ => Optional.empty[A] }
    /** Create a specialized primitive `java.util.Optional` type, if an appropriate one exists */
    def asPrimitive[That](implicit specOp: SpecializerOfOptions[A, That]): That = specOp.fromScala(underlying)
  }

  /** Provides conversions from `java.util.OptionalDouble` to the generic `Optional` and Scala `Option` */
  implicit class RichOptionalDouble(val underlying: OptionalDouble) extends AnyVal {
    /** Create a `scala.Option` version of this `OptionalDouble` */
    def asScala: Option[Double] = if (underlying.isPresent) Some(underlying.getAsDouble) else None
    /** Create a generic `java.util.Optional` version of this `OptionalDouble` */
    def asGeneric: Optional[Double] = if (underlying.isPresent) Optional.of(underlying.getAsDouble) else Optional.empty[Double]
  }

  /** Provides conversions from `java.util.OptionalInt` to the generic `Optional` and Scala `Option` */
  implicit class RichOptionalInt(val underlying: OptionalInt) extends AnyVal {
    /** Create a `scala.Option` version of this `OptionalInt` */
    def asScala: Option[Int] = if (underlying.isPresent) Some(underlying.getAsInt) else None
    /** Create a generic `java.util.Optional` version of this `OptionalInt` */
    def asGeneric: Optional[Int] = if (underlying.isPresent) Optional.of(underlying.getAsInt) else Optional.empty[Int]
  }

  /** Provides conversions from `java.util.OptionalLong` to the generic `Optional` and Scala `Option` */
  implicit class RichOptionalLong(val underlying: OptionalLong) extends AnyVal {
    /** Create a `scala.Option` version of this `OptionalLong` */
    def asScala: Option[Long] = if (underlying.isPresent) Some(underlying.getAsLong) else None
    /** Create a generic `java.util.Optional` version of this `OptionalLong` */
    def asGeneric: Optional[Long] = if (underlying.isPresent) Optional.of(underlying.getAsLong) else Optional.empty[Long]
  }

  /** Conversion from Scala `Option` to Java `Optional` without using implicits, for convenient use from Java. */
  final def toJava[A](o: Option[A]): Optional[A] = o match { case Some(a) => Optional.ofNullable(a); case _ => Optional.empty[A] }

  /** Conversion from Java `Optional` to Scala `Option` without using implicits, for convenient use from Java */
  final def toScala[A](o: Optional[A]): Option[A] = if (o.isPresent) Some(o.get) else None

  /** Conversion from Java `OptionalDouble` to Scala `Option` without using implicits, for convenient use from Java */
  final def toScala(o: OptionalDouble): Option[Double] = if (o.isPresent) Some(o.getAsDouble) else None

  /** Conversion from Java `OptionalInt` to Scala `Option` without using implicits, for convenient use from Java */
  final def toScala(o: OptionalInt): Option[Int] = if (o.isPresent) Some(o.getAsInt) else None

  /** Conversion from Java `OptionalLong` to Scala `Option` without using implicits, for convenient use from Java */
  final def toScala(o: OptionalLong): Option[Long] = if (o.isPresent) Some(o.getAsLong) else None
}

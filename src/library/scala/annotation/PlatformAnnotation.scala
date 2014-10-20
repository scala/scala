/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2015, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.annotation

/** A trait for annotations which are intended to be represented by the underlying platforms annotation capabilities. */
sealed trait PlatformAnnotation extends ConstantAnnotation with java.lang.annotation.Annotation

/** A base class for classfile annotations not visible at runtime.
 *
 *  Inheriting from this trait is equivalent to adding the meta-annotation
 *  `@java.lang.annotation.Retention(java.lang.annotation.RetentionPolicy.CLASS)`
 *  to an annotation in Java.
 *
 *  These are stored as
 *  [[http://docs.oracle.com/javase/7/docs/technotes/guides/language/annotations.html#_top Java annotations]]]
 *  in classfiles.
 *
 *  @author  Martin Odersky
 *  @version 1.1, 2/02/2007
 *  @since 2.4
 */
abstract class ClassfileAnnotation extends PlatformAnnotation

/** A base class for classfile annotations visible at runtime.
 *
 *  Inheriting from this trait is equivalent to adding the meta-annotation
 *  `@java.lang.annotation.Retention(java.lang.annotation.RetentionPolicy.RUNTIME)`
 *  to an annotation in Java.
 *
 *  These are stored as
 *  [[http://docs.oracle.com/javase/7/docs/technotes/guides/language/annotations.html#_top Java annotations]]]
 *  in classfiles.
 *
 *  @since 2.12
 */
abstract class RuntimeAnnotation extends PlatformAnnotation

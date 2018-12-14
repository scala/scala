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

package scala.annotation;

import java.lang.annotation.*;

/**
 * Marks APIs that are considered internal to the library and may change at any point in time without any warning.
 * <p/>
 * For example, this annotation should be used when the Scala {@code private[foo]} access restriction is used,
 * as Java has no way of representing this package restricted access and such methods and classes are represented
 * as {@code public} in byte-code.
 * <p/>
 * If a method/class annotated with this method has a javadoc/scaladoc comment, the first line MUST include
 * {@code INTERNAL API} in order to be easily identifiable from generated documentation. Additional information
 * may be put on the same line as the INTERNAL API comment in order to clarify further.
 *
 * @since 2.13.0
 */
@Documented
@Retention(RetentionPolicy.CLASS) // to be accessible by MiMa
@Target({ElementType.METHOD, ElementType.CONSTRUCTOR, ElementType.FIELD, ElementType.TYPE, ElementType.PACKAGE})
public @interface InternalApi {
}

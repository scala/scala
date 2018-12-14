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
 * Marks APIs that are designed under an closed-world assumption for and are NOT meant to be extended by user-code.
 * It is fine to extend these classes within the library itself however.
 * <p/>
 * This is most useful for binary compatibility purposes when a set of classes and interfaces assume a "closed world"
 * between them, and gain the ability to add methods to the interfaces without breaking binary compatibility for
 * users of this code. Specifically this assumption may be understood intuitively: as all classes that implement this
 * interface are in this compilation unit / artifact, it is impossible to obtain a "old" class with a "new" interface,
 * as they are part of the same dependency.
 *
 * @since 2.13.0
 */
@Documented
@Retention(RetentionPolicy.CLASS) // to be accessible by MiMa
@Target({ElementType.TYPE})
public @interface DoNotInherit {
}

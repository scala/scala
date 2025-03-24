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

package scala

import scala.annotation.meta._

/** An annotation that designates that the name of a parameter is deprecated.
 *
 *  Using this name in a named argument generates a deprecation warning.
 *
 *  If the `name` is omitted, then using the canonical name is deprecated.
 *  In that case, lints such as `-Xlint:named-booleans` which encourage
 *  the use of a name will not warn.
 *
 *  Library authors should state the library's deprecation policy in their documentation to give
 *  developers guidance on how long a deprecated name will be preserved.
 *
 *  Library authors should prepend the name of their library to the version number to help
 *  developers distinguish deprecations coming from different libraries:
 *
 *  {{{
 *  def inc(x: Int, @deprecatedName("y", "FooLib 12.0") n: Int): Int = x + n
 *  inc(1, y = 2)
 *  }}}
 *  will produce the following warning:
 *  {{{
 *  warning: the parameter name y is deprecated (since FooLib 12.0): use n instead
 *  inc(1, y = 2)
 *           ^
 *  }}}
 *
 *  @see    [[scala.deprecated]]
 *  @see    [[scala.deprecatedInheritance]]
 *  @see    [[scala.deprecatedOverriding]]
 */
@param
@deprecatedInheritance("Scheduled for being final in the future", "2.13.0")
class deprecatedName(name: String = "<none>", since: String = "") extends scala.annotation.StaticAnnotation {
  // at the time we remove these constructors, we should also change this from a StaticAnnotation to
  // a ConstantAnnotation; for now, the presence of auxiliary constructors blocks that change
  @deprecated("The parameter name should be a String, not a symbol.", "2.13.0") def this(name: Symbol, since: String) = this(name.name, since)
  @deprecated("The parameter name should be a String, not a symbol.", "2.13.0") def this(name: Symbol) = this(name.name, "")
}

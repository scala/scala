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

/** An annotation that designates that a definition is deprecated.
 *  A deprecation warning is issued upon usage of the annotated definition.
 *
 *  Library authors should state the library's deprecation policy in their documentation to give
 *  developers guidance on how long a deprecated definition will be preserved.
 *
 *  Library authors should prepend the name of their library to the version number to help
 *  developers distinguish deprecations coming from different libraries:
 *
 *  {{{
 *  @deprecated("this method will be removed", "FooLib 12.0")
 *  def oldMethod(x: Int) = ...
 *  }}}
 *
 *  The compiler will emit deprecation warnings grouped by library and version:
 *
 *  {{{
 *  oldMethod(1)
 *  oldMethod(2)
 *  aDeprecatedMethodFromLibraryBar(3, 4)
 *
 *  // warning: there was one deprecation warning (since BarLib 3.2)
 *  // warning: there were two deprecation warnings (since FooLib 12.0)
 *  // warning: there were three deprecation warnings in total; re-run with -deprecation for details
 *  }}}
 *
 *  The Scala compiler also warns about using definitions annotated with [[java.lang.Deprecated]]. However it is
 *  recommended to use the Scala `@deprecated` annotation in Scala code because it allows providing a deprecation message.
 *
 *  '''`@deprecated` in the Scala language and its standard library'''<br/>
 *
 *  A deprecated element of the Scala language or a definition in the Scala standard library will
 *  be preserved at least for the current major version.
 *
 *  This means that an element deprecated in some 2.13.x release will be preserved in
 *  all 2.13.x releases, but may be removed in the future. (A deprecated element
 *  might be kept longer to ease migration, but developers should not rely on this.)
 *
 *  @see    The official documentation on [[https://www.scala-lang.org/news/2.11.0/#binary-compatibility binary compatibility]].
 *  @param  message the message to print during compilation if the definition is accessed
 *  @param  since   a string identifying the first version in which the definition was deprecated
 *  @see    [[scala.deprecatedInheritance]]
 *  @see    [[scala.deprecatedOverriding]]
 *  @see    [[scala.deprecatedName]]
 */
@getter @setter @beanGetter @beanSetter @field
@deprecatedInheritance("Scheduled for being final in the future", "2.13.0")
class deprecated(message: String = "", since: String = "") extends scala.annotation.ConstantAnnotation

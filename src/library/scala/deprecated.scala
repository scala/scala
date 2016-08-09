/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

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
 *  '''`@deprecated` in the Scala language and its standard library'''<br/>
 *
 *  A deprecated element of the Scala language or a definition in the Scala standard library will
 *  be preserved or at least another major version.
 *
 *  This means that an element deprecated since 2.12 will be preserved in 2.13, but will very likely
 *  not be part of 2.14. Sometimes a deprecated element might be kept for more than a major
 *  release to ease migration and upgrades from older Scala versions.<br/>
 *  Developers should not rely on this.
 *
 *  '''Special deprecation policy for Scala 2.12'''<br>
 *  The Scala team has decided to enact a special deprecation policy for the 2.12 release:<br/>
 *
 *  As an upgrade from Scala 2.11 to Scala 2.12 also requires upgrading from Java 6 to Java 8,
 *  no deprecated elements will be removed in this release to ease migration and upgrades
 *  from older Scala versions. This means that elements deprecated since 2.11 (or earlier)
 *  will not be removed in Scala 2.12.
 *
 *  @see    The official documentation on [[http://www.scala-lang.org/news/2.11.0/#binary-compatibility binary compatibility]].
 *  @param  message the message to print during compilation if the definition is accessed
 *  @param  since   a string identifying the first version in which the definition was deprecated
 *  @since  2.3
 *  @see    [[scala.deprecatedInheritance]]
 *  @see    [[scala.deprecatedOverriding]]
 *  @see    [[scala.deprecatedName]]
 */
@getter @setter @beanGetter @beanSetter
class deprecated(message: String = "", since: String = "") extends scala.annotation.StaticAnnotation

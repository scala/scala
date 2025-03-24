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

/** An annotation that designates that inheriting from a class is deprecated.
 *
 *  This is usually done to warn about a non-final class being made final in a future version.
 *  Sub-classing such a class then generates a warning.
 *
 *  No warnings are generated if the subclass is in the same compilation unit.
 *
 *  Library authors should state the library's deprecation policy in their documentation to give
 *  developers guidance on when a type annotated with `@deprecatedInheritance` will be `final`ized.
 *
 *  Library authors should prepend the name of their library to the version number to help
 *  developers distinguish deprecations coming from different libraries:
 *
 *  {{{
 *  @deprecatedInheritance("this class will be made final", "FooLib 12.0")
 *  class Foo
 *  }}}
 *
 *  {{{
 *  val foo = new Foo     // no deprecation warning
 *  class Bar extends Foo
 *  // warning: inheritance from class Foo is deprecated (since FooLib 12.0): this class will be made final
 *  // class Bar extends Foo
 *  //                   ^
 *  }}}
 *
 *  @param  message the message to print during compilation if the class was sub-classed
 *  @param  since   a string identifying the first version in which inheritance was deprecated
 *  @see    [[scala.deprecated]]
 *  @see    [[scala.deprecatedOverriding]]
 *  @see    [[scala.deprecatedName]]
 */
@getter @setter @beanGetter @beanSetter
final class deprecatedInheritance(message: String = "", since: String = "") extends scala.annotation.ConstantAnnotation

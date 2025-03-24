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

/** An annotation that designates that overriding a member is deprecated.
 *
 *  Overriding such a member in  a sub-class then generates a warning.
 *
 *  Library authors should state the library's deprecation policy in their documentation to give
 *  developers guidance on when a method annotated with `@deprecatedOverriding` will be `final`ized.
 *
 *  Library authors should prepend the name of their library to the version number to help
 *  developers distinguish deprecations coming from different libraries:
 *
 *  {{{
 *  class Foo {
 *    @deprecatedOverriding("this method will be made final", "FooLib 12.0")
 *    def add(x: Int, y: Int) = x + y
 *  }
 *  }}}
 *
 *  {{{
 *  class Bar extends Foo // no deprecation warning
 *  class Baz extends Foo {
 *    override def add(x: Int, y: Int) = x - y
 *  }
 *  // warning: overriding method add in class Foo is deprecated (since FooLib 12.0): this method will be made final
 *  // override def add(x: Int, y: Int) = x - y
 *  //              ^
 *  }}}
 *
 *  @param  message the message to print during compilation if the member was overridden
 *  @param  since   a string identifying the first version in which overriding was deprecated
 *  @see    [[scala.deprecated]]
 *  @see    [[scala.deprecatedInheritance]]
 *  @see    [[scala.deprecatedName]]
 */
@getter @setter @beanGetter @beanSetter
@deprecatedInheritance("Scheduled for being final in the future", "2.13.0")
class deprecatedOverriding(message: String = "", since: String = "") extends scala.annotation.ConstantAnnotation

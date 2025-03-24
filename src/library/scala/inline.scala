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

/**
 * An annotation for methods that the optimizer should inline.
 *
 * Note that by default, the Scala optimizer is disabled and no callsites are inlined. See
 * `-opt:help` and [[https://docs.scala-lang.org/overviews/compiler-options/optimizer.html the overview document]]
 * for information on how to enable the optimizer and inliner.
 *
 * When inlining is enabled, the inliner will always try to inline methods or callsites annotated
 * `@inline` (under the condition that inlining from the defining class is allowed).
 * If inlining is not possible, for example because the method is not
 * final, an optimizer warning will be issued. See `-Wopt:help` for details.
 *
 * Examples:
 *
 * {{{
 * @inline   final def f1(x: Int) = x
 * @noinline final def f2(x: Int) = x
 *           final def f3(x: Int) = x
 *
 *  def t1 = f1(1)              // inlined if possible
 *  def t2 = f2(1)              // not inlined
 *  def t3 = f3(1)              // may be inlined (the inliner heuristics can select the callsite)
 *  def t4 = f1(1): @noinline   // not inlined (override at callsite)
 *  def t5 = f2(1): @inline     // inlined if possible (override at callsite)
 *  def t6 = f3(1): @inline     // inlined if possible
 *  def t7 = f3(1): @noinline   // not inlined
 * }
 * }}}
 *
 * Note: parentheses are required when annotating a callsite within a larger expression.
 *
 * {{{
 * def t1 = f1(1) + f1(1): @noinline   // equivalent to (f1(1) + f1(1)): @noinline
 * def t2 = f1(1) + (f1(1): @noinline) // the second call to f1 is not inlined
 * }}}
 */
@deprecatedInheritance("Scheduled for being final in the future", "2.13.0")
class inline extends scala.annotation.StaticAnnotation

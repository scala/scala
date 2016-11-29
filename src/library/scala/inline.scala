/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

/**
 * An annotation on methods that requests that the compiler should try especially hard to inline the
 * annotated method. The annotation can be used at definition site or at callsite.
 *
 * {{{
 * @inline   final def f1(x: Int) = x
 * @noinline final def f2(x: Int) = x
 *           final def f3(x: Int) = x
 *
 *  def t1 = f1(1)              // inlined if possible
 *  def t2 = f2(1)              // not inlined
 *  def t3 = f3(1)              // may be inlined (heuristics)
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
 *
 * @author Lex Spoon
 * @version 1.0, 2007-5-21
 */
class inline extends scala.annotation.StaticAnnotation

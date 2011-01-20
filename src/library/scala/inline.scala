/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

/**
 * An annotation on methods that requests that the compiler should
 * try especially hard to inline the annotated method.
 *
 * @author Lex Spoon
 * @version 1.0, 2007-5-21
 */
class inline extends annotation.StaticAnnotation

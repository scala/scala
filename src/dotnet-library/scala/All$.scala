/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: All$.scala 9262 2006-11-14 17:29:59Z mihaylov $


package scala


/**
 * Dummy class which exist only to satisfy the JVM. It corresponds
 * to <code>scala.All</code>. If such type appears in method
 * signatures, it is erased to this one.
 *
 * @deprecated <i>To be removed at some time in the future. Kept only for
 * backward compatibility. Newly compiled code will refer to
 * <code>scala.runtime.Nothing$</code></i>
 */

sealed abstract class All$

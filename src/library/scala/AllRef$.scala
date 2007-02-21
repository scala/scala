/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: AllRef$.scala 9262 2006-11-14 17:29:59 +0000 (Tue, 14 Nov 2006) mihaylov $


package scala


/**
 * Dummy class which exist only to satisfy the JVM. It corresponds
 * to <code>scala.AllRef</code>. If such type appears in method
 * signatures, it is erased to this one.
 *
 * @deprecated <i>To be removed at some time in the future. Kept only for
 * backward compatibility. Newly compiled code will refer to
 * <code>scala.runtime.Null$</code></i>
 */

sealed abstract class AllRef$

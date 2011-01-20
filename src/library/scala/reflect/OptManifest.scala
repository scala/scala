/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.reflect

/** <p>
 *    A <code>OptManifest[T]</code> is an optional <a href="Manifest.html"
 *    target="ContentFrame"><code>Manifest</code></a>.<br/>
 *    It is either a <code>Manifest</code> or the value <code>NoManifest</code>.
 *  </p>
 *
 *  @author Martin Odersky
 */
trait OptManifest[+T] extends Serializable

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime.types;

/**
 * Abstract superclass for suspended computations of parent classes.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public abstract class LazyParents {
    abstract public ScalaClassType[] force();
}

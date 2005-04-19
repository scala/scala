/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;

import scala.runtime.types.ScalaClassType;

/** @meta class extends scala.AnyRef;
 */
public interface ScalaObject {
    /**
     * Return the Scala type of the object.
     */
    public ScalaClassType getScalaType();

    /** This method is needed for optimizing pattern matching expressions
     *  which match on constructors of case classes.
     */
    public int $tag();
}

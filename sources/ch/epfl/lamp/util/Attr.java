/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package ch.epfl.lamp.util;

/** An XML attribute.
 */
public class Attr {

    /** Name of the attribute.
     */
    public String name;

    /** Value of the attribute.
     */
    public String value;

    public Attr(String name, String value) {
	this.name = name;
	this.value = value;
    }

    public String toString() {
	return name + "=\"" + value + "\"";
    }
}
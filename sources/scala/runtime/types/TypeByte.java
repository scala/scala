/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime.types;

import scala.runtime.RunTime;
import scala.Type;
import scala.Array;
import scala.Byte;

public class TypeByte extends BasicType {
    private final Byte ZERO = RunTime.box_bvalue((byte)0);
    public Array newArray(int size) {
        return RunTime.box_barray(new byte[size]);
    }
    public Object defaultValue() { return ZERO; }
};

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

public class TypeFloat extends BasicType {
    private final scala.Float ZERO = RunTime.box_fvalue(0.0f);
    public Array newArray(int size) {
        return RunTime.box_farray(new float[size]);
    }
    public Object defaultValue() { return ZERO; }
};


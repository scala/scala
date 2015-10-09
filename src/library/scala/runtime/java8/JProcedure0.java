
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

import scala.runtime.BoxedUnit;

@FunctionalInterface
public interface JProcedure0 extends JFunction0<BoxedUnit> {
    default void $init$() {
    }

    void applyVoid();

    default BoxedUnit apply() {
        applyVoid();
        return BoxedUnit.UNIT;
    }
}


/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

import scala.runtime.BoxedUnit;

@FunctionalInterface
public interface JProcedure1<T1> extends JFunction1<T1, BoxedUnit> {
    default void $init$() {
    }

    void applyVoid(T1 t1);

    default BoxedUnit apply(T1 t1) {
        applyVoid(t1);
        return BoxedUnit.UNIT;
    }
}

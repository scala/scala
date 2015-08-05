
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

import scala.runtime.BoxedUnit;

@FunctionalInterface
public interface JProcedure4<T1, T2, T3, T4> extends JFunction4<T1, T2, T3, T4, BoxedUnit> {
    default void $init$() {
    }

    void applyVoid(T1 t1, T2 t2, T3 t3, T4 t4);

    default BoxedUnit apply(T1 t1, T2 t2, T3 t3, T4 t4) {
        applyVoid(t1, t2, t3, t4);
        return BoxedUnit.UNIT;
    }
}

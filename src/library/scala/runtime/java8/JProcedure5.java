
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

import scala.runtime.BoxedUnit;

@FunctionalInterface
public interface JProcedure5<T1, T2, T3, T4, T5> extends JFunction5<T1, T2, T3, T4, T5, BoxedUnit> {
    default void $init$() {
    }

    void applyVoid(T1 t1, T2 t2, T3 t3, T4 t4, T5 t5);

    default BoxedUnit apply(T1 t1, T2 t2, T3 t3, T4 t4, T5 t5) {
        applyVoid(t1, t2, t3, t4, t5);
        return BoxedUnit.UNIT;
    }
}

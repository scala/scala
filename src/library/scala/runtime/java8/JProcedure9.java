
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

import scala.runtime.BoxedUnit;

@FunctionalInterface
public interface JProcedure9<T1, T2, T3, T4, T5, T6, T7, T8, T9> extends JFunction9<T1, T2, T3, T4, T5, T6, T7, T8, T9, BoxedUnit> {
    default void $init$() {
    }

    void applyVoid(T1 t1, T2 t2, T3 t3, T4 t4, T5 t5, T6 t6, T7 t7, T8 t8, T9 t9);

    default BoxedUnit apply(T1 t1, T2 t2, T3 t3, T4 t4, T5 t5, T6 t6, T7 t7, T8 t8, T9 t9) {
        applyVoid(t1, t2, t3, t4, t5, t6, t7, t8, t9);
        return BoxedUnit.UNIT;
    }
}

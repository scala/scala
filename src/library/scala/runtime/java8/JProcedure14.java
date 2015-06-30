
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

import scala.runtime.BoxedUnit;

@FunctionalInterface
public interface JProcedure14<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14> extends JFunction14<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, BoxedUnit> {
    default void $init$() {
    }

    void applyVoid(T1 t1, T2 t2, T3 t3, T4 t4, T5 t5, T6 t6, T7 t7, T8 t8, T9 t9, T10 t10, T11 t11, T12 t12, T13 t13, T14 t14);

    default BoxedUnit apply(T1 t1, T2 t2, T3 t3, T4 t4, T5 t5, T6 t6, T7 t7, T8 t8, T9 t9, T10 t10, T11 t11, T12 t12, T13 t13, T14 t14) {
        applyVoid(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14);
        return BoxedUnit.UNIT;
    }
}

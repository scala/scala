
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction6<T1, T2, T3, T4, T5, T6, R> extends scala.Function6<T1, T2, T3, T4, T5, T6, R> {
    default void $init$() {
    };

    default scala.Function1<T1, scala.Function1<T2, scala.Function1<T3, scala.Function1<T4, scala.Function1<T5, scala.Function1<T6, R>>>>>> curried() {
      return scala.Function6$class.curried(this);
    }

    default scala.Function1<scala.Tuple6<T1, T2, T3, T4, T5, T6>, R> tupled() {
      return scala.Function6$class.tupled(this);
    }


}

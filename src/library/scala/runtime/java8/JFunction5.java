
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction5<T1, T2, T3, T4, T5, R> extends scala.Function5<T1, T2, T3, T4, T5, R> {
    default void $init$() {
    };

    default scala.Function1<T1, scala.Function1<T2, scala.Function1<T3, scala.Function1<T4, scala.Function1<T5, R>>>>> curried() {
      return scala.Function5$class.curried(this);
    }

    default scala.Function1<scala.Tuple5<T1, T2, T3, T4, T5>, R> tupled() {
      return scala.Function5$class.tupled(this);
    }


}

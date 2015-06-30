
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction9<T1, T2, T3, T4, T5, T6, T7, T8, T9, R> extends scala.Function9<T1, T2, T3, T4, T5, T6, T7, T8, T9, R> {
    default void $init$() {
    };

    default scala.Function1<T1, scala.Function1<T2, scala.Function1<T3, scala.Function1<T4, scala.Function1<T5, scala.Function1<T6, scala.Function1<T7, scala.Function1<T8, scala.Function1<T9, R>>>>>>>>> curried() {
      return scala.Function9$class.curried(this);
    }

    default scala.Function1<scala.Tuple9<T1, T2, T3, T4, T5, T6, T7, T8, T9>, R> tupled() {
      return scala.Function9$class.tupled(this);
    }


}

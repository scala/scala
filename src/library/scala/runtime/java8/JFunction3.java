
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction3<T1, T2, T3, R> extends scala.Function3<T1, T2, T3, R> {
    default void $init$() {
    };

    default scala.Function1<T1, scala.Function1<T2, scala.Function1<T3, R>>> curried() {
      return scala.Function3$class.curried(this);
    }

    default scala.Function1<scala.Tuple3<T1, T2, T3>, R> tupled() {
      return scala.Function3$class.tupled(this);
    }


}

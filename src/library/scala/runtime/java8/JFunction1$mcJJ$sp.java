
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction1$mcJJ$sp extends scala.Function1, java.io.Serializable {
    long apply$mcJJ$sp(long v1);

    default Object apply(Object t) { return scala.runtime.BoxesRunTime.boxToLong(apply$mcJJ$sp(scala.runtime.BoxesRunTime.unboxToLong(t))); }
}

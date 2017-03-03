
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction1$mcZI$sp extends scala.Function1, java.io.Serializable {
    boolean apply$mcZI$sp(int v1);

    default Object apply(Object t) { return scala.runtime.BoxesRunTime.boxToBoolean(apply$mcZI$sp(scala.runtime.BoxesRunTime.unboxToInt(t))); }
}

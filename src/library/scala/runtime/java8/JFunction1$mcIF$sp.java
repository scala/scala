
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction1$mcIF$sp extends scala.Function1, java.io.Serializable {
    int apply$mcIF$sp(float v1);

    default Object apply(Object t) { return scala.runtime.BoxesRunTime.boxToInteger(apply$mcIF$sp(scala.runtime.BoxesRunTime.unboxToFloat(t))); }
}

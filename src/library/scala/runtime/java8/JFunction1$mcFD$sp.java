
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction1$mcFD$sp extends scala.Function1, java.io.Serializable {
    float apply$mcFD$sp(double v1);

    default Object apply(Object t) { return scala.runtime.BoxesRunTime.boxToFloat(apply$mcFD$sp(scala.runtime.BoxesRunTime.unboxToDouble(t))); }
}

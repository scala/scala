
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction2$mcFJD$sp extends JFunction2 {
    float apply$mcFJD$sp(long v1, double v2);

    default Object apply(Object v1, Object v2) { return scala.runtime.BoxesRunTime.boxToFloat(apply$mcFJD$sp(scala.runtime.BoxesRunTime.unboxToLong(v1), scala.runtime.BoxesRunTime.unboxToDouble(v2))); }
}

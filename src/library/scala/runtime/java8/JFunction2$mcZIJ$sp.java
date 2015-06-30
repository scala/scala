
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction2$mcZIJ$sp extends JFunction2 {
    boolean apply$mcZIJ$sp(int v1, long v2);

    default Object apply(Object v1, Object v2) { return scala.runtime.BoxesRunTime.boxToBoolean(apply$mcZIJ$sp(scala.runtime.BoxesRunTime.unboxToInt(v1), scala.runtime.BoxesRunTime.unboxToLong(v2))); }
}

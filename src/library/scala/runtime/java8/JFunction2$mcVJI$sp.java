
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction2$mcVJI$sp extends JFunction2 {
    void apply$mcVJI$sp(long v1, int v2);

    default Object apply(Object v1, Object v2) { apply$mcVJI$sp(scala.runtime.BoxesRunTime.unboxToLong(v1), scala.runtime.BoxesRunTime.unboxToInt(v2)); return scala.runtime.BoxedUnit.UNIT; }
}


/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction2$mcJJJ$sp extends JFunction2 {
    long apply$mcJJJ$sp(long v1, long v2);

    default Object apply(Object v1, Object v2) { return scala.runtime.BoxesRunTime.boxToLong(apply$mcJJJ$sp(scala.runtime.BoxesRunTime.unboxToLong(v1), scala.runtime.BoxesRunTime.unboxToLong(v2))); }
}

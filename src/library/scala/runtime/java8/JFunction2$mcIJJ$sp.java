
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction2$mcIJJ$sp extends scala.Function2, java.io.Serializable {
    int apply$mcIJJ$sp(long v1, long v2);

    default Object apply(Object v1, Object v2) { return scala.runtime.BoxesRunTime.boxToInteger(apply$mcIJJ$sp(scala.runtime.BoxesRunTime.unboxToLong(v1), scala.runtime.BoxesRunTime.unboxToLong(v2))); }
}

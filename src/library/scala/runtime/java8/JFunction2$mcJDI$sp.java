
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction2$mcJDI$sp extends scala.Function2, java.io.Serializable {
    long apply$mcJDI$sp(double v1, int v2);

    default Object apply(Object v1, Object v2) { return scala.runtime.BoxesRunTime.boxToLong(apply$mcJDI$sp(scala.runtime.BoxesRunTime.unboxToDouble(v1), scala.runtime.BoxesRunTime.unboxToInt(v2))); }
}

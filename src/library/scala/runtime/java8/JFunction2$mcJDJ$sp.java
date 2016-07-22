
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction2$mcJDJ$sp extends scala.Function2, java.io.Serializable {
    long apply$mcJDJ$sp(double v1, long v2);

    default Object apply(Object v1, Object v2) { return scala.runtime.BoxesRunTime.boxToLong(apply$mcJDJ$sp(scala.runtime.BoxesRunTime.unboxToDouble(v1), scala.runtime.BoxesRunTime.unboxToLong(v2))); }
}

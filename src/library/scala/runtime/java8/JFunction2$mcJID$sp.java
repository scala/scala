
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction2$mcJID$sp extends scala.Function2, java.io.Serializable {
    long apply$mcJID$sp(int v1, double v2);

    default Object apply(Object v1, Object v2) { return scala.runtime.BoxesRunTime.boxToLong(apply$mcJID$sp(scala.runtime.BoxesRunTime.unboxToInt(v1), scala.runtime.BoxesRunTime.unboxToDouble(v2))); }
}

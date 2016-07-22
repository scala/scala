
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction2$mcFDI$sp extends scala.Function2, java.io.Serializable {
    float apply$mcFDI$sp(double v1, int v2);

    default Object apply(Object v1, Object v2) { return scala.runtime.BoxesRunTime.boxToFloat(apply$mcFDI$sp(scala.runtime.BoxesRunTime.unboxToDouble(v1), scala.runtime.BoxesRunTime.unboxToInt(v2))); }
}

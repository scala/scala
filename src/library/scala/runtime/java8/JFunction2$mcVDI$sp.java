
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction2$mcVDI$sp extends JFunction2 {
    void apply$mcVDI$sp(double v1, int v2);

    default Object apply(Object v1, Object v2) { apply$mcVDI$sp(scala.runtime.BoxesRunTime.unboxToDouble(v1), scala.runtime.BoxesRunTime.unboxToInt(v2)); return scala.runtime.BoxedUnit.UNIT; }
}

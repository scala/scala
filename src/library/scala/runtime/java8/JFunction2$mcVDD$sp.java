
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction2$mcVDD$sp extends JFunction2 {
    void apply$mcVDD$sp(double v1, double v2);

    default Object apply(Object v1, Object v2) { apply$mcVDD$sp(scala.runtime.BoxesRunTime.unboxToDouble(v1), scala.runtime.BoxesRunTime.unboxToDouble(v2)); return scala.runtime.BoxedUnit.UNIT; }
}

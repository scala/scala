
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction2$mcDII$sp extends JFunction2 {
    double apply$mcDII$sp(int v1, int v2);

    default Object apply(Object v1, Object v2) { return scala.runtime.BoxesRunTime.boxToDouble(apply$mcDII$sp(scala.runtime.BoxesRunTime.unboxToInt(v1), scala.runtime.BoxesRunTime.unboxToInt(v2))); }
}

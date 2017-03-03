
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction1$mcDI$sp extends scala.Function1, java.io.Serializable {
    double apply$mcDI$sp(int v1);

    default Object apply(Object t) { return scala.runtime.BoxesRunTime.boxToDouble(apply$mcDI$sp(scala.runtime.BoxesRunTime.unboxToInt(t))); }
}


/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction1$mcDD$sp extends scala.Function1, java.io.Serializable {
    double apply$mcDD$sp(double v1);

    default Object apply(Object t) { return scala.runtime.BoxesRunTime.boxToDouble(apply$mcDD$sp(scala.runtime.BoxesRunTime.unboxToDouble(t))); }
}

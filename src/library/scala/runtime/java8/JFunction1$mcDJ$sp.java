
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction1$mcDJ$sp extends scala.Function1, java.io.Serializable {
    double apply$mcDJ$sp(long v1);

    default Object apply(Object t) { return scala.runtime.BoxesRunTime.boxToDouble(apply$mcDJ$sp(scala.runtime.BoxesRunTime.unboxToLong(t))); }
}

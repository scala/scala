
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction1$mcIJ$sp extends scala.Function1, java.io.Serializable {
    int apply$mcIJ$sp(long v1);

    default Object apply(Object t) { return scala.runtime.BoxesRunTime.boxToInteger(apply$mcIJ$sp(scala.runtime.BoxesRunTime.unboxToLong(t))); }
}

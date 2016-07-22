
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction1$mcII$sp extends scala.Function1, java.io.Serializable {
    int apply$mcII$sp(int v1);

    default Object apply(Object t) { return scala.runtime.BoxesRunTime.boxToInteger(apply$mcII$sp(scala.runtime.BoxesRunTime.unboxToInt(t))); }
}

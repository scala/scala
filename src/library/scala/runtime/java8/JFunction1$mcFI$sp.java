
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction1$mcFI$sp extends scala.Function1, java.io.Serializable {
    float apply$mcFI$sp(int v1);

    default Object apply(Object t) { return scala.runtime.BoxesRunTime.boxToFloat(apply$mcFI$sp(scala.runtime.BoxesRunTime.unboxToInt(t))); }
}

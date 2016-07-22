
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction1$mcVJ$sp extends scala.Function1, java.io.Serializable {
    void apply$mcVJ$sp(long v1);

    default Object apply(Object t) { apply$mcVJ$sp(scala.runtime.BoxesRunTime.unboxToLong(t)); return scala.runtime.BoxedUnit.UNIT; }
}

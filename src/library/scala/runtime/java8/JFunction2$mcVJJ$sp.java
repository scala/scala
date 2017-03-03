
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction2$mcVJJ$sp extends scala.Function2, java.io.Serializable {
    void apply$mcVJJ$sp(long v1, long v2);

    default Object apply(Object v1, Object v2) { apply$mcVJJ$sp(scala.runtime.BoxesRunTime.unboxToLong(v1), scala.runtime.BoxesRunTime.unboxToLong(v2)); return scala.runtime.BoxedUnit.UNIT; }
}


/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction1$mcVF$sp extends scala.Function1, java.io.Serializable {
    void apply$mcVF$sp(float v1);

    default Object apply(Object t) { apply$mcVF$sp(scala.runtime.BoxesRunTime.unboxToFloat(t)); return scala.runtime.BoxedUnit.UNIT; }
}

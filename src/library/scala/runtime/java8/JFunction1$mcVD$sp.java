
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction1$mcVD$sp extends scala.Function1, java.io.Serializable {
    void apply$mcVD$sp(double v1);

    default Object apply(Object t) { apply$mcVD$sp(scala.runtime.BoxesRunTime.unboxToDouble(t)); return scala.runtime.BoxedUnit.UNIT; }
}

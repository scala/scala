
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction1$mcZF$sp extends JFunction1 {
    boolean apply$mcZF$sp(float v1);

    default Object apply(Object t) { return scala.runtime.BoxesRunTime.boxToBoolean(apply$mcZF$sp(scala.runtime.BoxesRunTime.unboxToFloat(t))); }
}


/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction1$mcZD$sp extends JFunction1 {
    boolean apply$mcZD$sp(double v1);

    default Object apply(Object t) { return scala.runtime.BoxesRunTime.boxToBoolean(apply$mcZD$sp(scala.runtime.BoxesRunTime.unboxToDouble(t))); }
}

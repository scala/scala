
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction1$mcZJ$sp extends JFunction1 {
    boolean apply$mcZJ$sp(long v1);

    default Object apply(Object t) { return scala.runtime.BoxesRunTime.boxToBoolean(apply$mcZJ$sp(scala.runtime.BoxesRunTime.unboxToLong(t))); }
}

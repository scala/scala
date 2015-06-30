
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction1$mcFF$sp extends JFunction1 {
    float apply$mcFF$sp(float v1);

    default Object apply(Object t) { return scala.runtime.BoxesRunTime.boxToFloat(apply$mcFF$sp(scala.runtime.BoxesRunTime.unboxToFloat(t))); }
}

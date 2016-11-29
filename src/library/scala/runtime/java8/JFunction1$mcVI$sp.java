
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction1$mcVI$sp extends scala.Function1, java.io.Serializable {
    void apply$mcVI$sp(int v1);

    default Object apply(Object t) { apply$mcVI$sp(scala.runtime.BoxesRunTime.unboxToInt(t)); return scala.runtime.BoxedUnit.UNIT; }
}

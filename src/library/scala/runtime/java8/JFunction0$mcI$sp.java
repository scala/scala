
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction0$mcI$sp extends scala.Function0, java.io.Serializable {
    int apply$mcI$sp();

    default Object apply() { return scala.runtime.BoxesRunTime.boxToInteger(apply$mcI$sp()); }
}

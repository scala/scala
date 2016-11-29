
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction0$mcS$sp extends scala.Function0, java.io.Serializable {
    short apply$mcS$sp();

    default Object apply() { return scala.runtime.BoxesRunTime.boxToShort(apply$mcS$sp()); }
}

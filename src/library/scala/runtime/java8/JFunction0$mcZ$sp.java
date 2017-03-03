
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction0$mcZ$sp extends scala.Function0, java.io.Serializable {
    boolean apply$mcZ$sp();

    default Object apply() { return scala.runtime.BoxesRunTime.boxToBoolean(apply$mcZ$sp()); }
}

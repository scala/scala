
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction0$mcD$sp extends scala.Function0, java.io.Serializable {
    double apply$mcD$sp();

    default Object apply() { return scala.runtime.BoxesRunTime.boxToDouble(apply$mcD$sp()); }
}

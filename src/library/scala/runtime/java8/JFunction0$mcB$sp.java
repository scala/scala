
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction0$mcB$sp extends scala.Function0, java.io.Serializable {
    byte apply$mcB$sp();

    default Object apply() { return scala.runtime.BoxesRunTime.boxToByte(apply$mcB$sp()); }
}

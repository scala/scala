
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction0$mcJ$sp extends JFunction0 {
    long apply$mcJ$sp();

    default Object apply() { return scala.runtime.BoxesRunTime.boxToLong(apply$mcJ$sp()); }
}

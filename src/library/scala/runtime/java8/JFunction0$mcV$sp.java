
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction0$mcV$sp extends JFunction0 {
    void apply$mcV$sp();

    default Object apply() { apply$mcV$sp(); return scala.runtime.BoxedUnit.UNIT; }
}


/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction0$mcC$sp extends JFunction0 {
    char apply$mcC$sp();

    default Object apply() { return scala.runtime.BoxesRunTime.boxToCharacter(apply$mcC$sp()); }
}

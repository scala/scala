
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction0$mcF$sp extends scala.Function0, java.io.Serializable {
    float apply$mcF$sp();

    default Object apply() { return scala.runtime.BoxesRunTime.boxToFloat(apply$mcF$sp()); }
}

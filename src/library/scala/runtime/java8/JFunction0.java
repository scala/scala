
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction0<R> extends scala.Function0<R> {
    default void $init$() {
    };
    default void apply$mcV$sp() {
        apply();
    }
    default byte apply$mcB$sp() {
        return scala.runtime.BoxesRunTime.unboxToByte(apply());
    }
    default short apply$mcS$sp() {
        return scala.runtime.BoxesRunTime.unboxToShort(apply());
    }
    default int apply$mcI$sp() {
        return scala.runtime.BoxesRunTime.unboxToInt(apply());
    }
    default long apply$mcJ$sp() {
        return scala.runtime.BoxesRunTime.unboxToLong(apply());
    }
    default char apply$mcC$sp() {
        return scala.runtime.BoxesRunTime.unboxToChar(apply());
    }
    default float apply$mcF$sp() {
        return scala.runtime.BoxesRunTime.unboxToFloat(apply());
    }
    default double apply$mcD$sp() {
        return scala.runtime.BoxesRunTime.unboxToDouble(apply());
    }
    default boolean apply$mcZ$sp() {
        return scala.runtime.BoxesRunTime.unboxToBoolean(apply());
    }
}

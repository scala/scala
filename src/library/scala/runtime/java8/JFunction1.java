
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction1<T1, R> extends scala.Function1<T1, R> {
    default void $init$() {
    };

    @Override
    default <A> scala.Function1<T1, A> andThen(scala.Function1<R, A> g) {
        return scala.Function1$class.andThen(this, g);
    }

    @Override
    default <A> scala.Function1<A, R> compose(scala.Function1<A, T1> g) {
        return scala.Function1$class.compose(this, g);
    }
    default void apply$mcVI$sp(int v1) {
        apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1));
    }
    default boolean apply$mcZI$sp(int v1) {
        return scala.runtime.BoxesRunTime.unboxToBoolean(apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1)));
    }
    default int apply$mcII$sp(int v1) {
        return scala.runtime.BoxesRunTime.unboxToInt(apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1)));
    }
    default float apply$mcFI$sp(int v1) {
        return scala.runtime.BoxesRunTime.unboxToFloat(apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1)));
    }
    default long apply$mcJI$sp(int v1) {
        return scala.runtime.BoxesRunTime.unboxToLong(apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1)));
    }
    default double apply$mcDI$sp(int v1) {
        return scala.runtime.BoxesRunTime.unboxToDouble(apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1)));
    }
    default void apply$mcVJ$sp(long v1) {
        apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1));
    }
    default boolean apply$mcZJ$sp(long v1) {
        return scala.runtime.BoxesRunTime.unboxToBoolean(apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1)));
    }
    default int apply$mcIJ$sp(long v1) {
        return scala.runtime.BoxesRunTime.unboxToInt(apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1)));
    }
    default float apply$mcFJ$sp(long v1) {
        return scala.runtime.BoxesRunTime.unboxToFloat(apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1)));
    }
    default long apply$mcJJ$sp(long v1) {
        return scala.runtime.BoxesRunTime.unboxToLong(apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1)));
    }
    default double apply$mcDJ$sp(long v1) {
        return scala.runtime.BoxesRunTime.unboxToDouble(apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1)));
    }
    default void apply$mcVF$sp(float v1) {
        apply((T1) scala.runtime.BoxesRunTime.boxToFloat(v1));
    }
    default boolean apply$mcZF$sp(float v1) {
        return scala.runtime.BoxesRunTime.unboxToBoolean(apply((T1) scala.runtime.BoxesRunTime.boxToFloat(v1)));
    }
    default int apply$mcIF$sp(float v1) {
        return scala.runtime.BoxesRunTime.unboxToInt(apply((T1) scala.runtime.BoxesRunTime.boxToFloat(v1)));
    }
    default float apply$mcFF$sp(float v1) {
        return scala.runtime.BoxesRunTime.unboxToFloat(apply((T1) scala.runtime.BoxesRunTime.boxToFloat(v1)));
    }
    default long apply$mcJF$sp(float v1) {
        return scala.runtime.BoxesRunTime.unboxToLong(apply((T1) scala.runtime.BoxesRunTime.boxToFloat(v1)));
    }
    default double apply$mcDF$sp(float v1) {
        return scala.runtime.BoxesRunTime.unboxToDouble(apply((T1) scala.runtime.BoxesRunTime.boxToFloat(v1)));
    }
    default void apply$mcVD$sp(double v1) {
        apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1));
    }
    default boolean apply$mcZD$sp(double v1) {
        return scala.runtime.BoxesRunTime.unboxToBoolean(apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1)));
    }
    default int apply$mcID$sp(double v1) {
        return scala.runtime.BoxesRunTime.unboxToInt(apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1)));
    }
    default float apply$mcFD$sp(double v1) {
        return scala.runtime.BoxesRunTime.unboxToFloat(apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1)));
    }
    default long apply$mcJD$sp(double v1) {
        return scala.runtime.BoxesRunTime.unboxToLong(apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1)));
    }
    default double apply$mcDD$sp(double v1) {
        return scala.runtime.BoxesRunTime.unboxToDouble(apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1)));
    }
    
    default scala.Function1 compose$mcVI$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcZI$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcII$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcFI$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcJI$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcDI$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcVJ$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcZJ$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcIJ$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcFJ$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcJJ$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcDJ$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcVF$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcZF$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcIF$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcFF$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcJF$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcDF$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcVD$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcZD$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcID$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcFD$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcJD$sp(scala.Function1 g) {
        return compose(g);
    }
    default scala.Function1 compose$mcDD$sp(scala.Function1 g) {
        return compose(g);
    }
    
    default scala.Function1 andThen$mcVI$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcZI$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcII$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcFI$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcJI$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcDI$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcVJ$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcZJ$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcIJ$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcFJ$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcJJ$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcDJ$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcVF$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcZF$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcIF$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcFF$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcJF$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcDF$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcVD$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcZD$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcID$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcFD$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcJD$sp(scala.Function1 g) {
        return andThen(g);
    }
    default scala.Function1 andThen$mcDD$sp(scala.Function1 g) {
        return andThen(g);
    }
}

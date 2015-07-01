
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

@FunctionalInterface
public interface JFunction2<T1, T2, R> extends scala.Function2<T1, T2, R> {
    default void $init$() {
    };

    default scala.Function1<T1, scala.Function1<T2, R>> curried() {
      return scala.Function2$class.curried(this);
    }

    default scala.Function1<scala.Tuple2<T1, T2>, R> tupled() {
      return scala.Function2$class.tupled(this);
    }

    default void apply$mcVII$sp(int v1, int v2) {
        apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1), (T2) scala.runtime.BoxesRunTime.boxToInteger(v2));
    }
    default boolean apply$mcZII$sp(int v1, int v2) {
        return scala.runtime.BoxesRunTime.unboxToBoolean(apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1), (T2) scala.runtime.BoxesRunTime.boxToInteger(v2)));
    }
    default int apply$mcIII$sp(int v1, int v2) {
        return scala.runtime.BoxesRunTime.unboxToInt(apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1), (T2) scala.runtime.BoxesRunTime.boxToInteger(v2)));
    }
    default float apply$mcFII$sp(int v1, int v2) {
        return scala.runtime.BoxesRunTime.unboxToFloat(apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1), (T2) scala.runtime.BoxesRunTime.boxToInteger(v2)));
    }
    default long apply$mcJII$sp(int v1, int v2) {
        return scala.runtime.BoxesRunTime.unboxToLong(apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1), (T2) scala.runtime.BoxesRunTime.boxToInteger(v2)));
    }
    default double apply$mcDII$sp(int v1, int v2) {
        return scala.runtime.BoxesRunTime.unboxToDouble(apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1), (T2) scala.runtime.BoxesRunTime.boxToInteger(v2)));
    }
    default void apply$mcVIJ$sp(int v1, long v2) {
        apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1), (T2) scala.runtime.BoxesRunTime.boxToLong(v2));
    }
    default boolean apply$mcZIJ$sp(int v1, long v2) {
        return scala.runtime.BoxesRunTime.unboxToBoolean(apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1), (T2) scala.runtime.BoxesRunTime.boxToLong(v2)));
    }
    default int apply$mcIIJ$sp(int v1, long v2) {
        return scala.runtime.BoxesRunTime.unboxToInt(apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1), (T2) scala.runtime.BoxesRunTime.boxToLong(v2)));
    }
    default float apply$mcFIJ$sp(int v1, long v2) {
        return scala.runtime.BoxesRunTime.unboxToFloat(apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1), (T2) scala.runtime.BoxesRunTime.boxToLong(v2)));
    }
    default long apply$mcJIJ$sp(int v1, long v2) {
        return scala.runtime.BoxesRunTime.unboxToLong(apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1), (T2) scala.runtime.BoxesRunTime.boxToLong(v2)));
    }
    default double apply$mcDIJ$sp(int v1, long v2) {
        return scala.runtime.BoxesRunTime.unboxToDouble(apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1), (T2) scala.runtime.BoxesRunTime.boxToLong(v2)));
    }
    default void apply$mcVID$sp(int v1, double v2) {
        apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1), (T2) scala.runtime.BoxesRunTime.boxToDouble(v2));
    }
    default boolean apply$mcZID$sp(int v1, double v2) {
        return scala.runtime.BoxesRunTime.unboxToBoolean(apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1), (T2) scala.runtime.BoxesRunTime.boxToDouble(v2)));
    }
    default int apply$mcIID$sp(int v1, double v2) {
        return scala.runtime.BoxesRunTime.unboxToInt(apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1), (T2) scala.runtime.BoxesRunTime.boxToDouble(v2)));
    }
    default float apply$mcFID$sp(int v1, double v2) {
        return scala.runtime.BoxesRunTime.unboxToFloat(apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1), (T2) scala.runtime.BoxesRunTime.boxToDouble(v2)));
    }
    default long apply$mcJID$sp(int v1, double v2) {
        return scala.runtime.BoxesRunTime.unboxToLong(apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1), (T2) scala.runtime.BoxesRunTime.boxToDouble(v2)));
    }
    default double apply$mcDID$sp(int v1, double v2) {
        return scala.runtime.BoxesRunTime.unboxToDouble(apply((T1) scala.runtime.BoxesRunTime.boxToInteger(v1), (T2) scala.runtime.BoxesRunTime.boxToDouble(v2)));
    }
    default void apply$mcVJI$sp(long v1, int v2) {
        apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1), (T2) scala.runtime.BoxesRunTime.boxToInteger(v2));
    }
    default boolean apply$mcZJI$sp(long v1, int v2) {
        return scala.runtime.BoxesRunTime.unboxToBoolean(apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1), (T2) scala.runtime.BoxesRunTime.boxToInteger(v2)));
    }
    default int apply$mcIJI$sp(long v1, int v2) {
        return scala.runtime.BoxesRunTime.unboxToInt(apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1), (T2) scala.runtime.BoxesRunTime.boxToInteger(v2)));
    }
    default float apply$mcFJI$sp(long v1, int v2) {
        return scala.runtime.BoxesRunTime.unboxToFloat(apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1), (T2) scala.runtime.BoxesRunTime.boxToInteger(v2)));
    }
    default long apply$mcJJI$sp(long v1, int v2) {
        return scala.runtime.BoxesRunTime.unboxToLong(apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1), (T2) scala.runtime.BoxesRunTime.boxToInteger(v2)));
    }
    default double apply$mcDJI$sp(long v1, int v2) {
        return scala.runtime.BoxesRunTime.unboxToDouble(apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1), (T2) scala.runtime.BoxesRunTime.boxToInteger(v2)));
    }
    default void apply$mcVJJ$sp(long v1, long v2) {
        apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1), (T2) scala.runtime.BoxesRunTime.boxToLong(v2));
    }
    default boolean apply$mcZJJ$sp(long v1, long v2) {
        return scala.runtime.BoxesRunTime.unboxToBoolean(apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1), (T2) scala.runtime.BoxesRunTime.boxToLong(v2)));
    }
    default int apply$mcIJJ$sp(long v1, long v2) {
        return scala.runtime.BoxesRunTime.unboxToInt(apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1), (T2) scala.runtime.BoxesRunTime.boxToLong(v2)));
    }
    default float apply$mcFJJ$sp(long v1, long v2) {
        return scala.runtime.BoxesRunTime.unboxToFloat(apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1), (T2) scala.runtime.BoxesRunTime.boxToLong(v2)));
    }
    default long apply$mcJJJ$sp(long v1, long v2) {
        return scala.runtime.BoxesRunTime.unboxToLong(apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1), (T2) scala.runtime.BoxesRunTime.boxToLong(v2)));
    }
    default double apply$mcDJJ$sp(long v1, long v2) {
        return scala.runtime.BoxesRunTime.unboxToDouble(apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1), (T2) scala.runtime.BoxesRunTime.boxToLong(v2)));
    }
    default void apply$mcVJD$sp(long v1, double v2) {
        apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1), (T2) scala.runtime.BoxesRunTime.boxToDouble(v2));
    }
    default boolean apply$mcZJD$sp(long v1, double v2) {
        return scala.runtime.BoxesRunTime.unboxToBoolean(apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1), (T2) scala.runtime.BoxesRunTime.boxToDouble(v2)));
    }
    default int apply$mcIJD$sp(long v1, double v2) {
        return scala.runtime.BoxesRunTime.unboxToInt(apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1), (T2) scala.runtime.BoxesRunTime.boxToDouble(v2)));
    }
    default float apply$mcFJD$sp(long v1, double v2) {
        return scala.runtime.BoxesRunTime.unboxToFloat(apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1), (T2) scala.runtime.BoxesRunTime.boxToDouble(v2)));
    }
    default long apply$mcJJD$sp(long v1, double v2) {
        return scala.runtime.BoxesRunTime.unboxToLong(apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1), (T2) scala.runtime.BoxesRunTime.boxToDouble(v2)));
    }
    default double apply$mcDJD$sp(long v1, double v2) {
        return scala.runtime.BoxesRunTime.unboxToDouble(apply((T1) scala.runtime.BoxesRunTime.boxToLong(v1), (T2) scala.runtime.BoxesRunTime.boxToDouble(v2)));
    }
    default void apply$mcVDI$sp(double v1, int v2) {
        apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1), (T2) scala.runtime.BoxesRunTime.boxToInteger(v2));
    }
    default boolean apply$mcZDI$sp(double v1, int v2) {
        return scala.runtime.BoxesRunTime.unboxToBoolean(apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1), (T2) scala.runtime.BoxesRunTime.boxToInteger(v2)));
    }
    default int apply$mcIDI$sp(double v1, int v2) {
        return scala.runtime.BoxesRunTime.unboxToInt(apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1), (T2) scala.runtime.BoxesRunTime.boxToInteger(v2)));
    }
    default float apply$mcFDI$sp(double v1, int v2) {
        return scala.runtime.BoxesRunTime.unboxToFloat(apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1), (T2) scala.runtime.BoxesRunTime.boxToInteger(v2)));
    }
    default long apply$mcJDI$sp(double v1, int v2) {
        return scala.runtime.BoxesRunTime.unboxToLong(apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1), (T2) scala.runtime.BoxesRunTime.boxToInteger(v2)));
    }
    default double apply$mcDDI$sp(double v1, int v2) {
        return scala.runtime.BoxesRunTime.unboxToDouble(apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1), (T2) scala.runtime.BoxesRunTime.boxToInteger(v2)));
    }
    default void apply$mcVDJ$sp(double v1, long v2) {
        apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1), (T2) scala.runtime.BoxesRunTime.boxToLong(v2));
    }
    default boolean apply$mcZDJ$sp(double v1, long v2) {
        return scala.runtime.BoxesRunTime.unboxToBoolean(apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1), (T2) scala.runtime.BoxesRunTime.boxToLong(v2)));
    }
    default int apply$mcIDJ$sp(double v1, long v2) {
        return scala.runtime.BoxesRunTime.unboxToInt(apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1), (T2) scala.runtime.BoxesRunTime.boxToLong(v2)));
    }
    default float apply$mcFDJ$sp(double v1, long v2) {
        return scala.runtime.BoxesRunTime.unboxToFloat(apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1), (T2) scala.runtime.BoxesRunTime.boxToLong(v2)));
    }
    default long apply$mcJDJ$sp(double v1, long v2) {
        return scala.runtime.BoxesRunTime.unboxToLong(apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1), (T2) scala.runtime.BoxesRunTime.boxToLong(v2)));
    }
    default double apply$mcDDJ$sp(double v1, long v2) {
        return scala.runtime.BoxesRunTime.unboxToDouble(apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1), (T2) scala.runtime.BoxesRunTime.boxToLong(v2)));
    }
    default void apply$mcVDD$sp(double v1, double v2) {
        apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1), (T2) scala.runtime.BoxesRunTime.boxToDouble(v2));
    }
    default boolean apply$mcZDD$sp(double v1, double v2) {
        return scala.runtime.BoxesRunTime.unboxToBoolean(apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1), (T2) scala.runtime.BoxesRunTime.boxToDouble(v2)));
    }
    default int apply$mcIDD$sp(double v1, double v2) {
        return scala.runtime.BoxesRunTime.unboxToInt(apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1), (T2) scala.runtime.BoxesRunTime.boxToDouble(v2)));
    }
    default float apply$mcFDD$sp(double v1, double v2) {
        return scala.runtime.BoxesRunTime.unboxToFloat(apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1), (T2) scala.runtime.BoxesRunTime.boxToDouble(v2)));
    }
    default long apply$mcJDD$sp(double v1, double v2) {
        return scala.runtime.BoxesRunTime.unboxToLong(apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1), (T2) scala.runtime.BoxesRunTime.boxToDouble(v2)));
    }
    default double apply$mcDDD$sp(double v1, double v2) {
        return scala.runtime.BoxesRunTime.unboxToDouble(apply((T1) scala.runtime.BoxesRunTime.boxToDouble(v1), (T2) scala.runtime.BoxesRunTime.boxToDouble(v2)));
    }
    
    default scala.Function1 curried$mcVII$sp() {
        return curried();
    }
    default scala.Function1 curried$mcZII$sp() {
        return curried();
    }
    default scala.Function1 curried$mcIII$sp() {
        return curried();
    }
    default scala.Function1 curried$mcFII$sp() {
        return curried();
    }
    default scala.Function1 curried$mcJII$sp() {
        return curried();
    }
    default scala.Function1 curried$mcDII$sp() {
        return curried();
    }
    default scala.Function1 curried$mcVIJ$sp() {
        return curried();
    }
    default scala.Function1 curried$mcZIJ$sp() {
        return curried();
    }
    default scala.Function1 curried$mcIIJ$sp() {
        return curried();
    }
    default scala.Function1 curried$mcFIJ$sp() {
        return curried();
    }
    default scala.Function1 curried$mcJIJ$sp() {
        return curried();
    }
    default scala.Function1 curried$mcDIJ$sp() {
        return curried();
    }
    default scala.Function1 curried$mcVID$sp() {
        return curried();
    }
    default scala.Function1 curried$mcZID$sp() {
        return curried();
    }
    default scala.Function1 curried$mcIID$sp() {
        return curried();
    }
    default scala.Function1 curried$mcFID$sp() {
        return curried();
    }
    default scala.Function1 curried$mcJID$sp() {
        return curried();
    }
    default scala.Function1 curried$mcDID$sp() {
        return curried();
    }
    default scala.Function1 curried$mcVJI$sp() {
        return curried();
    }
    default scala.Function1 curried$mcZJI$sp() {
        return curried();
    }
    default scala.Function1 curried$mcIJI$sp() {
        return curried();
    }
    default scala.Function1 curried$mcFJI$sp() {
        return curried();
    }
    default scala.Function1 curried$mcJJI$sp() {
        return curried();
    }
    default scala.Function1 curried$mcDJI$sp() {
        return curried();
    }
    default scala.Function1 curried$mcVJJ$sp() {
        return curried();
    }
    default scala.Function1 curried$mcZJJ$sp() {
        return curried();
    }
    default scala.Function1 curried$mcIJJ$sp() {
        return curried();
    }
    default scala.Function1 curried$mcFJJ$sp() {
        return curried();
    }
    default scala.Function1 curried$mcJJJ$sp() {
        return curried();
    }
    default scala.Function1 curried$mcDJJ$sp() {
        return curried();
    }
    default scala.Function1 curried$mcVJD$sp() {
        return curried();
    }
    default scala.Function1 curried$mcZJD$sp() {
        return curried();
    }
    default scala.Function1 curried$mcIJD$sp() {
        return curried();
    }
    default scala.Function1 curried$mcFJD$sp() {
        return curried();
    }
    default scala.Function1 curried$mcJJD$sp() {
        return curried();
    }
    default scala.Function1 curried$mcDJD$sp() {
        return curried();
    }
    default scala.Function1 curried$mcVDI$sp() {
        return curried();
    }
    default scala.Function1 curried$mcZDI$sp() {
        return curried();
    }
    default scala.Function1 curried$mcIDI$sp() {
        return curried();
    }
    default scala.Function1 curried$mcFDI$sp() {
        return curried();
    }
    default scala.Function1 curried$mcJDI$sp() {
        return curried();
    }
    default scala.Function1 curried$mcDDI$sp() {
        return curried();
    }
    default scala.Function1 curried$mcVDJ$sp() {
        return curried();
    }
    default scala.Function1 curried$mcZDJ$sp() {
        return curried();
    }
    default scala.Function1 curried$mcIDJ$sp() {
        return curried();
    }
    default scala.Function1 curried$mcFDJ$sp() {
        return curried();
    }
    default scala.Function1 curried$mcJDJ$sp() {
        return curried();
    }
    default scala.Function1 curried$mcDDJ$sp() {
        return curried();
    }
    default scala.Function1 curried$mcVDD$sp() {
        return curried();
    }
    default scala.Function1 curried$mcZDD$sp() {
        return curried();
    }
    default scala.Function1 curried$mcIDD$sp() {
        return curried();
    }
    default scala.Function1 curried$mcFDD$sp() {
        return curried();
    }
    default scala.Function1 curried$mcJDD$sp() {
        return curried();
    }
    default scala.Function1 curried$mcDDD$sp() {
        return curried();
    }
    
    default scala.Function1 tupled$mcVII$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcZII$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcIII$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcFII$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcJII$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcDII$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcVIJ$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcZIJ$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcIIJ$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcFIJ$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcJIJ$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcDIJ$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcVID$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcZID$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcIID$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcFID$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcJID$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcDID$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcVJI$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcZJI$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcIJI$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcFJI$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcJJI$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcDJI$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcVJJ$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcZJJ$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcIJJ$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcFJJ$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcJJJ$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcDJJ$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcVJD$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcZJD$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcIJD$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcFJD$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcJJD$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcDJD$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcVDI$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcZDI$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcIDI$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcFDI$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcJDI$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcDDI$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcVDJ$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcZDJ$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcIDJ$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcFDJ$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcJDJ$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcDDJ$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcVDD$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcZDD$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcIDD$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcFDD$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcJDD$sp() {
        return tupled();
    }
    default scala.Function1 tupled$mcDDD$sp() {
        return tupled();
    }
}

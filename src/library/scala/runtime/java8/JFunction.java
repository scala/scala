
/*
 * Copyright (C) 2012-2015 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.java8;

import scala.runtime.BoxedUnit;

public final class JFunction {
    private JFunction() {}
    public static <R> scala.Function0<R> func(JFunction0<R> f) { return f; }
    public static scala.Function0<BoxedUnit> proc(JProcedure0 p) { return p; }
    public static scala.Function0<BoxedUnit> procSpecialized(JFunction0$mcV$sp f) { return f; }
    public static scala.Function0<Byte> funcSpecialized(JFunction0$mcB$sp f) { return f; }
    public static scala.Function0<Short> funcSpecialized(JFunction0$mcS$sp f) { return f; }
    public static scala.Function0<Integer> funcSpecialized(JFunction0$mcI$sp f) { return f; }
    public static scala.Function0<Long> funcSpecialized(JFunction0$mcJ$sp f) { return f; }
    public static scala.Function0<Character> funcSpecialized(JFunction0$mcC$sp f) { return f; }
    public static scala.Function0<Float> funcSpecialized(JFunction0$mcF$sp f) { return f; }
    public static scala.Function0<Double> funcSpecialized(JFunction0$mcD$sp f) { return f; }
    public static scala.Function0<Boolean> funcSpecialized(JFunction0$mcZ$sp f) { return f; }
    public static <T1, R> scala.Function1<T1, R> func(JFunction1<T1, R> f) { return f; }
    public static <T1> scala.Function1<T1, BoxedUnit> proc(JProcedure1<T1> p) { return p; }
    public static scala.Function1<Integer, BoxedUnit> procSpecialized(JFunction1$mcVI$sp f) { return f; }
    public static scala.Function1<Integer, Boolean> funcSpecialized(JFunction1$mcZI$sp f) { return f; }
    public static scala.Function1<Integer, Integer> funcSpecialized(JFunction1$mcII$sp f) { return f; }
    public static scala.Function1<Integer, Float> funcSpecialized(JFunction1$mcFI$sp f) { return f; }
    public static scala.Function1<Integer, Long> funcSpecialized(JFunction1$mcJI$sp f) { return f; }
    public static scala.Function1<Integer, Double> funcSpecialized(JFunction1$mcDI$sp f) { return f; }
    public static scala.Function1<Long, BoxedUnit> procSpecialized(JFunction1$mcVJ$sp f) { return f; }
    public static scala.Function1<Long, Boolean> funcSpecialized(JFunction1$mcZJ$sp f) { return f; }
    public static scala.Function1<Long, Integer> funcSpecialized(JFunction1$mcIJ$sp f) { return f; }
    public static scala.Function1<Long, Float> funcSpecialized(JFunction1$mcFJ$sp f) { return f; }
    public static scala.Function1<Long, Long> funcSpecialized(JFunction1$mcJJ$sp f) { return f; }
    public static scala.Function1<Long, Double> funcSpecialized(JFunction1$mcDJ$sp f) { return f; }
    public static scala.Function1<Float, BoxedUnit> procSpecialized(JFunction1$mcVF$sp f) { return f; }
    public static scala.Function1<Float, Boolean> funcSpecialized(JFunction1$mcZF$sp f) { return f; }
    public static scala.Function1<Float, Integer> funcSpecialized(JFunction1$mcIF$sp f) { return f; }
    public static scala.Function1<Float, Float> funcSpecialized(JFunction1$mcFF$sp f) { return f; }
    public static scala.Function1<Float, Long> funcSpecialized(JFunction1$mcJF$sp f) { return f; }
    public static scala.Function1<Float, Double> funcSpecialized(JFunction1$mcDF$sp f) { return f; }
    public static scala.Function1<Double, BoxedUnit> procSpecialized(JFunction1$mcVD$sp f) { return f; }
    public static scala.Function1<Double, Boolean> funcSpecialized(JFunction1$mcZD$sp f) { return f; }
    public static scala.Function1<Double, Integer> funcSpecialized(JFunction1$mcID$sp f) { return f; }
    public static scala.Function1<Double, Float> funcSpecialized(JFunction1$mcFD$sp f) { return f; }
    public static scala.Function1<Double, Long> funcSpecialized(JFunction1$mcJD$sp f) { return f; }
    public static scala.Function1<Double, Double> funcSpecialized(JFunction1$mcDD$sp f) { return f; }
    public static <T1, T2, R> scala.Function2<T1, T2, R> func(JFunction2<T1, T2, R> f) { return f; }
    public static <T1, T2> scala.Function2<T1, T2, BoxedUnit> proc(JProcedure2<T1, T2> p) { return p; }
    public static scala.Function2<Integer, Integer, BoxedUnit> procSpecialized(JFunction2$mcVII$sp f) { return f; }
    public static scala.Function2<Integer, Integer, Boolean> funcSpecialized(JFunction2$mcZII$sp f) { return f; }
    public static scala.Function2<Integer, Integer, Integer> funcSpecialized(JFunction2$mcIII$sp f) { return f; }
    public static scala.Function2<Integer, Integer, Float> funcSpecialized(JFunction2$mcFII$sp f) { return f; }
    public static scala.Function2<Integer, Integer, Long> funcSpecialized(JFunction2$mcJII$sp f) { return f; }
    public static scala.Function2<Integer, Integer, Double> funcSpecialized(JFunction2$mcDII$sp f) { return f; }
    public static scala.Function2<Integer, Long, BoxedUnit> procSpecialized(JFunction2$mcVIJ$sp f) { return f; }
    public static scala.Function2<Integer, Long, Boolean> funcSpecialized(JFunction2$mcZIJ$sp f) { return f; }
    public static scala.Function2<Integer, Long, Integer> funcSpecialized(JFunction2$mcIIJ$sp f) { return f; }
    public static scala.Function2<Integer, Long, Float> funcSpecialized(JFunction2$mcFIJ$sp f) { return f; }
    public static scala.Function2<Integer, Long, Long> funcSpecialized(JFunction2$mcJIJ$sp f) { return f; }
    public static scala.Function2<Integer, Long, Double> funcSpecialized(JFunction2$mcDIJ$sp f) { return f; }
    public static scala.Function2<Integer, Double, BoxedUnit> procSpecialized(JFunction2$mcVID$sp f) { return f; }
    public static scala.Function2<Integer, Double, Boolean> funcSpecialized(JFunction2$mcZID$sp f) { return f; }
    public static scala.Function2<Integer, Double, Integer> funcSpecialized(JFunction2$mcIID$sp f) { return f; }
    public static scala.Function2<Integer, Double, Float> funcSpecialized(JFunction2$mcFID$sp f) { return f; }
    public static scala.Function2<Integer, Double, Long> funcSpecialized(JFunction2$mcJID$sp f) { return f; }
    public static scala.Function2<Integer, Double, Double> funcSpecialized(JFunction2$mcDID$sp f) { return f; }
    public static scala.Function2<Long, Integer, BoxedUnit> procSpecialized(JFunction2$mcVJI$sp f) { return f; }
    public static scala.Function2<Long, Integer, Boolean> funcSpecialized(JFunction2$mcZJI$sp f) { return f; }
    public static scala.Function2<Long, Integer, Integer> funcSpecialized(JFunction2$mcIJI$sp f) { return f; }
    public static scala.Function2<Long, Integer, Float> funcSpecialized(JFunction2$mcFJI$sp f) { return f; }
    public static scala.Function2<Long, Integer, Long> funcSpecialized(JFunction2$mcJJI$sp f) { return f; }
    public static scala.Function2<Long, Integer, Double> funcSpecialized(JFunction2$mcDJI$sp f) { return f; }
    public static scala.Function2<Long, Long, BoxedUnit> procSpecialized(JFunction2$mcVJJ$sp f) { return f; }
    public static scala.Function2<Long, Long, Boolean> funcSpecialized(JFunction2$mcZJJ$sp f) { return f; }
    public static scala.Function2<Long, Long, Integer> funcSpecialized(JFunction2$mcIJJ$sp f) { return f; }
    public static scala.Function2<Long, Long, Float> funcSpecialized(JFunction2$mcFJJ$sp f) { return f; }
    public static scala.Function2<Long, Long, Long> funcSpecialized(JFunction2$mcJJJ$sp f) { return f; }
    public static scala.Function2<Long, Long, Double> funcSpecialized(JFunction2$mcDJJ$sp f) { return f; }
    public static scala.Function2<Long, Double, BoxedUnit> procSpecialized(JFunction2$mcVJD$sp f) { return f; }
    public static scala.Function2<Long, Double, Boolean> funcSpecialized(JFunction2$mcZJD$sp f) { return f; }
    public static scala.Function2<Long, Double, Integer> funcSpecialized(JFunction2$mcIJD$sp f) { return f; }
    public static scala.Function2<Long, Double, Float> funcSpecialized(JFunction2$mcFJD$sp f) { return f; }
    public static scala.Function2<Long, Double, Long> funcSpecialized(JFunction2$mcJJD$sp f) { return f; }
    public static scala.Function2<Long, Double, Double> funcSpecialized(JFunction2$mcDJD$sp f) { return f; }
    public static scala.Function2<Double, Integer, BoxedUnit> procSpecialized(JFunction2$mcVDI$sp f) { return f; }
    public static scala.Function2<Double, Integer, Boolean> funcSpecialized(JFunction2$mcZDI$sp f) { return f; }
    public static scala.Function2<Double, Integer, Integer> funcSpecialized(JFunction2$mcIDI$sp f) { return f; }
    public static scala.Function2<Double, Integer, Float> funcSpecialized(JFunction2$mcFDI$sp f) { return f; }
    public static scala.Function2<Double, Integer, Long> funcSpecialized(JFunction2$mcJDI$sp f) { return f; }
    public static scala.Function2<Double, Integer, Double> funcSpecialized(JFunction2$mcDDI$sp f) { return f; }
    public static scala.Function2<Double, Long, BoxedUnit> procSpecialized(JFunction2$mcVDJ$sp f) { return f; }
    public static scala.Function2<Double, Long, Boolean> funcSpecialized(JFunction2$mcZDJ$sp f) { return f; }
    public static scala.Function2<Double, Long, Integer> funcSpecialized(JFunction2$mcIDJ$sp f) { return f; }
    public static scala.Function2<Double, Long, Float> funcSpecialized(JFunction2$mcFDJ$sp f) { return f; }
    public static scala.Function2<Double, Long, Long> funcSpecialized(JFunction2$mcJDJ$sp f) { return f; }
    public static scala.Function2<Double, Long, Double> funcSpecialized(JFunction2$mcDDJ$sp f) { return f; }
    public static scala.Function2<Double, Double, BoxedUnit> procSpecialized(JFunction2$mcVDD$sp f) { return f; }
    public static scala.Function2<Double, Double, Boolean> funcSpecialized(JFunction2$mcZDD$sp f) { return f; }
    public static scala.Function2<Double, Double, Integer> funcSpecialized(JFunction2$mcIDD$sp f) { return f; }
    public static scala.Function2<Double, Double, Float> funcSpecialized(JFunction2$mcFDD$sp f) { return f; }
    public static scala.Function2<Double, Double, Long> funcSpecialized(JFunction2$mcJDD$sp f) { return f; }
    public static scala.Function2<Double, Double, Double> funcSpecialized(JFunction2$mcDDD$sp f) { return f; }
    public static <T1, T2, T3, R> scala.Function3<T1, T2, T3, R> func(JFunction3<T1, T2, T3, R> f) { return f; }
    public static <T1, T2, T3> scala.Function3<T1, T2, T3, BoxedUnit> proc(JProcedure3<T1, T2, T3> p) { return p; }
    public static <T1, T2, T3, T4, R> scala.Function4<T1, T2, T3, T4, R> func(JFunction4<T1, T2, T3, T4, R> f) { return f; }
    public static <T1, T2, T3, T4> scala.Function4<T1, T2, T3, T4, BoxedUnit> proc(JProcedure4<T1, T2, T3, T4> p) { return p; }
    public static <T1, T2, T3, T4, T5, R> scala.Function5<T1, T2, T3, T4, T5, R> func(JFunction5<T1, T2, T3, T4, T5, R> f) { return f; }
    public static <T1, T2, T3, T4, T5> scala.Function5<T1, T2, T3, T4, T5, BoxedUnit> proc(JProcedure5<T1, T2, T3, T4, T5> p) { return p; }
    public static <T1, T2, T3, T4, T5, T6, R> scala.Function6<T1, T2, T3, T4, T5, T6, R> func(JFunction6<T1, T2, T3, T4, T5, T6, R> f) { return f; }
    public static <T1, T2, T3, T4, T5, T6> scala.Function6<T1, T2, T3, T4, T5, T6, BoxedUnit> proc(JProcedure6<T1, T2, T3, T4, T5, T6> p) { return p; }
    public static <T1, T2, T3, T4, T5, T6, T7, R> scala.Function7<T1, T2, T3, T4, T5, T6, T7, R> func(JFunction7<T1, T2, T3, T4, T5, T6, T7, R> f) { return f; }
    public static <T1, T2, T3, T4, T5, T6, T7> scala.Function7<T1, T2, T3, T4, T5, T6, T7, BoxedUnit> proc(JProcedure7<T1, T2, T3, T4, T5, T6, T7> p) { return p; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, R> scala.Function8<T1, T2, T3, T4, T5, T6, T7, T8, R> func(JFunction8<T1, T2, T3, T4, T5, T6, T7, T8, R> f) { return f; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8> scala.Function8<T1, T2, T3, T4, T5, T6, T7, T8, BoxedUnit> proc(JProcedure8<T1, T2, T3, T4, T5, T6, T7, T8> p) { return p; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, R> scala.Function9<T1, T2, T3, T4, T5, T6, T7, T8, T9, R> func(JFunction9<T1, T2, T3, T4, T5, T6, T7, T8, T9, R> f) { return f; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9> scala.Function9<T1, T2, T3, T4, T5, T6, T7, T8, T9, BoxedUnit> proc(JProcedure9<T1, T2, T3, T4, T5, T6, T7, T8, T9> p) { return p; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R> scala.Function10<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R> func(JFunction10<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R> f) { return f; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10> scala.Function10<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, BoxedUnit> proc(JProcedure10<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10> p) { return p; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R> scala.Function11<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R> func(JFunction11<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R> f) { return f; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11> scala.Function11<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, BoxedUnit> proc(JProcedure11<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11> p) { return p; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R> scala.Function12<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R> func(JFunction12<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R> f) { return f; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12> scala.Function12<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, BoxedUnit> proc(JProcedure12<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12> p) { return p; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R> scala.Function13<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R> func(JFunction13<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R> f) { return f; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13> scala.Function13<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, BoxedUnit> proc(JProcedure13<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13> p) { return p; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R> scala.Function14<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R> func(JFunction14<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R> f) { return f; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14> scala.Function14<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, BoxedUnit> proc(JProcedure14<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14> p) { return p; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R> scala.Function15<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R> func(JFunction15<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R> f) { return f; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15> scala.Function15<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, BoxedUnit> proc(JProcedure15<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15> p) { return p; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R> scala.Function16<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R> func(JFunction16<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R> f) { return f; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16> scala.Function16<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, BoxedUnit> proc(JProcedure16<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16> p) { return p; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R> scala.Function17<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R> func(JFunction17<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R> f) { return f; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17> scala.Function17<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, BoxedUnit> proc(JProcedure17<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17> p) { return p; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R> scala.Function18<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R> func(JFunction18<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R> f) { return f; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18> scala.Function18<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, BoxedUnit> proc(JProcedure18<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18> p) { return p; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R> scala.Function19<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R> func(JFunction19<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R> f) { return f; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19> scala.Function19<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, BoxedUnit> proc(JProcedure19<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19> p) { return p; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R> scala.Function20<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R> func(JFunction20<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R> f) { return f; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20> scala.Function20<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, BoxedUnit> proc(JProcedure20<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20> p) { return p; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R> scala.Function21<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R> func(JFunction21<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R> f) { return f; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21> scala.Function21<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, BoxedUnit> proc(JProcedure21<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21> p) { return p; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R> scala.Function22<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R> func(JFunction22<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R> f) { return f; }
    public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22> scala.Function22<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, BoxedUnit> proc(JProcedure22<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22> p) { return p; }
}


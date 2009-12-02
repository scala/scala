/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

abstract class AbstractFunction0[+R] extends Function0[R] { }
abstract class AbstractFunction1[-T1, +R] extends Function1[T1, R] { }
abstract class AbstractFunction2[-T1, -T2, +R] extends Function2[T1, T2, R] { }
abstract class AbstractFunction3[-T1, -T2, -T3, +R] extends Function3[T1, T2, T3, R] { }
abstract class AbstractFunction4[-T1, -T2, -T3, -T4, +R] extends Function4[T1, T2, T3, T4, R] { }
abstract class AbstractFunction5[-T1, -T2, -T3, -T4, -T5, +R] extends Function5[T1, T2, T3, T4, T5, R] { }
abstract class AbstractFunction6[-T1, -T2, -T3, -T4, -T5, -T6, +R] extends Function6[T1, T2, T3, T4, T5, T6, R] { }
abstract class AbstractFunction7[-T1, -T2, -T3, -T4, -T5, -T6, -T7, +R] extends Function7[T1, T2, T3, T4, T5, T6, T7, R] { }
abstract class AbstractFunction8[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, +R] extends Function8[T1, T2, T3, T4, T5, T6, T7, T8, R] { }
abstract class AbstractFunction9[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, +R] extends Function9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R] { }
abstract class AbstractFunction10[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, +R] extends Function10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R] { }
abstract class AbstractFunction11[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, +R] extends Function11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R] { }
abstract class AbstractFunction12[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, +R] extends Function12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R] { }
abstract class AbstractFunction13[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, +R] extends Function13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R] { }
abstract class AbstractFunction14[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, +R] extends Function14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R] { }
abstract class AbstractFunction15[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, +R] extends Function15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R] { }
abstract class AbstractFunction16[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, +R] extends Function16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R] { }
abstract class AbstractFunction17[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, +R] extends Function17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R] { }
abstract class AbstractFunction18[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, +R] extends Function18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R] { }
abstract class AbstractFunction19[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, +R] extends Function19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R] { }
abstract class AbstractFunction20[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, +R] extends Function20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R] { }
abstract class AbstractFunction21[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, +R] extends Function21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R] { }
abstract class AbstractFunction22[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, -T22, +R] extends Function22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R] { }

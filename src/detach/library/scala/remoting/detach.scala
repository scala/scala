/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: detach.scala 16901 2009-01-13 15:37:05Z michelou $

package scala.remoting


/** The <code>detach</code> object is a <em>marker object</em> which informs
 *  the Scala compiler that arguments whose type is a function type are
 *  eligible for remote closure generation.
 *
 *  @author  Stephane Micheloud
 *  @version 1.0, 13/07/2005
 */
object detach {

  def apply[R](f: Function0[R]): Function0[R] = f
  def apply[T0, R](f: Function1[T0, R]): Function1[T0, R] = f
  def apply[T0, T1, R](f: Function2[T0, T1, R]): Function2[T0, T1, R] = f
  def apply[T0, T1, T2, R](f: Function3[T0, T1, T2, R]): Function3[T0, T1, T2, R] = f
  def apply[T0, T1, T2, T3, R](f: Function4[T0, T1, T2, T3, R]): Function4[T0, T1, T2, T3, R] = f
  def apply[T0, T1, T2, T3, T4, R](f: Function5[T0, T1, T2, T3, T4, R]): Function5[T0, T1, T2, T3, T4, R] = f
  def apply[T0, T1, T2, T3, T4, T5, R](f: Function6[T0, T1, T2, T3, T4, T5, R]): Function6[T0, T1, T2, T3, T4, T5, R] = f
  def apply[T0, T1, T2, T3, T4, T5, T6, R](f: Function7[T0, T1, T2, T3, T4, T5, T6, R]): Function7[T0, T1, T2, T3, T4, T5, T6, R] = f
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, R](f: Function8[T0, T1, T2, T3, T4, T5, T6, T7, R]): Function8[T0, T1, T2, T3, T4, T5, T6, T7, R] = f
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, R](f: Function9[T0, T1, T2, T3, T4, T5, T6, T7, T8, R]): Function9[T0, T1, T2, T3, T4, T5, T6, T7, T8, R] = f

  // since 2.7.0
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, R](f: Function10[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, R]): Function10[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, R] = f
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](f: Function11[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]): Function11[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R] = f
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](f: Function12[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]): Function12[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R] = f
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](f: Function13[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]): Function13[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R] = f
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](f: Function14[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]): Function14[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R] = f
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](f: Function15[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]): Function15[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R] = f
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](f: Function16[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]): Function16[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R] = f
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](f: Function17[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]): Function17[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R] = f
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](f: Function18[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]): Function18[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R] = f
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](f: Function19[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]): Function19[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R] = f
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](f: Function20[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]): Function20[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R] = f
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](f: Function21[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]): Function21[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R] = f
  def apply[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](f: Function22[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R]): Function22[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R] = f
}


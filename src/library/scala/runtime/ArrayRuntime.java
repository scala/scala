/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.runtime;

/**
 * Methods on Java arrays
 */
class ArrayRuntime {
  static boolean[] cloneArray(boolean[] array) { return array.clone(); }
  static byte[] cloneArray(byte[] array) { return array.clone(); }
  static short[] cloneArray(short[] array) { return array.clone(); }
  static char[] cloneArray(char[] array) { return array.clone(); }
  static int[] cloneArray(int[] array) { return array.clone(); }
  static long[] cloneArray(long[] array) { return array.clone(); }
  static float[] cloneArray(float[] array) { return array.clone(); }
  static double[] cloneArray(double[] array) { return array.clone(); }
  static Object[] cloneArray(Object[] array) { return array.clone(); }
}

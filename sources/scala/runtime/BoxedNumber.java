/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $OldId: RunTime.java,v 1.13 2002/11/19 12:01:40 paltherr Exp $
// $Id$
package scala.runtime;

public abstract class BoxedNumber {
  public abstract byte byteValue();
  public abstract short shortValue();
  public abstract char charValue();
  public abstract int intValue();
  public abstract long longValue();
  public abstract float floatValue();
  public abstract double doubleValue();
}

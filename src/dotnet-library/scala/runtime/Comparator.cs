/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

namespace scala.runtime {

  using System;

  public class Comparator {
    public static bool equals(object a, object b) {
      if (a == null)
        return b == null;
      if (a.Equals(b))
        return true;
      if (a == b)
        return true;
      IConvertible aa = a as IConvertible;
      IConvertible bb = b as IConvertible;
      
      // We must check if a and b are realy convertible to numbers, because 
      // String is also an implementation of IConvertible...
      // So we can avoid having "12" == 12 returning true !!
      if((a is Decimal || a is Double || a is Single || a is Byte 
          || a is SByte || a is Int16 || a is UInt16 || a is Int32 
          || a is UInt32 || a is Int64 || a is UInt64) 
          && (b is Decimal || b is Double || b is Single || b is Byte 
          || b is SByte || b is Int16 || b is UInt16 || b is Int32 
          || b is UInt32 || b is Int64 || b is UInt64)){
        if (aa != null && bb != null) {
         if (a is Decimal || b is Decimal)
           return aa.ToDecimal(null) == bb.ToDecimal(null);
         if (a is Double || b is Double)
            return aa.ToDouble(null) == bb.ToDouble(null);
          if (a is Single || b is Single)
            return aa.ToSingle(null) == bb.ToSingle(null);
          if (a is Int64 || b is Int64)
            return aa.ToInt64(null) == bb.ToInt64(null);
          return aa.ToInt32(null) == bb.ToInt32(null);
        }
      }
      return false;
    }
  }

}

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

using System;
using scala.runtime;

namespace scala
{

	[Meta("class extends scala.AnyVal;")]
	[Serializable]
	public abstract class Int : AnyVal {

		public readonly int value;

		public Int(int value)
		{
			this.value = value;
		}

		public override bool Equals(object other) 
		{
			return (other is Int) && (value == ((Int)other).value);
		}
		public override int GetHashCode() 
		{
			return value;
		}
		public override string ToString() 
		{
			return value.ToString();
		}

		[Meta("method (scala.Any)scala.Boolean;")]
		public bool __eq__eq  (object other) { return  Equals(other); }
		[Meta("method (scala.Any)scala.Boolean;")]
		public bool __bang__eq(object other) { return !Equals(other); }

		[Meta("method []scala.Int;")]
		public int     __plus      (            ) { return +value        ; }

		[Meta("method []scala.Int;")]
		public int     __minus     (            ) { return -value        ; }

		public string  __plus      (string  that) { return  value +  that; }

		public bool __eq__eq     (double  that) { return  value == that; }
		public bool __bang__eq   (double  that) { return  value != that; }
		public bool __less      (double  that) { return  value <  that; }
		public bool __greater   (double  that) { return  value >  that; }
		public bool __less__eq   (double  that) { return  value <= that; }
		public bool __greater__eq(double  that) { return  value >= that; }
		public double  __plus      (double  that) { return  value +  that; }
		public double  __minus     (double  that) { return  value -  that; }
		public double  __times     (double  that) { return  value *  that; }
		public double  __div       (double  that) { return  value /  that; }
		public double  __percent   (double  that) { return  value %  that; }

		[Meta("method []scala.Double;")]
		public double  coerce     (double dummy) { return  value; }

		public bool __eq__eq     (float   that) { return  value == that; }
		public bool __bang__eq   (float   that) { return  value != that; }
		public bool __less      (float   that) { return  value <  that; }
		public bool __greater   (float   that) { return  value >  that; }
		public bool __less__eq   (float   that) { return  value <= that; }
		public bool __greater__eq(float   that) { return  value >= that; }
		public float   __plus      (float   that) { return  value +  that; }
		public float   __minus     (float   that) { return  value -  that; }
		public float   __times     (float   that) { return  value *  that; }
		public float   __div       (float   that) { return  value /  that; }
		public float   __percent   (float   that) { return  value %  that; }

		[Meta("method []scala.Float;")]
		public float   coerce     (float dummy) { return  value; }
		[Meta("method []scala.Int;")]
		public int     __tilde     (            ) { return ~value; }

		public int     __less__less (int     that) { return  value << that; }
		public int     __less__less (long    that) { return  value << (int)that; }
		public int     __greater__greater(int     that) { return  value >> that; }
		public int     __greater__greater(long    that) { return  value >> (int)that; }
		public int     __greater__greater__greater(int     that) { return  (int)((uint)value >>that); }
		public int     __greater__greater__greater(long    that) { return  (int)((uint)value >>(int)that); }

		public bool __eq__eq     (long    that) { return  value == that; }
		public bool __bang__eq   (long    that) { return  value != that; }
		public bool __less      (long    that) { return  value <  that; }
		public bool __greater   (long    that) { return  value >  that; }
		public bool __less__eq   (long    that) { return  value <= that; }
		public bool __greater__eq(long    that) { return  value >= that; }
		public long    __plus      (long    that) { return  value +  that; }
		public long    __minus     (long    that) { return  value -  that; }
		public long    __times     (long    that) { return  value *  that; }
		public long    __div       (long    that) { return  value /  that; }
		public long    __percent   (long    that) { return  value %  that; }
		public long    __bar       (long    that) { return  value |  that; }
		public long    __amp       (long    that) { return  value &  that; }
		public long    __up        (long    that) { return  value ^  that; }

		[Meta("method []scala.Long;")]
		public long    coerce     (object dummy) { return  value        ; }

		public bool __eq__eq     (int     that) { return  value == that; }
		public bool __bang__eq   (int     that) { return  value != that; }
		public bool __less      (int     that) { return  value <  that; }
		public bool __greater   (int     that) { return  value >  that; }
		public bool __less__eq   (int     that) { return  value <= that; }
		public bool __greater__eq(int     that) { return  value >= that; }
		public int     __plus      (int     that) { return  value +  that; }
		public int     __minus     (int     that) { return  value -  that; }
		public int     __times     (int     that) { return  value *  that; }
		public int     __div       (int     that) { return  value /  that; }
		public int     __percent   (int     that) { return  value %  that; }
		public int     __bar       (int     that) { return  value |  that; }
		public int     __amp       (int     that) { return  value &  that; }
		public int     __up        (int     that) { return  value ^  that; }

	}
}
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
	public abstract class Float : AnyVal {

		public readonly float value;

		public Float(float value)
		{
			this.value = value;
		}

		public override bool Equals(object other) 
		{
			return (other is Float) && (value == ((Float)other).value);
		}
		public override int GetHashCode() 
		{
			return value.GetHashCode();
		}
		public override string ToString() 
		{
			return value.ToString();
		}

		[Meta("method (scala.Any)scala.Boolean;")]
		public bool __eq__eq(object other) { return Equals(other); }
		[Meta("method (scala.Any)scala.Boolean;")]
		public bool __bang__eq(object other) { return !Equals(other); }

		[Meta("method []scala.Float;")]
		public float   __plus      (            ) { return +value        ; }
		[Meta("method []scala.Float;")]
		public float   __minus     (            ) { return -value        ; }

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
		public double  coerce     (            ) { return  value; }

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

	}
}
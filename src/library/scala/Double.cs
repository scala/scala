/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:Double.cs 5359 2005-12-16 16:33:49 +0100 (Fri, 16 Dec 2005) dubochet $

using System;
using scala.runtime;

namespace scala
{

	[Meta("class extends scala.AnyVal;")]
	[Serializable]
	public abstract class Double : AnyVal {

		public readonly double  value;

		public Double (double  value)
		{
			this.value = value;
		}

		public override bool Equals(object other) 
		{
			return other is Double  && value == ((Double )other).value;
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
		public bool __eq__eq  (object other) { return  Equals(other); }
		[Meta("method (scala.Any)scala.Boolean;")]
		public bool __bang__eq(object other) { return !Equals(other); }

		[Meta("method []scala.Double;")]
		public double  __plus      (object dummy) { return +value; }
		[Meta("method []scala.Double;")]
		public double  __minus     (            ) { return -value; }

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

	}
}
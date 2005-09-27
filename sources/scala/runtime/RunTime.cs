/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $OldId: RunTime.java,v 1.13 2002/11/19 12:01:40 paltherr Exp $
// $Id$

using System;
using scala;

namespace scala.runtime 
{

	public abstract class RunTime 
	{

		//########################################################################
		// Private Constants

		private static readonly int BITS        = 8;
		private static readonly int ARRAY_SIZE  = 2 << BITS;
		private static readonly int INDEX_MASK  = ARRAY_SIZE - 1;
		private static readonly int CHECK_MASK  = ~(ARRAY_SIZE / 2 - 1);

		private static readonly UValue uvalue   = new UValue();
		private static readonly ZValue zvalue_f = new ZValue(false);
		private static readonly ZValue zvalue_t = new ZValue(true);
		private static readonly BValue[] bvalue = new BValue[256];
		private static readonly SValue[] svalue = new SValue[ARRAY_SIZE];
		private static readonly CValue[] cvalue = new CValue[ARRAY_SIZE / 2];
		private static readonly IValue[] ivalue = new IValue[ARRAY_SIZE];
		private static readonly LValue[] lvalue = new LValue[ARRAY_SIZE];

		static RunTime() 
		{
			for (int i = 0; i < bvalue.Length; i++)
				bvalue[i] = new BValue((sbyte)i);
			for (int i = 0; i < ARRAY_SIZE / 2; i++) 
			{
				svalue[i] = new SValue((short)i);
				cvalue[i] = new CValue((char )i);
				ivalue[i] = new IValue((int  )i);
				lvalue[i] = new LValue((long )i);
				svalue[i + ARRAY_SIZE / 2] = new SValue((short)(CHECK_MASK | i));
				ivalue[i + ARRAY_SIZE / 2] = new IValue((int  )(CHECK_MASK | i));
				lvalue[i + ARRAY_SIZE / 2] = new LValue((long )(CHECK_MASK | i));
			}
		}

		//########################################################################
		// Private Variables

		//private static ClassLoader loader = ClassLoader.getSystemClassLoader();

		//########################################################################
		// Public Functions - Getting & setting class loader

//		public static ClassLoader getClassLoader() 
//		{
//			return loader;
//		}
//
//		public static void setClassLoader(ClassLoader loader) 
//		{
//			RunTime.loader = loader;
//		}

		//########################################################################
		// Public Functions - Catching exceptions
		public interface Runnable {
			void run();
		}

		public static Exception tryCatch(Runnable runnable) 
		{
			try 
			{
				runnable.run();
				return null;
			} 
			catch (Exception exception) 
			{
				return exception;
			}
		}

		//########################################################################
		// Public Functions - Boxing primitives

		public static Unit    box_uvalue(         ) 
		{
			return uvalue;
		}

		public static Boolean box_zvalue(bool x) 
		{
			return x ? zvalue_t : zvalue_f;
		}

		public static Byte    box_bvalue(sbyte    x) 
		{
			return bvalue[x & 0x000000FF];
		}

		public static Short   box_svalue(short   x) 
		{
			int c = x & CHECK_MASK;
			if (c == 0 || c == CHECK_MASK) return svalue[x & INDEX_MASK];
			return new SValue(x);
		}

		public static Char    box_cvalue(char    x) 
		{
			int c = (int)x & CHECK_MASK;
			if (c == 0) return cvalue[(int)x & INDEX_MASK];
			return new CValue(x);
		}

		public static Int     box_ivalue(int     x) 
		{
			int c = x & CHECK_MASK;
			if (c == 0 || c == CHECK_MASK) return ivalue[x & INDEX_MASK];
			return new IValue(x);
		}

		public static Long    box_lvalue(long    x) 
		{
			long c = x & CHECK_MASK;
			if (c == 0 || c == CHECK_MASK) return lvalue[(int)x & INDEX_MASK];
			return new LValue(x);
		}

		public static Float   box_fvalue(float   x) 
		{
			return new FValue(x);
		}

		public static Double  box_dvalue(double  x) 
		{
			return new DValue(x);
		}

		/** @meta method (scala.Array[scala.Boolean]) scala.Array[scala.Boolean];*/
		public static Array   box_zarray(bool[] xs) 
		{
			return new ZArray(xs);
		}

		/** @meta method (scala.Array[scala.Byte]) scala.Array[scala.Byte]; */
		public static Array   box_barray(sbyte   [] xs) 
		{
			return new BArray(xs);
		}

		/** @meta method (scala.Array[scala.Short]) scala.Array[scala.Short]; */
		public static Array   box_sarray(short  [] xs) 
		{
			return new SArray(xs);
		}

		/** @meta method (scala.Array[scala.Char]) scala.Array[scala.Char]; */
		public static Array   box_carray(char   [] xs) 
		{
			return new CArray(xs);
		}

		/** @meta method (scala.Array[scala.Int]) scala.Array[scala.Int]; */
		public static Array   box_iarray(int    [] xs) 
		{
			return new IArray(xs);
		}

		/** @meta method (scala.Array[scala.Long]) scala.Array[scala.Long]; */
		public static Array   box_larray(long   [] xs) 
		{
			return new LArray(xs);
		}

		/** @meta method (scala.Array[scala.Float]) scala.Array[scala.Float]; */
		public static Array   box_farray(float  [] xs) 
		{
			return new FArray(xs);
		}

		/** @meta method (scala.Array[scala.Double]) scala.Array[scala.Double]; */
		public static Array   box_darray(double [] xs) 
		{
			return new DArray(xs);
		}

		/** @meta method [?T < scala.AnyRef](scala.Array[?T]) scala.Array[?T]; */
		public static Array   box_oarray(object [] xs) 
		{
			return new OArray(xs);
		}

		/** @meta method [?T](scala.Array[?T]) scala.Array[?T]; */
		public static Array   box__array(object    xs) 
		{
			if (xs == null             ) return box_oarray((object [])xs);
			if (xs is bool[]) return box_zarray((bool[])xs);
			if (xs is sbyte   []) return box_barray((sbyte   [])xs);
			if (xs is short  []) return box_sarray((short  [])xs);
			if (xs is char   []) return box_carray((char   [])xs);
			if (xs is int    []) return box_iarray((int    [])xs);
			if (xs is long   []) return box_larray((long   [])xs);
			if (xs is float  []) return box_farray((float  [])xs);
			if (xs is double []) return box_darray((double [])xs);
			if (xs is object []) return box_oarray((object [])xs);
			throw new InvalidCastException(xs.GetType() + " is not an array class");
		}

		//########################################################################
		// Public Functions - Unboxing primitives

		public static void      unbox_uvalue(Unit    x) {        x.value(); }
		public static bool   unbox_zvalue(Boolean x) { return x.value  ; }
		public static sbyte      unbox_bvalue(Byte    x) { return x.value  ; }
		public static short     unbox_svalue(Short   x) { return x.value  ; }
		public static char      unbox_cvalue(Char    x) { return x.value  ; }
		public static int       unbox_ivalue(Int     x) { return x.value  ; }
		public static long      unbox_lvalue(Long    x) { return x.value  ; }
		public static float     unbox_fvalue(Float   x) { return x.value  ; }
		public static double    unbox_dvalue(Double  x) { return x.value  ; }

		/** @meta method (scala.Array[scala.Boolean]) scala.Array[scala.Boolean];*/
		public static bool[] unbox_zarray(Array xs) 
		{
			return xs == null ? null : ((ZArray)xs)._value;
		}
		/** @meta method (scala.Array[scala.Byte]) scala.Array[scala.Byte]; */
		public static sbyte   [] unbox_barray(Array xs) 
		{
			return xs == null ? null : ((BArray)xs)._value;
		}
		/** @meta method (scala.Array[scala.Short]) scala.Array[scala.Short]; */
		public static short  [] unbox_sarray(Array xs) 
		{
			return xs == null ? null : ((SArray)xs)._value;
		}
		/** @meta method (scala.Array[scala.Char]) scala.Array[scala.Char]; */
		public static char   [] unbox_carray(Array xs) 
		{
			return xs == null ? null : ((CArray)xs)._value;
		}
		/** @meta method (scala.Array[scala.Int]) scala.Array[scala.Int]; */
		public static int    [] unbox_iarray(Array xs) 
		{
			return xs == null ? null : ((IArray)xs)._value;
		}
		/** @meta method (scala.Array[scala.Long]) scala.Array[scala.Long]; */
		public static long   [] unbox_larray(Array xs) 
		{
			return xs == null ? null : ((LArray)xs)._value;
		}
		/** @meta method (scala.Array[scala.Float]) scala.Array[scala.Float]; */
		public static float  [] unbox_farray(Array xs) 
		{
			return xs == null ? null : ((FArray)xs)._value;
		}
		/** @meta method (scala.Array[scala.Double]) scala.Array[scala.Double]; */
		public static double [] unbox_darray(Array xs) 
		{
			return xs == null ? null : ((DArray)xs)._value;
		}
		/** @meta method [?T < scala.AnyRef](scala.Array[?T]) scala.Array[scala.AnyRef]; */
		public static object [] unbox_oarray(Array xs) 
		{
			return xs == null ? null : ((OArray)xs)._value;
		}
		/** @meta method [?T](scala.Array[?T]) scala.AnyRef; */
		public static object    unbox__array(Array xs) 
		{
			return xs == null ? null : xs.value();
		}

		//########################################################################
		// Public Functions - Conversion primitives

		public static sbyte   b2b(sbyte   x) { return (sbyte  )x; }
		public static short  b2s(sbyte   x) { return (short )x; }
		public static char   b2c(sbyte   x) { return (char  )x; }
		public static int    b2i(sbyte   x) { return (int   )x; }
		public static long   b2l(sbyte   x) { return (long  )x; }
		public static float  b2f(sbyte   x) { return (float )x; }
		public static double b2d(sbyte   x) { return (double)x; }
		public static sbyte   s2b(short  x) { return (sbyte  )x; }
		public static short  s2s(short  x) { return (short )x; }
		public static char   s2c(short  x) { return (char  )x; }
		public static int    s2i(short  x) { return (int   )x; }
		public static long   s2l(short  x) { return (long  )x; }
		public static float  s2f(short  x) { return (float )x; }
		public static double s2d(short  x) { return (double)x; }
		public static sbyte   c2b(char   x) { return (sbyte  )x; }
		public static short  c2s(char   x) { return (short )x; }
		public static char   c2c(char   x) { return (char  )x; }
		public static int    c2i(char   x) { return (int   )x; }
		public static long   c2l(char   x) { return (long  )x; }
		public static float  c2f(char   x) { return (float )x; }
		public static double c2d(char   x) { return (double)x; }
		public static sbyte   i2b(int    x) { return (sbyte  )x; }
		public static short  i2s(int    x) { return (short )x; }
		public static char   i2c(int    x) { return (char  )x; }
		public static int    i2i(int    x) { return (int   )x; }
		public static long   i2l(int    x) { return (long  )x; }
		public static float  i2f(int    x) { return (float )x; }
		public static double i2d(int    x) { return (double)x; }
		public static sbyte   l2b(long   x) { return (sbyte  )x; }
		public static short  l2s(long   x) { return (short )x; }
		public static char   l2c(long   x) { return (char  )x; }
		public static int    l2i(long   x) { return (int   )x; }
		public static long   l2l(long   x) { return (long  )x; }
		public static float  l2f(long   x) { return (float )x; }
		public static double l2d(long   x) { return (double)x; }
		public static sbyte   f2b(float  x) { return (sbyte  )x; }
		public static short  f2s(float  x) { return (short )x; }
		public static char   f2c(float  x) { return (char  )x; }
		public static int    f2i(float  x) { return (int   )x; }
		public static long   f2l(float  x) { return (long  )x; }
		public static float  f2f(float  x) { return (float )x; }
		public static double f2d(float  x) { return (double)x; }
		public static sbyte   d2b(double x) { return (sbyte  )x; }
		public static short  d2s(double x) { return (short )x; }
		public static char   d2c(double x) { return (char  )x; }
		public static int    d2i(double x) { return (int   )x; }
		public static long   d2l(double x) { return (long  )x; }
		public static float  d2f(double x) { return (float )x; }
		public static double d2d(double x) { return (double)x; }

		//########################################################################
		// Public Functions - Array primitives

		public static bool[] zarray(int length) { return new bool[length]; }
		public static sbyte   [] barray(int length) { return new sbyte   [length]; }
		public static short  [] sarray(int length) { return new short  [length]; }
		public static char   [] carray(int length) { return new char   [length]; }
		public static int    [] iarray(int length) { return new int    [length]; }
		public static long   [] larray(int length) { return new long   [length]; }
		public static float  [] farray(int length) { return new float  [length]; }
		public static double [] darray(int length) { return new double [length]; }
		public static object    oarray(int length, string classname) 
		{
			try
			{
				Type clasz = Type.GetType(classname);
				return System.Array.CreateInstance(clasz, length);
			} 
			catch (Exception exception) 
			{
				//throw new Error(exception.ToString());
				throw new ApplicationException(exception.ToString());
			}
		}

		public static int zarray_length(bool[] xs) { return xs.Length; }
		public static int barray_length(sbyte   [] xs) { return xs.Length; }
		public static int sarray_length(short  [] xs) { return xs.Length; }
		public static int carray_length(char   [] xs) { return xs.Length; }
		public static int iarray_length(int    [] xs) { return xs.Length; }
		public static int larray_length(long   [] xs) { return xs.Length; }
		public static int farray_length(float  [] xs) { return xs.Length; }
		public static int darray_length(double [] xs) { return xs.Length; }
		public static int oarray_length(object [] xs) { return xs.Length; }

		public static bool zarray_get(bool[] xs, int i) { return xs[i]; }
		public static sbyte    barray_get(sbyte   [] xs, int i) { return xs[i]; }
		public static short   sarray_get(short  [] xs, int i) { return xs[i]; }
		public static char    carray_get(char   [] xs, int i) { return xs[i]; }
		public static int     iarray_get(int    [] xs, int i) { return xs[i]; }
		public static long    larray_get(long   [] xs, int i) { return xs[i]; }
		public static float   farray_get(float  [] xs, int i) { return xs[i]; }
		public static double  darray_get(double [] xs, int i) { return xs[i]; }
		public static object  oarray_get(object [] xs, int i) { return xs[i]; }

		public static void zarray_set(bool[] xs, int i, bool x) { xs[i] = x;}
		public static void barray_set(sbyte   [] xs, int i, sbyte    x) { xs[i] = x;}
		public static void sarray_set(short  [] xs, int i, short   x) { xs[i] = x;}
		public static void carray_set(char   [] xs, int i, char    x) { xs[i] = x;}
		public static void iarray_set(int    [] xs, int i, int     x) { xs[i] = x;}
		public static void larray_set(long   [] xs, int i, long    x) { xs[i] = x;}
		public static void farray_set(float  [] xs, int i, float   x) { xs[i] = x;}
		public static void darray_set(double [] xs, int i, double  x) { xs[i] = x;}
		public static void oarray_set(object [] xs, int i, object  x) { xs[i] = x;}

		//########################################################################
	}

	// These classes may not be defined in class RunTime because inner
	// classes confuse pico which then attributes the metadata to the
	// wrong members.

	sealed class UValue : Unit    { public UValue(         ) : base( ) { } }
	sealed class ZValue : Boolean { public ZValue(bool x   ) : base(x) { } }
	sealed class BValue : Byte    { public BValue(sbyte    x) : base(x) { } }
	sealed class SValue : Short   { public SValue(short   x) : base(x) { } }
	sealed class CValue : Char    { public CValue(char    x) : base(x) { } }
	sealed class IValue : Int     { public IValue(int     x) : base(x) { } }
	sealed class LValue : Long    { public LValue(long    x) : base(x) { } }
	sealed class FValue : Float   { public FValue(float   x) : base(x) { } }
	sealed class DValue : Double  { public DValue(double  x) : base(x) { } }

	/** @meta class extends scala.Array[scala.Boolean]; */
	sealed class ZArray : Array 
	{
		internal readonly bool[] _value;
		public ZArray(bool[] _value) { this._value = _value; }
		public override object value() { return _value; }
		public override object apply(int i) { return RunTime.box_zvalue(_value[i]); }
		public override void update(int i, object x) { _value[i] = ((Boolean)x).value; }
		public override int length() { return _value.Length; }
		public override string ToString() { return _value.ToString(); }
	}

	[Meta("class extends scala.Array[scala.Byte];")]
	sealed class BArray : Array 
	{
		internal readonly sbyte[] _value;
		public BArray(sbyte[] _value) { this._value = _value; }
		public override object value() { return _value; }
		public override object apply(int i) { return RunTime.box_bvalue(_value[i]); }
		public override void update(int i, object x) { _value[i] = ((Byte)x).value; }
		public override int length() { return _value.Length; }
		public override string ToString() { return _value.ToString(); }
	}

	[Meta("class extends scala.Array[scala.Short];")]
	sealed class SArray : Array 
	{
		internal readonly short[] _value;
		public SArray(short[] _value) { this._value = _value; }
		public override object value() { return _value; }
		public override object apply(int i) { return RunTime.box_svalue(_value[i]); }
		public override void update(int i, object x) { _value[i] = ((Short)x).value; }
		public override int length() { return _value.Length; }
		public override string ToString() { return _value.ToString(); }
	}

	[Meta("class extends scala.Array[scala.Char];")]
	sealed class CArray : Array 
	{
		internal readonly char[] _value;
		public CArray(char[] _value) { this._value = _value; }
		public override object value() { return _value; }
		public override object apply(int i) { return RunTime.box_cvalue(_value[i]); }
		public override void update(int i, object x) { _value[i] = ((Char)x).value; }
		public override int length() { return _value.Length; }
		public override string ToString() { return ((object)_value).ToString(); }
	}

	[Meta("class extends scala.Array[scala.Int];")]
	sealed class IArray : Array 
	{
		internal readonly int[] _value;
		public IArray(int[] _value) { this._value = _value; }
		public override object value() { return _value; }
		public override object apply(int i) { return RunTime.box_ivalue(_value[i]); }
		public override void update(int i, object x) { _value[i] = ((Int)x).value; }
		public override int length() { return _value.Length; }
		public override string ToString() { return _value.ToString(); }
	}

	[Meta("class extends scala.Array[scala.Long];")]
	sealed class LArray : Array 
	{
		internal readonly long[] _value;
		public LArray(long[] _value) { this._value = _value; }
		public override object value() { return _value; }
		public override object apply(int i) { return RunTime.box_lvalue(_value[i]); }
		public override void update(int i, object x) { _value[i] = ((Long)x).value; }
		public override int length() { return _value.Length; }
		public override string ToString() { return _value.ToString(); }
	}

	[Meta("class extends scala.Array[scala.Float];")]
	sealed class FArray : Array 
	{
		internal readonly float[] _value;
		public FArray(float[] _value) { this._value = _value; }
		public override object value() { return _value; }
		public override object apply(int i) { return RunTime.box_fvalue(_value[i]); }
		public override void update(int i, object x) { _value[i] = ((Float)x).value; }
		public override int length() { return _value.Length; }
		public override string ToString() { return _value.ToString(); }
	}

	[Meta("class extends scala.Array[scala.Double];")]
	sealed class DArray : Array 
	{
		internal readonly double[] _value;
		public DArray(double[] _value) { this._value = _value; }
		public override object value() { return _value; }
		public override object apply(int i) { return RunTime.box_dvalue(_value[i]); }
		public override void update(int i, object x) { _value[i] = ((Double)x).value; }
		public override int length() { return _value.Length; }
		public override string ToString() { return _value.ToString(); }
	}

	[Meta("class [?T < scala.AnyRef] extends scala.Array[?T];")]
	sealed class OArray : Array 
	{
		internal readonly object[] _value;
		public OArray(object[] _value) { this._value = _value; }
		public override object value() { return _value; }
		public override object apply(int i) { return _value[i]; }
		public override void update(int i, object x) { _value[i] = x; }
		public override int length() { return _value.Length; }
		public override string ToString() { return _value.ToString(); }
	}
}
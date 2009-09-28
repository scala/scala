//############################################################################
// Arrays
//############################################################################

//############################################################################

object Test {

  //##########################################################################
  // Types

  type Strings = List[String]
  type Map     = scala.collection.Map[Int, Any]
  type HashMap = scala.collection.mutable.HashMap[Int, Any]
  type TreeMap = scala.collection.immutable.TreeMap[Int, Any]

  //##########################################################################
  // Identity Functions

  def id_Ta_T[T <: Any    ](x: T): T       = x;
  def id_Tr_T[T <: AnyRef ](x: T): T       = x;
  def id_To_T[T <: Object ](x: T): T       = x;

  def id_Ta_a[T <: Any    ](x: T): Any     = x;
  def id_Tr_a[T <: AnyRef ](x: T): Any     = x;
  def id_To_a[T <: Object ](x: T): Any     = x;

  def id_Tr_r[T <: AnyRef ](x: T): AnyRef  = x;
  def id_To_r[T <: Object ](x: T): AnyRef  = x;

  def id_To_o[T <: Object ](x: T): Object  = x;

  def id_TSa_T [S <: Any    , T <: Array[S]](x: T): T        = x;
  def id_TSv_T [S <: AnyVal , T <: Array[S]](x: T): T        = x;
  def id_TSr_T [S <: AnyRef , T <: Array[S]](x: T): T        = x;
  def id_TSo_T [S <: Object , T <: Array[S]](x: T): T        = x;
  def id_TSm_T [S <: Map    , T <: Array[S]](x: T): T        = x;
  def id_TSn_T [S <: Strings, T <: Array[S]](x: T): T        = x;

  def id_TSa_Ss[S <: Any    , T <: Array[S]](x: T): Array[S] = x;
  def id_TSv_Ss[S <: AnyVal , T <: Array[S]](x: T): Array[S] = x;
  def id_TSr_Ss[S <: AnyRef , T <: Array[S]](x: T): Array[S] = x;
  def id_TSo_Ss[S <: Object , T <: Array[S]](x: T): Array[S] = x;
  def id_TSm_Ss[S <: Map    , T <: Array[S]](x: T): Array[S] = x;
  def id_TSn_Ss[S <: Strings, T <: Array[S]](x: T): Array[S] = x;

  def id_TSa_a [S <: Any    , T <: Array[S]](x: T): Any      = x;
  def id_TSv_a [S <: AnyVal , T <: Array[S]](x: T): Any      = x;
  def id_TSr_a [S <: AnyRef , T <: Array[S]](x: T): Any      = x;
  def id_TSo_a [S <: Object , T <: Array[S]](x: T): Any      = x;
  def id_TSm_a [S <: Map    , T <: Array[S]](x: T): Any      = x;
  def id_TSn_a [S <: Strings, T <: Array[S]](x: T): Any      = x;

  def id_TSa_r [S <: Any    , T <: Array[S]](x: T): AnyRef   = x;
  def id_TSv_r [S <: AnyVal , T <: Array[S]](x: T): AnyRef   = x;
  def id_TSr_r [S <: AnyRef , T <: Array[S]](x: T): AnyRef   = x;
  def id_TSo_r [S <: Object , T <: Array[S]](x: T): AnyRef   = x;
  def id_TSm_r [S <: Map    , T <: Array[S]](x: T): AnyRef   = x;
  def id_TSn_r [S <: Strings, T <: Array[S]](x: T): AnyRef   = x;

  def id_TSa_o [S <: Any    , T <: Array[S]](x: T): Object   = x;
  def id_TSv_o [S <: AnyVal , T <: Array[S]](x: T): Object   = x;
  def id_TSr_o [S <: AnyRef , T <: Array[S]](x: T): Object   = x;
  def id_TSo_o [S <: Object , T <: Array[S]](x: T): Object   = x;
  def id_TSm_o [S <: Map    , T <: Array[S]](x: T): Object   = x;
  def id_TSn_o [S <: Strings, T <: Array[S]](x: T): Object   = x;

  def id_Sas_Ss[S <: Any    ](xs: Array[S]): Array[S] = xs;
  def id_Svs_Ss[S <: AnyVal ](xs: Array[S]): Array[S] = xs;
  def id_Srs_Ss[S <: AnyRef ](xs: Array[S]): Array[S] = xs;
  def id_Sos_Ss[S <: Object ](xs: Array[S]): Array[S] = xs;
  def id_Sms_Ss[S <: Map    ](xs: Array[S]): Array[S] = xs;
  def id_Sns_Ss[S <: Strings](xs: Array[S]): Array[S] = xs;

  def id_Sas_a [S <: Any    ](xs: Array[S]): Any      = xs;
  def id_Svs_a [S <: AnyVal ](xs: Array[S]): Any      = xs;
  def id_Srs_a [S <: AnyRef ](xs: Array[S]): Any      = xs;
  def id_Sos_a [S <: Object ](xs: Array[S]): Any      = xs;
  def id_Sms_a [S <: Map    ](xs: Array[S]): Any      = xs;
  def id_Sns_a [S <: Strings](xs: Array[S]): Any      = xs;

  def id_Sas_r [S <: Any    ](xs: Array[S]): AnyRef   = xs;
  def id_Svs_r [S <: AnyVal ](xs: Array[S]): AnyRef   = xs;
  def id_Srs_r [S <: AnyRef ](xs: Array[S]): AnyRef   = xs;
  def id_Sos_r [S <: Object ](xs: Array[S]): AnyRef   = xs;
  def id_Sms_r [S <: Map    ](xs: Array[S]): AnyRef   = xs;
  def id_Sns_r [S <: Strings](xs: Array[S]): AnyRef   = xs;

  def id_Sas_o [S <: Any    ](xs: Array[S]): Object   = xs;
  def id_Svs_o [S <: AnyVal ](xs: Array[S]): Object   = xs;
  def id_Srs_o [S <: AnyRef ](xs: Array[S]): Object   = xs;
  def id_Sos_o [S <: Object ](xs: Array[S]): Object   = xs;
  def id_Sms_o [S <: Map    ](xs: Array[S]): Object   = xs;
  def id_Sns_o [S <: Strings](xs: Array[S]): Object   = xs;

  //##########################################################################
  // Generic Checks

  type Check[T] = Array[T] => Unit;

  var checks: Int = 0;

  def check(test0: Boolean, actual: Any, expected: Any) {
    val test1: Boolean = actual == expected;
    if (!test0 || !test1) {
      val s0 = if (test0) "ok" else "KO";
      val s1 = if (test1) "ok" else "KO";
      val s2 = actual.toString();
      val s3 = expected.toString();
      error(s0 + " - " + s1 + ": " + s2 + " != " + s3);
    }
    checks += 1
  }

  def check_Ta[T <: Any    ](xs: Array[T], l: Int, x0: T, c: Check[T]) {
    check(xs.length == l, xs.length, l);
    check(xs(0) == x0, xs(0), x0);
    c(xs);
  }

  def check_Tv[T <: AnyVal ](xs: Array[T], l: Int, x0: T, c: Check[T]) {
    check(xs.length == l, xs.length, l);
    check(xs(0) == x0, xs(0), x0);
    check_Ta(xs, l, x0, c);
    c(xs);
  }

  def check_Tr[T <: AnyRef ](xs: Array[T], l: Int, x0: T, c: Check[T]) {
    check(xs.length == l, xs.length, l);
    check(xs(0) == x0, xs(0), x0);
    check_Ta(xs, l, x0, c);
    c(xs);
  }

  def check_To[T <: Object ](xs: Array[T], l: Int, x0: T, c: Check[T]) {
    check(xs.length == l, xs.length, l);
    check(xs(0) == x0, xs(0), x0);
    check_Ta(xs, l, x0, c);
    check_Tr(xs, l, x0, c);
    c(xs);
  }

  def check_Tm[T <: Map    ](xs: Array[T], l: Int, x0: T, c: Check[T]) {
    check(xs.length == l, xs.length, l)
    check(xs(0) == x0, xs(0), x0)
    check_Ta(xs, l, x0, c)
    check_Tr(xs, l, x0, c)
    check_To(xs, l, x0, c)
    c(xs)
  }

  def check_Tn[T <: Strings](xs: Array[T], l: Int, x0: T, c: Check[T]) {
    check(xs.length == l, xs.length, l)
    check(xs(0) == x0, xs(0), x0)
    check_Ta(xs, l, x0, c)
    check_Tr(xs, l, x0, c)
    check_To(xs, l, x0, c)
    c(xs)
  }

  def checkT2368() {
    val arr = Array(1, 2, 3)
    arr(0) += 1
    assert(arr(0) == 2)
  }

  //##########################################################################
  // Values

  import Math._

  val u0: Unit    = ();
  val u1: Unit    = ();

  val z0: Boolean = false;
  val z1: Boolean = true;

  val b0: Byte    = MIN_BYTE;
  val b1: Byte    = 1;
  val b2: Byte    = MAX_BYTE;

  val s0: Short   = MIN_SHORT;
  val s1: Short   = 2;
  val s2: Short   = MAX_SHORT;

  val c0: Char    = MIN_CHAR;
  val c1: Char    = '3';
  val c2: Char    = MAX_CHAR;

  val i0: Int     = MIN_INT;
  val i1: Int     = 4;
  val i2: Int     = MAX_INT;

  val l0: Long    = MIN_LONG;
  val l1: Int     = 5;
  val l2: Long    = MAX_LONG;

  val f0: Float   = MIN_FLOAT;
  val f1: Int     = 6;
  val f2: Float   = MAX_FLOAT;

  val d0: Double  = MIN_DOUBLE;
  val d1: Int     = 7;
  val d2: Double  = MAX_DOUBLE;

  val a0: Unit    = ();
  val a1: Boolean = false;
  val a2: Int     = 0;
  val a3: Null  = null;
  val a4: String  = "a-z";
  val a5: Symbol  = 'token;
  val a6: HashMap = new HashMap();
  val a7: TreeMap = scala.collection.immutable.TreeMap.empty[Int, Any];
  val a8: Strings = List("a", "z");

  val v0: Unit    = ();
  val v1: Boolean = false;
  val v2: Int     = 0;
  val v3: Long    = l2;
  val v4: Float   = f2;
  val v5: Double  = d2;

  val r0: Null  = a3;
  val r1: String  = a4;
  val r2: Symbol  = a5;
  val r3: HashMap = a6;
  val r4: TreeMap = a7;
  val r5: Strings = a8;

  val o0: Null  = r0;
  val o1: String  = r1;
  val o2: Symbol  = r2;
  val o3: HashMap = r3;
  val o4: TreeMap = r4;
  val o5: Strings = r5;

  val m0: Null  = r0;
  val m1: HashMap = r3;
  val m2: TreeMap = r4;

  val n0: Null  = r0;
  val n1: Strings = r5;
  val n2: Nil.type= Nil;

  //##########################################################################
  // Specific Checks

  def ucheck(xs: Array[Unit   ]): Unit = {
    check(xs.length == 2, xs.length, 2);
    check(xs(0) == u0, xs(0), u0);
    check(xs(1) == u1, xs(1), u1);
  }

  def zcheck(xs: Array[Boolean]): Unit = {
    check(xs.length == 2, xs.length, 2);
    check(xs(0) == z0, xs(0), z0);
    check(xs(1) == z1, xs(1), z1);
  }

  def bcheck(xs: Array[Byte   ]): Unit = {
    check(xs.length == 3, xs.length, 3);
    check(xs(0) == b0, xs(0), b0);
    check(xs(1) == b1, xs(1), b1);
    check(xs(2) == b2, xs(2), b2);
  }

  def scheck(xs: Array[Short  ]): Unit = {
    check(xs.length == 3, xs.length, 3);
    check(xs(0) == s0, xs(0), s0);
    check(xs(1) == s1, xs(1), s1);
    check(xs(2) == s2, xs(2), s2);
  }

  def ccheck(xs: Array[Char   ]): Unit = {
    check(xs.length == 3, xs.length, 3);
    check(xs(0) == c0, xs(0), c0);
    check(xs(1) == c1, xs(1), c1);
    check(xs(2) == c2, xs(2), c2);
  }

  def icheck(xs: Array[Int    ]): Unit = {
    check(xs.length == 3, xs.length, 3);
    check(xs(0) == i0, xs(0), i0);
    check(xs(1) == i1, xs(1), i1);
    check(xs(2) == i2, xs(2), i2);
  }

  def lcheck(xs: Array[Long   ]): Unit = {
    check(xs.length == 3, xs.length, 3);
    check(xs(0) == l0, xs(0), l0);
    check(xs(1) == l1, xs(1), l1: Long); // !!! : Long
    check(xs(2) == l2, xs(2), l2);
  }

  def fcheck(xs: Array[Float  ]): Unit = {
    check(xs.length == 3, xs.length, 3);
    check(xs(0) == f0, xs(0), f0);
    check(xs(1) == f1, xs(1), f1: Float); // !!! : Float
    check(xs(2) == f2, xs(2), f2);
  }

  def dcheck(xs: Array[Double ]): Unit = {
    check(xs.length == 3, xs.length, 3);
    check(xs(0) == d0, xs(0), d0);
    check(xs(1) == d1, xs(1), d1: Double); // !!! : Double
    check(xs(2) == d2, xs(2), d2);
  }

  def rcheck(xs: Array[AnyRef ]): Unit = {
    check(xs.length == 6, xs.length, 6);
    check(xs(0) == r0, xs(0), r0);
    check(xs(1) == r1, xs(1), r1);
    check(xs(2) == r2, xs(2), r2);
    check(xs(3) == r3, xs(3), r3);
    check(xs(4) == r4, xs(4), r4);
    check(xs(5) == r5, xs(5), r5);
  }

  def ocheck(xs: Array[Object ]): Unit = {
    check(xs.length == 6, xs.length, 6);
    check(xs(0) == o0, xs(0), o0);
    check(xs(1) == o1, xs(1), o1);
    check(xs(2) == o2, xs(2), o2);
    check(xs(3) == o3, xs(3), o3);
    check(xs(4) == o4, xs(4), o4);
    check(xs(5) == o5, xs(5), o5);
  }

  def mcheck(xs: Array[Map    ]): Unit = {
    check(xs.length == 3, xs.length, 3);
    check(xs(0) == m0, xs(0), m0);
    check(xs(1) == m1, xs(1), m1);
    check(xs(2) == m2, xs(2), m2);
  }

  def ncheck(xs: Array[Strings]) {
    check(xs.length == 3, xs.length, 3)
    check(xs(0) == n0, xs(0), n0)
    check(xs(1) == n1, xs(1), n1)
    check(xs(2) == n2, xs(2), n2)
  }

  //##########################################################################
  // Miscellaneous checks

  def checkZip {
    val zipped = Array("a", "b", "c").zip(Array(1, 2))
    val expected = Array(("a",1), ("b",2))
    check(zipped sameElements expected, zipped.toList, expected.toList)
  }

  def checkConcat { // ticket #713
    val x1 = Array.concat(Array(1, 2), Array(3, 4))
    val y1 = Array(1, 2, 3, 4)
    check(x1 sameElements y1, x1.toList, y1.toList)
  }

  //##########################################################################
  // Arrays

  val uarray: Array[Unit   ] = Array(u0, u1);
  val zarray: Array[Boolean] = Array(z0, z1);
  val barray: Array[Byte   ] = Array(b0, b1, b2);
  val sarray: Array[Short  ] = Array(s0, s1, s2);
  val carray: Array[Char   ] = Array(c0, c1, c2);
  val iarray: Array[Int    ] = Array(i0, i1, i2);
  val larray: Array[Long   ] = Array(l0, l1, l2);
  val farray: Array[Float  ] = Array(f0, f1, f2);
  val darray: Array[Double ] = Array(d0, d1, d2);
  val rarray: Array[AnyRef ] = Array(r0, r1, r2, r4, r4, r5);
  val oarray: Array[Object ] = Array(o0, o1, o2, o4, o4, o5);
  val marray: Array[Map    ] = Array(m0, m1, m2);
  val narray: Array[Strings] = Array(n0, n1, n2);

  //##########################################################################
  // Main

  def main(args: Array[String]): Unit = {

    //######################################################################

    ucheck(uarray);
    zcheck(zarray);
    bcheck(barray);
    scheck(sarray);
    ccheck(carray);
    icheck(iarray);
    lcheck(larray);
    fcheck(farray);
    dcheck(darray);
    rcheck(rarray);
    ocheck(oarray);
    mcheck(marray);
    ncheck(narray);

    //######################################################################

    ucheck(id_Ta_T(uarray));
    zcheck(id_Ta_T(zarray));
    bcheck(id_Ta_T(barray));
    scheck(id_Ta_T(sarray));
    ccheck(id_Ta_T(carray));
    icheck(id_Ta_T(iarray));
    lcheck(id_Ta_T(larray));
    fcheck(id_Ta_T(farray));
    dcheck(id_Ta_T(darray));
    rcheck(id_Ta_T(rarray));
    ocheck(id_Ta_T(oarray));
    mcheck(id_Ta_T(marray));
    ncheck(id_Ta_T(narray));

    ucheck(id_Tr_T(uarray));
    zcheck(id_Tr_T(zarray));
    bcheck(id_Tr_T(barray));
    scheck(id_Tr_T(sarray));
    ccheck(id_Tr_T(carray));
    icheck(id_Tr_T(iarray));
    lcheck(id_Tr_T(larray));
    fcheck(id_Tr_T(farray));
    dcheck(id_Tr_T(darray));
    rcheck(id_Tr_T(rarray));
    ocheck(id_Tr_T(oarray));
    mcheck(id_Tr_T(marray));
    ncheck(id_Tr_T(narray));

    ucheck(id_To_T(uarray));
    zcheck(id_To_T(zarray));
    bcheck(id_To_T(barray));
    scheck(id_To_T(sarray));
    ccheck(id_To_T(carray));
    icheck(id_To_T(iarray));
    lcheck(id_To_T(larray));
    fcheck(id_To_T(farray));
    dcheck(id_To_T(darray));
    rcheck(id_To_T(rarray));
    ocheck(id_To_T(oarray));
    mcheck(id_To_T(marray));
    ncheck(id_To_T(narray));

    ucheck(id_Ta_a(uarray).asInstanceOf[Array[Unit   ]]);
    zcheck(id_Ta_a(zarray).asInstanceOf[Array[Boolean]]);
    bcheck(id_Ta_a(barray).asInstanceOf[Array[Byte   ]]);
    scheck(id_Ta_a(sarray).asInstanceOf[Array[Short  ]]);
    ccheck(id_Ta_a(carray).asInstanceOf[Array[Char   ]]);
    icheck(id_Ta_a(iarray).asInstanceOf[Array[Int    ]]);
    lcheck(id_Ta_a(larray).asInstanceOf[Array[Long   ]]);
    fcheck(id_Ta_a(farray).asInstanceOf[Array[Float  ]]);
    dcheck(id_Ta_a(darray).asInstanceOf[Array[Double ]]);
    rcheck(id_Ta_a(rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_Ta_a(oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_Ta_a(marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_Ta_a(narray).asInstanceOf[Array[Strings]]);

    ucheck(id_Tr_a(uarray).asInstanceOf[Array[Unit   ]]);
    zcheck(id_Tr_a(zarray).asInstanceOf[Array[Boolean]]);
    bcheck(id_Tr_a(barray).asInstanceOf[Array[Byte   ]]);
    scheck(id_Tr_a(sarray).asInstanceOf[Array[Short  ]]);
    ccheck(id_Tr_a(carray).asInstanceOf[Array[Char   ]]);
    icheck(id_Tr_a(iarray).asInstanceOf[Array[Int    ]]);
    lcheck(id_Tr_a(larray).asInstanceOf[Array[Long   ]]);
    fcheck(id_Tr_a(farray).asInstanceOf[Array[Float  ]]);
    dcheck(id_Tr_a(darray).asInstanceOf[Array[Double ]]);
    rcheck(id_Tr_a(rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_Tr_a(oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_Tr_a(marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_Tr_a(narray).asInstanceOf[Array[Strings]]);

    ucheck(id_To_a(uarray).asInstanceOf[Array[Unit   ]]);
    zcheck(id_To_a(zarray).asInstanceOf[Array[Boolean]]);
    bcheck(id_To_a(barray).asInstanceOf[Array[Byte   ]]);
    scheck(id_To_a(sarray).asInstanceOf[Array[Short  ]]);
    ccheck(id_To_a(carray).asInstanceOf[Array[Char   ]]);
    icheck(id_To_a(iarray).asInstanceOf[Array[Int    ]]);
    lcheck(id_To_a(larray).asInstanceOf[Array[Long   ]]);
    fcheck(id_To_a(farray).asInstanceOf[Array[Float  ]]);
    dcheck(id_To_a(darray).asInstanceOf[Array[Double ]]);
    rcheck(id_To_a(rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_To_a(oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_To_a(marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_To_a(narray).asInstanceOf[Array[Strings]]);

    ucheck(id_Tr_r(uarray).asInstanceOf[Array[Unit   ]]);
    zcheck(id_Tr_r(zarray).asInstanceOf[Array[Boolean]]);
    bcheck(id_Tr_r(barray).asInstanceOf[Array[Byte   ]]);
    scheck(id_Tr_r(sarray).asInstanceOf[Array[Short  ]]);
    ccheck(id_Tr_r(carray).asInstanceOf[Array[Char   ]]);
    icheck(id_Tr_r(iarray).asInstanceOf[Array[Int    ]]);
    lcheck(id_Tr_r(larray).asInstanceOf[Array[Long   ]]);
    fcheck(id_Tr_r(farray).asInstanceOf[Array[Float  ]]);
    dcheck(id_Tr_r(darray).asInstanceOf[Array[Double ]]);
    rcheck(id_Tr_r(rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_Tr_r(oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_Tr_r(marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_Tr_r(narray).asInstanceOf[Array[Strings]]);

    ucheck(id_To_r(uarray).asInstanceOf[Array[Unit   ]]);
    zcheck(id_To_r(zarray).asInstanceOf[Array[Boolean]]);
    bcheck(id_To_r(barray).asInstanceOf[Array[Byte   ]]);
    scheck(id_To_r(sarray).asInstanceOf[Array[Short  ]]);
    ccheck(id_To_r(carray).asInstanceOf[Array[Char   ]]);
    icheck(id_To_r(iarray).asInstanceOf[Array[Int    ]]);
    lcheck(id_To_r(larray).asInstanceOf[Array[Long   ]]);
    fcheck(id_To_r(farray).asInstanceOf[Array[Float  ]]);
    dcheck(id_To_r(darray).asInstanceOf[Array[Double ]]);
    rcheck(id_To_r(rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_To_r(oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_To_r(marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_To_r(narray).asInstanceOf[Array[Strings]]);

    ucheck(id_To_o(uarray).asInstanceOf[Array[Unit   ]]);
    zcheck(id_To_o(zarray).asInstanceOf[Array[Boolean]]);
    bcheck(id_To_o(barray).asInstanceOf[Array[Byte   ]]);
    scheck(id_To_o(sarray).asInstanceOf[Array[Short  ]]);
    ccheck(id_To_o(carray).asInstanceOf[Array[Char   ]]);
    icheck(id_To_o(iarray).asInstanceOf[Array[Int    ]]);
    lcheck(id_To_o(larray).asInstanceOf[Array[Long   ]]);
    fcheck(id_To_o(farray).asInstanceOf[Array[Float  ]]);
    dcheck(id_To_o(darray).asInstanceOf[Array[Double ]]);
    rcheck(id_To_o(rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_To_o(oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_To_o(marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_To_o(narray).asInstanceOf[Array[Strings]]);

    //######################################################################

    ucheck(id_TSa_T [Unit   , Array[Unit   ]](uarray));
    zcheck(id_TSa_T [Boolean, Array[Boolean]](zarray));
    bcheck(id_TSa_T [Byte   , Array[Byte   ]](barray));
    scheck(id_TSa_T [Short  , Array[Short  ]](sarray));
    ccheck(id_TSa_T [Char   , Array[Char   ]](carray));
    icheck(id_TSa_T [Int    , Array[Int    ]](iarray));
    lcheck(id_TSa_T [Long   , Array[Long   ]](larray));
    fcheck(id_TSa_T [Float  , Array[Float  ]](farray));
    dcheck(id_TSa_T [Double , Array[Double ]](darray));
    rcheck(id_TSa_T [AnyRef , Array[AnyRef ]](rarray));
    ocheck(id_TSa_T [Object , Array[Object ]](oarray));
    mcheck(id_TSa_T [Map    , Array[Map    ]](marray));
    ncheck(id_TSa_T [Strings, Array[Strings]](narray));

    ucheck(id_TSv_T [Unit   , Array[Unit   ]](uarray));
    zcheck(id_TSv_T [Boolean, Array[Boolean]](zarray));
    bcheck(id_TSv_T [Byte   , Array[Byte   ]](barray));
    scheck(id_TSv_T [Short  , Array[Short  ]](sarray));
    ccheck(id_TSv_T [Char   , Array[Char   ]](carray));
    icheck(id_TSv_T [Int    , Array[Int    ]](iarray));
    lcheck(id_TSv_T [Long   , Array[Long   ]](larray));
    fcheck(id_TSv_T [Float  , Array[Float  ]](farray));
    dcheck(id_TSv_T [Double , Array[Double ]](darray));

    rcheck(id_TSr_T [AnyRef , Array[AnyRef ]](rarray));
    ocheck(id_TSr_T [Object , Array[Object ]](oarray));
    mcheck(id_TSr_T [Map    , Array[Map    ]](marray));
    ncheck(id_TSr_T [Strings, Array[Strings]](narray));

    rcheck(id_TSo_T [AnyRef , Array[AnyRef ]](rarray));
    ocheck(id_TSo_T [Object , Array[Object ]](oarray));
    mcheck(id_TSo_T [Map    , Array[Map    ]](marray));
    ncheck(id_TSo_T [Strings, Array[Strings]](narray));

    mcheck(id_TSm_T [Map    , Array[Map    ]](marray));

    ncheck(id_TSn_T [Strings, Array[Strings]](narray));

    //######################################################################

    ucheck(id_TSa_Ss[Unit   , Array[Unit   ]](uarray));
    zcheck(id_TSa_Ss[Boolean, Array[Boolean]](zarray));
    bcheck(id_TSa_Ss[Byte   , Array[Byte   ]](barray));
    scheck(id_TSa_Ss[Short  , Array[Short  ]](sarray));
    ccheck(id_TSa_Ss[Char   , Array[Char   ]](carray));
    icheck(id_TSa_Ss[Int    , Array[Int    ]](iarray));
    lcheck(id_TSa_Ss[Long   , Array[Long   ]](larray));
    fcheck(id_TSa_Ss[Float  , Array[Float  ]](farray));
    dcheck(id_TSa_Ss[Double , Array[Double ]](darray));
    rcheck(id_TSa_Ss[AnyRef , Array[AnyRef ]](rarray));
    ocheck(id_TSa_Ss[Object , Array[Object ]](oarray));
    mcheck(id_TSa_Ss[Map    , Array[Map    ]](marray));
    ncheck(id_TSa_Ss[Strings, Array[Strings]](narray));

    ucheck(id_TSv_Ss[Unit   , Array[Unit   ]](uarray));
    zcheck(id_TSv_Ss[Boolean, Array[Boolean]](zarray));
    bcheck(id_TSv_Ss[Byte   , Array[Byte   ]](barray));
    scheck(id_TSv_Ss[Short  , Array[Short  ]](sarray));
    ccheck(id_TSv_Ss[Char   , Array[Char   ]](carray));
    icheck(id_TSv_Ss[Int    , Array[Int    ]](iarray));
    lcheck(id_TSv_Ss[Long   , Array[Long   ]](larray));
    fcheck(id_TSv_Ss[Float  , Array[Float  ]](farray));
    dcheck(id_TSv_Ss[Double , Array[Double ]](darray));

    rcheck(id_TSr_Ss[AnyRef , Array[AnyRef ]](rarray));
    ocheck(id_TSr_Ss[Object , Array[Object ]](oarray));
    mcheck(id_TSr_Ss[Map    , Array[Map    ]](marray));
    ncheck(id_TSr_Ss[Strings, Array[Strings]](narray));

    rcheck(id_TSo_Ss[AnyRef , Array[AnyRef ]](rarray));
    ocheck(id_TSo_Ss[Object , Array[Object ]](oarray));
    mcheck(id_TSo_Ss[Map    , Array[Map    ]](marray));
    ncheck(id_TSo_Ss[Strings, Array[Strings]](narray));

    mcheck(id_TSm_Ss[Map    , Array[Map    ]](marray));

    ncheck(id_TSn_Ss[Strings, Array[Strings]](narray));

    //######################################################################

    ucheck(id_TSa_a [Unit   , Array[Unit   ]](uarray).asInstanceOf[Array[Unit   ]]);
    zcheck(id_TSa_a [Boolean, Array[Boolean]](zarray).asInstanceOf[Array[Boolean]]);
    bcheck(id_TSa_a [Byte   , Array[Byte   ]](barray).asInstanceOf[Array[Byte   ]]);
    scheck(id_TSa_a [Short  , Array[Short  ]](sarray).asInstanceOf[Array[Short  ]]);
    ccheck(id_TSa_a [Char   , Array[Char   ]](carray).asInstanceOf[Array[Char   ]]);
    icheck(id_TSa_a [Int    , Array[Int    ]](iarray).asInstanceOf[Array[Int    ]]);
    lcheck(id_TSa_a [Long   , Array[Long   ]](larray).asInstanceOf[Array[Long   ]]);
    fcheck(id_TSa_a [Float  , Array[Float  ]](farray).asInstanceOf[Array[Float  ]]);
    dcheck(id_TSa_a [Double , Array[Double ]](darray).asInstanceOf[Array[Double ]]);
    rcheck(id_TSa_a [AnyRef , Array[AnyRef ]](rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_TSa_a [Object , Array[Object ]](oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_TSa_a [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_TSa_a [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    ucheck(id_TSv_a [Unit   , Array[Unit   ]](uarray).asInstanceOf[Array[Unit   ]]);
    zcheck(id_TSv_a [Boolean, Array[Boolean]](zarray).asInstanceOf[Array[Boolean]]);
    bcheck(id_TSv_a [Byte   , Array[Byte   ]](barray).asInstanceOf[Array[Byte   ]]);
    scheck(id_TSv_a [Short  , Array[Short  ]](sarray).asInstanceOf[Array[Short  ]]);
    ccheck(id_TSv_a [Char   , Array[Char   ]](carray).asInstanceOf[Array[Char   ]]);
    icheck(id_TSv_a [Int    , Array[Int    ]](iarray).asInstanceOf[Array[Int    ]]);
    lcheck(id_TSv_a [Long   , Array[Long   ]](larray).asInstanceOf[Array[Long   ]]);
    fcheck(id_TSv_a [Float  , Array[Float  ]](farray).asInstanceOf[Array[Float  ]]);
    dcheck(id_TSv_a [Double , Array[Double ]](darray).asInstanceOf[Array[Double ]]);

    rcheck(id_TSr_a [AnyRef , Array[AnyRef ]](rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_TSr_a [Object , Array[Object ]](oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_TSr_a [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_TSr_a [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    rcheck(id_TSo_a [AnyRef , Array[AnyRef ]](rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_TSo_a [Object , Array[Object ]](oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_TSo_a [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_TSo_a [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    mcheck(id_TSm_a [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);

    ncheck(id_TSn_a [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    //######################################################################

    ucheck(id_TSa_r [Unit   , Array[Unit   ]](uarray).asInstanceOf[Array[Unit   ]]);
    zcheck(id_TSa_r [Boolean, Array[Boolean]](zarray).asInstanceOf[Array[Boolean]]);
    bcheck(id_TSa_r [Byte   , Array[Byte   ]](barray).asInstanceOf[Array[Byte   ]]);
    scheck(id_TSa_r [Short  , Array[Short  ]](sarray).asInstanceOf[Array[Short  ]]);
    ccheck(id_TSa_r [Char   , Array[Char   ]](carray).asInstanceOf[Array[Char   ]]);
    icheck(id_TSa_r [Int    , Array[Int    ]](iarray).asInstanceOf[Array[Int    ]]);
    lcheck(id_TSa_r [Long   , Array[Long   ]](larray).asInstanceOf[Array[Long   ]]);
    fcheck(id_TSa_r [Float  , Array[Float  ]](farray).asInstanceOf[Array[Float  ]]);
    dcheck(id_TSa_r [Double , Array[Double ]](darray).asInstanceOf[Array[Double ]]);
    rcheck(id_TSa_r [AnyRef , Array[AnyRef ]](rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_TSa_r [Object , Array[Object ]](oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_TSa_r [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_TSa_r [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    ucheck(id_TSv_r [Unit   , Array[Unit   ]](uarray).asInstanceOf[Array[Unit   ]]);
    zcheck(id_TSv_r [Boolean, Array[Boolean]](zarray).asInstanceOf[Array[Boolean]]);
    bcheck(id_TSv_r [Byte   , Array[Byte   ]](barray).asInstanceOf[Array[Byte   ]]);
    scheck(id_TSv_r [Short  , Array[Short  ]](sarray).asInstanceOf[Array[Short  ]]);
    ccheck(id_TSv_r [Char   , Array[Char   ]](carray).asInstanceOf[Array[Char   ]]);
    icheck(id_TSv_r [Int    , Array[Int    ]](iarray).asInstanceOf[Array[Int    ]]);
    lcheck(id_TSv_r [Long   , Array[Long   ]](larray).asInstanceOf[Array[Long   ]]);
    fcheck(id_TSv_r [Float  , Array[Float  ]](farray).asInstanceOf[Array[Float  ]]);
    dcheck(id_TSv_r [Double , Array[Double ]](darray).asInstanceOf[Array[Double ]]);

    rcheck(id_TSr_r [AnyRef , Array[AnyRef ]](rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_TSr_r [Object , Array[Object ]](oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_TSr_r [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_TSr_r [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    rcheck(id_TSo_r [AnyRef , Array[AnyRef ]](rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_TSo_r [Object , Array[Object ]](oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_TSo_r [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_TSo_r [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    mcheck(id_TSm_r [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);

    ncheck(id_TSn_r [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    //######################################################################

    ucheck(id_TSa_o [Unit   , Array[Unit   ]](uarray).asInstanceOf[Array[Unit   ]]);
    zcheck(id_TSa_o [Boolean, Array[Boolean]](zarray).asInstanceOf[Array[Boolean]]);
    bcheck(id_TSa_o [Byte   , Array[Byte   ]](barray).asInstanceOf[Array[Byte   ]]);
    scheck(id_TSa_o [Short  , Array[Short  ]](sarray).asInstanceOf[Array[Short  ]]);
    ccheck(id_TSa_o [Char   , Array[Char   ]](carray).asInstanceOf[Array[Char   ]]);
    icheck(id_TSa_o [Int    , Array[Int    ]](iarray).asInstanceOf[Array[Int    ]]);
    lcheck(id_TSa_o [Long   , Array[Long   ]](larray).asInstanceOf[Array[Long   ]]);
    fcheck(id_TSa_o [Float  , Array[Float  ]](farray).asInstanceOf[Array[Float  ]]);
    dcheck(id_TSa_o [Double , Array[Double ]](darray).asInstanceOf[Array[Double ]]);
    rcheck(id_TSa_o [AnyRef , Array[AnyRef ]](rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_TSa_o [Object , Array[Object ]](oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_TSa_o [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_TSa_o [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    ucheck(id_TSv_o [Unit   , Array[Unit   ]](uarray).asInstanceOf[Array[Unit   ]]);
    zcheck(id_TSv_o [Boolean, Array[Boolean]](zarray).asInstanceOf[Array[Boolean]]);
    bcheck(id_TSv_o [Byte   , Array[Byte   ]](barray).asInstanceOf[Array[Byte   ]]);
    scheck(id_TSv_o [Short  , Array[Short  ]](sarray).asInstanceOf[Array[Short  ]]);
    ccheck(id_TSv_o [Char   , Array[Char   ]](carray).asInstanceOf[Array[Char   ]]);
    icheck(id_TSv_o [Int    , Array[Int    ]](iarray).asInstanceOf[Array[Int    ]]);
    lcheck(id_TSv_o [Long   , Array[Long   ]](larray).asInstanceOf[Array[Long   ]]);
    fcheck(id_TSv_o [Float  , Array[Float  ]](farray).asInstanceOf[Array[Float  ]]);
    dcheck(id_TSv_o [Double , Array[Double ]](darray).asInstanceOf[Array[Double ]]);

    rcheck(id_TSr_o [AnyRef , Array[AnyRef ]](rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_TSr_o [Object , Array[Object ]](oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_TSr_o [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_TSr_o [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    rcheck(id_TSo_o [AnyRef , Array[AnyRef ]](rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_TSo_o [Object , Array[Object ]](oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_TSo_o [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_TSo_o [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    mcheck(id_TSm_o [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);

    ncheck(id_TSn_o [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    //######################################################################

    ucheck(id_Sas_Ss[Unit   ](uarray));
    zcheck(id_Sas_Ss[Boolean](zarray));
    bcheck(id_Sas_Ss[Byte   ](barray));
    scheck(id_Sas_Ss[Short  ](sarray));
    ccheck(id_Sas_Ss[Char   ](carray));
    icheck(id_Sas_Ss[Int    ](iarray));
    lcheck(id_Sas_Ss[Long   ](larray));
    fcheck(id_Sas_Ss[Float  ](farray));
    dcheck(id_Sas_Ss[Double ](darray));
    rcheck(id_Sas_Ss[AnyRef ](rarray));
    ocheck(id_Sas_Ss[Object ](oarray));
    mcheck(id_Sas_Ss[Map    ](marray));
    ncheck(id_Sas_Ss[Strings](narray));

    ucheck(id_Svs_Ss[Unit   ](uarray));
    zcheck(id_Svs_Ss[Boolean](zarray));
    bcheck(id_Svs_Ss[Byte   ](barray));
    scheck(id_Svs_Ss[Short  ](sarray));
    ccheck(id_Svs_Ss[Char   ](carray));
    icheck(id_Svs_Ss[Int    ](iarray));
    lcheck(id_Svs_Ss[Long   ](larray));
    fcheck(id_Svs_Ss[Float  ](farray));
    dcheck(id_Svs_Ss[Double ](darray));

    rcheck(id_Srs_Ss[AnyRef ](rarray));
    ocheck(id_Srs_Ss[Object ](oarray));
    mcheck(id_Srs_Ss[Map    ](marray));
    ncheck(id_Srs_Ss[Strings](narray));

    rcheck(id_Sos_Ss[AnyRef ](rarray));
    ocheck(id_Sos_Ss[Object ](oarray));
    mcheck(id_Sos_Ss[Map    ](marray));
    ncheck(id_Sos_Ss[Strings](narray));

    mcheck(id_Sms_Ss[Map    ](marray));

    ncheck(id_Sns_Ss[Strings](narray));

    //######################################################################

    ucheck(id_TSa_a [Unit   , Array[Unit   ]](uarray).asInstanceOf[Array[Unit   ]]);
    zcheck(id_TSa_a [Boolean, Array[Boolean]](zarray).asInstanceOf[Array[Boolean]]);
    bcheck(id_TSa_a [Byte   , Array[Byte   ]](barray).asInstanceOf[Array[Byte   ]]);
    scheck(id_TSa_a [Short  , Array[Short  ]](sarray).asInstanceOf[Array[Short  ]]);
    ccheck(id_TSa_a [Char   , Array[Char   ]](carray).asInstanceOf[Array[Char   ]]);
    icheck(id_TSa_a [Int    , Array[Int    ]](iarray).asInstanceOf[Array[Int    ]]);
    lcheck(id_TSa_a [Long   , Array[Long   ]](larray).asInstanceOf[Array[Long   ]]);
    fcheck(id_TSa_a [Float  , Array[Float  ]](farray).asInstanceOf[Array[Float  ]]);
    dcheck(id_TSa_a [Double , Array[Double ]](darray).asInstanceOf[Array[Double ]]);
    rcheck(id_TSa_a [AnyRef , Array[AnyRef ]](rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_TSa_a [Object , Array[Object ]](oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_TSa_a [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_TSa_a [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    ucheck(id_TSv_a [Unit   , Array[Unit   ]](uarray).asInstanceOf[Array[Unit   ]]);
    zcheck(id_TSv_a [Boolean, Array[Boolean]](zarray).asInstanceOf[Array[Boolean]]);
    bcheck(id_TSv_a [Byte   , Array[Byte   ]](barray).asInstanceOf[Array[Byte   ]]);
    scheck(id_TSv_a [Short  , Array[Short  ]](sarray).asInstanceOf[Array[Short  ]]);
    ccheck(id_TSv_a [Char   , Array[Char   ]](carray).asInstanceOf[Array[Char   ]]);
    icheck(id_TSv_a [Int    , Array[Int    ]](iarray).asInstanceOf[Array[Int    ]]);
    lcheck(id_TSv_a [Long   , Array[Long   ]](larray).asInstanceOf[Array[Long   ]]);
    fcheck(id_TSv_a [Float  , Array[Float  ]](farray).asInstanceOf[Array[Float  ]]);
    dcheck(id_TSv_a [Double , Array[Double ]](darray).asInstanceOf[Array[Double ]]);

    rcheck(id_TSr_a [AnyRef , Array[AnyRef ]](rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_TSr_a [Object , Array[Object ]](oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_TSr_a [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_TSr_a [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    rcheck(id_TSo_a [AnyRef , Array[AnyRef ]](rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_TSo_a [Object , Array[Object ]](oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_TSo_a [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_TSo_a [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    mcheck(id_TSm_a [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);

    ncheck(id_TSn_a [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    //######################################################################

    ucheck(id_TSa_r [Unit   , Array[Unit   ]](uarray).asInstanceOf[Array[Unit   ]]);
    zcheck(id_TSa_r [Boolean, Array[Boolean]](zarray).asInstanceOf[Array[Boolean]]);
    bcheck(id_TSa_r [Byte   , Array[Byte   ]](barray).asInstanceOf[Array[Byte   ]]);
    scheck(id_TSa_r [Short  , Array[Short  ]](sarray).asInstanceOf[Array[Short  ]]);
    ccheck(id_TSa_r [Char   , Array[Char   ]](carray).asInstanceOf[Array[Char   ]]);
    icheck(id_TSa_r [Int    , Array[Int    ]](iarray).asInstanceOf[Array[Int    ]]);
    lcheck(id_TSa_r [Long   , Array[Long   ]](larray).asInstanceOf[Array[Long   ]]);
    fcheck(id_TSa_r [Float  , Array[Float  ]](farray).asInstanceOf[Array[Float  ]]);
    dcheck(id_TSa_r [Double , Array[Double ]](darray).asInstanceOf[Array[Double ]]);
    rcheck(id_TSa_r [AnyRef , Array[AnyRef ]](rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_TSa_r [Object , Array[Object ]](oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_TSa_r [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_TSa_r [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    ucheck(id_TSv_r [Unit   , Array[Unit   ]](uarray).asInstanceOf[Array[Unit   ]]);
    zcheck(id_TSv_r [Boolean, Array[Boolean]](zarray).asInstanceOf[Array[Boolean]]);
    bcheck(id_TSv_r [Byte   , Array[Byte   ]](barray).asInstanceOf[Array[Byte   ]]);
    scheck(id_TSv_r [Short  , Array[Short  ]](sarray).asInstanceOf[Array[Short  ]]);
    ccheck(id_TSv_r [Char   , Array[Char   ]](carray).asInstanceOf[Array[Char   ]]);
    icheck(id_TSv_r [Int    , Array[Int    ]](iarray).asInstanceOf[Array[Int    ]]);
    lcheck(id_TSv_r [Long   , Array[Long   ]](larray).asInstanceOf[Array[Long   ]]);
    fcheck(id_TSv_r [Float  , Array[Float  ]](farray).asInstanceOf[Array[Float  ]]);
    dcheck(id_TSv_r [Double , Array[Double ]](darray).asInstanceOf[Array[Double ]]);

    rcheck(id_TSr_r [AnyRef , Array[AnyRef ]](rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_TSr_r [Object , Array[Object ]](oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_TSr_r [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_TSr_r [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    rcheck(id_TSo_r [AnyRef , Array[AnyRef ]](rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_TSo_r [Object , Array[Object ]](oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_TSo_r [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_TSo_r [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    mcheck(id_TSm_r [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);

    ncheck(id_TSn_r [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    //######################################################################

    ucheck(id_TSa_o [Unit   , Array[Unit   ]](uarray).asInstanceOf[Array[Unit   ]]);
    zcheck(id_TSa_o [Boolean, Array[Boolean]](zarray).asInstanceOf[Array[Boolean]]);
    bcheck(id_TSa_o [Byte   , Array[Byte   ]](barray).asInstanceOf[Array[Byte   ]]);
    scheck(id_TSa_o [Short  , Array[Short  ]](sarray).asInstanceOf[Array[Short  ]]);
    ccheck(id_TSa_o [Char   , Array[Char   ]](carray).asInstanceOf[Array[Char   ]]);
    icheck(id_TSa_o [Int    , Array[Int    ]](iarray).asInstanceOf[Array[Int    ]]);
    lcheck(id_TSa_o [Long   , Array[Long   ]](larray).asInstanceOf[Array[Long   ]]);
    fcheck(id_TSa_o [Float  , Array[Float  ]](farray).asInstanceOf[Array[Float  ]]);
    dcheck(id_TSa_o [Double , Array[Double ]](darray).asInstanceOf[Array[Double ]]);
    rcheck(id_TSa_o [AnyRef , Array[AnyRef ]](rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_TSa_o [Object , Array[Object ]](oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_TSa_o [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_TSa_o [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    ucheck(id_TSv_o [Unit   , Array[Unit   ]](uarray).asInstanceOf[Array[Unit   ]]);
    zcheck(id_TSv_o [Boolean, Array[Boolean]](zarray).asInstanceOf[Array[Boolean]]);
    bcheck(id_TSv_o [Byte   , Array[Byte   ]](barray).asInstanceOf[Array[Byte   ]]);
    scheck(id_TSv_o [Short  , Array[Short  ]](sarray).asInstanceOf[Array[Short  ]]);
    ccheck(id_TSv_o [Char   , Array[Char   ]](carray).asInstanceOf[Array[Char   ]]);
    icheck(id_TSv_o [Int    , Array[Int    ]](iarray).asInstanceOf[Array[Int    ]]);
    lcheck(id_TSv_o [Long   , Array[Long   ]](larray).asInstanceOf[Array[Long   ]]);
    fcheck(id_TSv_o [Float  , Array[Float  ]](farray).asInstanceOf[Array[Float  ]]);
    dcheck(id_TSv_o [Double , Array[Double ]](darray).asInstanceOf[Array[Double ]]);

    rcheck(id_TSr_o [AnyRef , Array[AnyRef ]](rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_TSr_o [Object , Array[Object ]](oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_TSr_o [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_TSr_o [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    rcheck(id_TSo_o [AnyRef , Array[AnyRef ]](rarray).asInstanceOf[Array[AnyRef ]]);
    ocheck(id_TSo_o [Object , Array[Object ]](oarray).asInstanceOf[Array[Object ]]);
    mcheck(id_TSo_o [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);
    ncheck(id_TSo_o [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    mcheck(id_TSm_o [Map    , Array[Map    ]](marray).asInstanceOf[Array[Map    ]]);

    ncheck(id_TSn_o [Strings, Array[Strings]](narray).asInstanceOf[Array[Strings]]);

    //######################################################################

    check_Ta(uarray, 2, u0, ucheck)
    check_Ta(zarray, 2, z0, zcheck)
    check_Ta(barray, 3, b0, bcheck)
    check_Ta(sarray, 3, s0, scheck)
    check_Ta(carray, 3, c0, ccheck)
    check_Ta(iarray, 3, i0, icheck)
    check_Ta(larray, 3, l0, lcheck)
    check_Ta(farray, 3, f0, fcheck)
    check_Ta(darray, 3, d0, dcheck)
    check_Ta(rarray, 6, r0, rcheck)
    check_Ta(oarray, 6, o0, ocheck)
    check_Ta(marray, 3, m0, mcheck)
    check_Ta(narray, 3, n0, ncheck)

    check_Tv(uarray, 2, u0, ucheck)
    check_Tv(zarray, 2, z0, zcheck)
    check_Tv(barray, 3, b0, bcheck)
    check_Tv(sarray, 3, s0, scheck)
    check_Tv(carray, 3, c0, ccheck)
    check_Tv(iarray, 3, i0, icheck)
    check_Tv(larray, 3, l0, lcheck)
    check_Tv(farray, 3, f0, fcheck)
    check_Tv(darray, 3, d0, dcheck)

    check_Tr(rarray, 6, r0, rcheck)
    check_Tr(oarray, 6, o0, ocheck)
    check_Tr(marray, 3, m0, mcheck)
    check_Tr(narray, 3, n0, ncheck)

    check_To(rarray, 6, r0, rcheck)
    check_To(oarray, 6, o0, ocheck)
    check_To(marray, 3, m0, mcheck)
    check_To(narray, 3, n0, ncheck)

    check_Tm(marray, 3, m0, mcheck)

    check_Tn(narray, 3, n0, ncheck)

    //######################################################################

    checkZip
    checkConcat
    checkT2368()

    //######################################################################

    println("checks: " + checks)

    //######################################################################
  }

  //##########################################################################
}


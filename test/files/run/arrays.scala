//############################################################################
// Arrays
//############################################################################
// $Id$

//############################################################################

object arrays {

  type JObject = java.lang.Object;
  type JSet    = java.util.Set;

  def check(what: String, actual: Any, expected: Any): Unit = {
    val success: Boolean = actual == expected;
    System.out.print(if (success) "ok" else "KO");
    var value: String = if (actual == null) "null" else actual.toString();
    if (value == "\u0000") value = "\\u0000";
    System.out.print(": " + what + " = " + value);
    if (!success) System.out.print(" != " + expected);
    System.out.println();
    System.out.flush();
  }

  def check1(name: String, array: Array[Any], i: Int, exp: Any): Unit = {
    check (name+"("+i+")", array(i), exp);
  }

  def check2(name: String, array: Array[AnyVal], i: Int, exp: AnyVal): Unit = {
    check (name+"("+i+")", array(i), exp);
  }

  def check3(name: String, array: Array[AnyRef], i: Int, exp: AnyRef): Unit = {
    check (name+"("+i+")", array(i), exp);
  }

  def check4(name: String, array: Array[Object], i: Int, exp: Object): Unit = {
    check (name+"("+i+")", array(i), exp);
  }

  def check5(name: String, array: Array[JObject], i: Int, exp: JObject): Unit={
    check (name+"("+i+")", array(i), exp);
  }

  def check6(name: String, array: Array[Double], i: Int, exp: Double): Unit = {
    check (name+"("+i+")", array(i), exp);
  }

  def test: Unit = {
    val zarray: Array[Boolean] = new Array[Boolean](2);
    val barray: Array[Byte] = new Array[Byte](2);
    val sarray: Array[Short] = new Array[Short](2);
    val carray: Array[Char] = new Array[Char](2);
    val iarray: Array[Int] = new Array[Int](2);
    val larray: Array[Long] = new Array[Long](2);
    val farray: Array[Float] = new Array[Float](2);
    val darray: Array[Double] = new Array[Double](2);
    // !!! test Array[Any]
    // !!! test Array[AnyVal] ?
    val rarray: Array[AnyRef] = new Array[AnyRef](2);
    // !!! test Array[Object]
    // !!! test Array[List]
    // !!! test Array[JavaObject]
    // !!! test Array[JavaSet]

    check("zarray.length", zarray.length, 2);
    check("barray.length", barray.length, 2);
    check("sarray.length", sarray.length, 2);
    check("carray.length", carray.length, 2);
    check("iarray.length", iarray.length, 2);
    check("larray.length", larray.length, 2);
    check("farray.length", farray.length, 2);
    check("darray.length", darray.length, 2);
    check("rarray.length", rarray.length, 2);
    System.out.println();

    check("zarray(0)", zarray(0), false);
    check("barray(0)", barray(0), 0);
    check("sarray(0)", sarray(0), 0);
    check("carray(0)", carray(0), 0);
    check("iarray(0)", iarray(0), 0);
    check("larray(0)", larray(0), 0);
    check("farray(0)", farray(0), 0);
    check("darray(0)", darray(0), 0);
    check("rarray(0)", rarray(0), null);
    System.out.println();

    check("zarray(1)", zarray(1), false);
    check("barray(1)", barray(1), 0);
    check("sarray(1)", sarray(1), 0);
    check("carray(1)", carray(1), 0);
    check("iarray(1)", iarray(1), 0);
    check("larray(1)", larray(1), 0);
    check("farray(1)", farray(1), 0);
    check("darray(1)", darray(1), 0);
    check("rarray(1)", rarray(1), null);
    System.out.println();

    zarray(0) = false;
    barray(0) = 1 as Byte;
    sarray(0) = 2 as Short;
    carray(0) ='3';
    iarray(0) = 4;
    larray(0) = 5;
    farray(0) = 6;
    darray(0) = 7;
    rarray(0) ="8";

    zarray(1) = true;
    barray(1) = 2 as Byte;
    sarray(1) = 3 as Short;
    carray(1) ='4';
    iarray(1) = 5;
    larray(1) = 6;
    farray(1) = 7;
    darray(1) = 8;
    rarray(1) ="9";

    check("zarray(0)", zarray(0), false);
    check("barray(0)", barray(0), 1);
    check("sarray(0)", sarray(0), 2);
    check("carray(0)", carray(0),'3');
    check("iarray(0)", iarray(0), 4);
    check("larray(0)", larray(0), 5);
    check("farray(0)", farray(0), 6);
    check("darray(0)", darray(0), 7);
    check("rarray(0)", rarray(0),"8");
    System.out.println();

    check("zarray(1)", zarray(1), true);
    check("barray(1)", barray(1), 2);
    check("sarray(1)", sarray(1), 3);
    check("carray(1)", carray(1),'4');
    check("iarray(1)", iarray(1), 5);
    check("larray(1)", larray(1), 6);
    check("farray(1)", farray(1), 7);
    check("darray(1)", darray(1), 8);
    check("rarray(1)", rarray(1),"9");
    System.out.println();

    // !!! check1("zarray(1)", zarray, 1, true);
    // !!! check2("zarray(1)", zarray, 1, true);
    // !!! ...

  }
}

//############################################################################

object Test {

  def main(args: Array[String]): Unit = {
    arrays.test;
  }
}

//############################################################################

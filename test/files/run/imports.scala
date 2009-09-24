//############################################################################
// Import statements
//############################################################################

//############################################################################

object checker {
  def check(location: String, what: String, value: Any): Unit = {
    Console.print("In " + location + ", " + what + ".toString() returns ");
    Console.flush;
    val string: String = if (value == null) "null" else value.toString();
    val test = if (string == location) "ok" else "KO";
    Console.println(string + " -> " + test);
    Console.flush;
  }
}

import checker.check;

//############################################################################

//import o_ico.v_ico;

class C_ico() {
  o_ico.v_ico = this;
  import o_ico.v_ico;
  override def toString(): String = "C_ico";
  def method: C_ico = v_ico;
  val field: C_ico = v_ico;

  check("C_ico", "v_ico ", v_ico);
  check("C_ico", "field ", field);
  check("C_ico", "method", method);
  Console.println;
}

object o_ico {
  var v_ico: C_ico = null;
  new C_ico();
}

//############################################################################

object o_ioc {
  var v_ioc: C_ioc = null;
  new C_ioc();
}

import o_ioc.v_ioc;


class C_ioc() {
  o_ioc.v_ioc = this;
  override def toString(): String = "C_ioc";
  def method: C_ioc = v_ioc;
  val field: C_ioc = v_ioc;

  check("C_ioc", "v_ioc ", v_ioc);
  check("C_ioc", "field ", field);
  check("C_ioc", "method", method);
  Console.println;
}

//############################################################################

object o_oic {
  var v_oic: C_oic = null;
  new C_oic();
}

import o_oic.v_oic;

class C_oic() {
  o_oic.v_oic = this;
  override def toString(): String = "C_oic";
  def method: C_oic = v_oic;
  val field: C_oic = v_oic;

  check("C_oic", "v_oic ", v_oic);
  check("C_oic", "field ", field);
  check("C_oic", "method", method);
  Console.println;
}

//############################################################################

object Test {
  def main(args: Array[String]): Unit = {
    o_ico;
    o_ioc;
    o_oic;
    ()
  }
}

//############################################################################

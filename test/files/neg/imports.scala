//############################################################################
// Import statements
//############################################################################
// $Id$

//############################################################################

object checker {
  def check(where: String, what: String, value: Any): Unit = {
    System.out.print("In " + where + ", " + what + ".toString() returns ");
    System.out.flush();
    val string: String = if (value == null) "null" else value.toString();
    val test = if (string == where) "ok" else "KO";
    System.out.println(string + " -> " + test);
    System.out.flush();
  }
}

import checker.check;

//############################################################################

class C_coi() {
  o_coi.v_coi = this;
  override def toString(): String = "C_coi";
  def method: C_coi = v_coi;
  val field: C_coi = v_coi;

  check("C_coi", "v_coi ", v_coi);
  check("C_coi", "field ", field);
  check("C_coi", "method", method);
  System.out.println();
}

object o_coi {
  var v_coi: C_coi = null;
  new C_coi();
}

import o_coi.v_coi;

//############################################################################

class C_cio() {
  o_cio.v_cio = this;
  override def toString(): String = "C_cio";
  def method: C_cio = v_cio;
  val field: C_cio = v_cio;

  check("C_cio", "v_cio ", v_cio);
  check("C_cio", "field ", field);
  check("C_cio", "method", method);
  System.out.println();
}

import o_cio.v_cio;

object o_cio {
  var v_cio: C_cio = null;
  new C_cio();
}

//############################################################################

import o_ico.v_ico;

class C_ico() {
  o_ico.v_ico = this;
  override def toString(): String = "C_ico";
  def method: C_ico = v_ico;
  val field: C_ico = v_ico;

  check("C_ico", "v_ico ", v_ico);
  check("C_ico", "field ", field);
  check("C_ico", "method", method);
  System.out.println();
}

object o_ico {
  var v_ico: C_ico = null;
  new C_ico();
}

//############################################################################

import o_ioc.v_ioc;

object o_ioc {
  var v_ioc: C_ioc = null;
  new C_ioc();
}

class C_ioc() {
  o_ioc.v_ioc = this;
  override def toString(): String = "C_ioc";
  def method: C_ioc = v_ioc;
  val field: C_ioc = v_ioc;

  check("C_ioc", "v_ioc ", v_ioc);
  check("C_ioc", "field ", field);
  check("C_ioc", "method", method);
  System.out.println();
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
  System.out.println();
}

//############################################################################

object o_oci {
  var v_oci: C_oci = null;
  new C_oci();
}

class C_oci() {
  o_oci.v_oci = this;
  override def toString(): String = "C_oci";
  def method: C_oci = v_oci;
  val field: C_oci = v_oci;

  check("C_oci", "v_oci ", v_oci);
  check("C_oci", "field ", field);
  check("C_oci", "method", method);
  System.out.println();
}

import o_oci.v_oci;

//############################################################################

object Test {
  def main(args: Array[String]): Unit = {
    o_coi;
    o_cio;
    o_ico;
    o_ioc;
    o_oic;
    o_oci;
    ()
  }
}

//############################################################################

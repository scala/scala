/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc;


import java.math.{BigDecimal, BigInteger};


/** This class ..
 *
 */
@deprecated(DbcIsDeprecated, "2.9.0") object Syntax {

  import syntax.DataTypeUtil;

  /* Data types */
  def boolean  = DataTypeUtil.boolean;
  def tinyint  = DataTypeUtil.tinyint;
  def smallint = DataTypeUtil.smallint;
  def integer  = DataTypeUtil.integer;
  def bigint   = DataTypeUtil.bigint;
  def real     = DataTypeUtil.real;

  def numeric(precision: Int) = DataTypeUtil.numeric(precision);
  def numeric(precision: Int, scale: Int) = DataTypeUtil.numeric(precision, scale);

  def doublePrecision = DataTypeUtil.doublePrecision;
  def character(length: Int) = DataTypeUtil.character(length);
  def characterVarying(length: Int) = DataTypeUtil.characterVarying(length);
  def characterLargeObject = DataTypeUtil.characterLargeObject;

  /* Statements */
  //def select

  /* Other stuff */
  def database (server: String, username: String, password: String): dbc.Database =
    syntax.Database.database(server, username, password);

}

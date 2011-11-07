package com.sap.dspace.model.othello;

import scala.xml._

trait XMLRenderer {
  type T <: {def getClass() : java.lang.Class[_]}
  val valueTypes =
    List(
      classOf[java.lang.Boolean],
      classOf[java.lang.Integer],
      classOf[java.lang.Float],
      classOf[java.lang.String] 
      // more to come
      )

  def value2XML(
    value : Object,
    field : java.lang.reflect.Field,
    pojo : T
    ) : Node = {
      value match {
	case null => Text( "null" )
	case vUnmatched =>
	  if (value.isInstanceOf[java.lang.Boolean]) 
	    Text( value.asInstanceOf[java.lang.Boolean].toString )
	  else if (value.isInstanceOf[java.lang.Integer]) 
	    Text( value.asInstanceOf[java.lang.Integer].toString )
	  else if (value.isInstanceOf[java.lang.Float]) 
	    Text( value.asInstanceOf[java.lang.Float].toString )
    // else if (value.isInstanceOf[T]) 
    //   pojo2XML( value.asInstanceOf[T] ) 
	  else
	  <unmatchedType>
	    <theType>
	      {vUnmatched.getClass.toString}
	    </theType>
	    <theValue>
	      {vUnmatched.toString}
	    </theValue>
	  </unmatchedType>
      }
    }

  def field2XML(
    field : java.lang.reflect.Field,
    pojo : T
  ) : Elem = {

    val accessible = field.isAccessible;
    field.setAccessible( true );
    // BUGBUG lgm need to disambiguate on type and possibly make
    // recursive call to pojo2XML
    val fldValXML = value2XML( field.get( pojo ), field, pojo );
    field.setAccessible( accessible );

    Elem(
      null,
      field.getName,
      null,
      TopScope, 
      fldValXML
      )
  }

  def pojo2XML( pojo : T ) : Elem = {
    val progeny =
      for (field <- pojo.getClass.getDeclaredFields)
      yield field2XML( field, pojo );

    Elem(
      null,
      pojo.getClass.getName,
      null,
      TopScope,
      progeny.asInstanceOf[Array[scala.xml.Node]] : _*
      )    
  }
}

case class POJO2XMLRenderer( recurse : Boolean )
     extends XMLRenderer {
       type T = java.io.Serializable
  override def value2XML(
    value : Object,
    field : java.lang.reflect.Field,
    pojo : java.io.Serializable
    ) : Node = {
      if (recurse) super.value2XML( value, field, pojo )
      else Text( value + "" )
    }
}

object thePOJO2XMLRenderer extends POJO2XMLRenderer( true ) {
}

object Test extends Application {
  println(com.sap.dspace.model.othello.thePOJO2XMLRenderer)
}

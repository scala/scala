/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.dbc.syntax;


import java.math.BigDecimal;
import java.math.BigInteger;

import StatementExpression._;

/*

ASSUMPTIONS:

IMPROVABLE:
For type safety, all types must be defined. If one is missing, none is taken into account.
It is possible to redefine many types or renamings for a field, in that case,
  only the last one is taken into account ("a" as "b" as "c" of boolean as "e" of integer
  is equivalent to "a" as "e" of integer).

FIXED:

*/

object Statement {

  // SELECT ZYGOTE ...

  def select: SelectZygote = new SelectZygote {
    val setQuantifier = None;
  }
  def selectBag: SelectZygote = new SelectZygote {
    val setQuantifier = Some(statement.SetQuantifier.AllTuples);
  }
  def selectSet: SelectZygote = new SelectZygote {
    val setQuantifier = Some(statement.SetQuantifier.DistinctTuples);
  }

  abstract class SelectZygote {
    def setQuantifier: Option[statement.SetQuantifier];
    def fields (sdc:SelectDerivedColumns): SelectOf = new SelectOf {
      val setQuantifier = SelectZygote.this.setQuantifier;
      val selectList = sdc.selectList;
      val selectTypes = sdc.selectTypes;
    }
  }

  abstract class SelectDerivedField {
    def fieldValue: StatementField;
    def fieldRename: Option[String] = {val x = None; x}
    def fieldType: Option[dbc.DataType] = {val x = None; x}
    def as (rename:String): SelectDerivedField = new SelectDerivedField {
      val fieldValue = SelectDerivedField.this.fieldValue;
      override val fieldRename = Some(rename);
      override val fieldType = SelectDerivedField.this.fieldType;
    }
    def of (datatype:dbc.DataType): SelectDerivedField = new SelectDerivedField {
      val fieldValue = SelectDerivedField.this.fieldValue;
      override val fieldRename = SelectDerivedField.this.fieldRename;
      override val fieldType = Some(datatype);
    }
  }

  implicit def view1 (fv:StatementField): SelectDerivedField = new SelectDerivedField {
    val fieldValue = fv;
  }

  implicit def view2 (fv:String): SelectDerivedField = new SelectDerivedField {
    val fieldValue: StatementField = StatementExpression.view(fv);
  }

  abstract class SelectDerivedColumns {
    def selectList: List[statement.DerivedColumn];
    def selectTypes: List[DataType];
    def and (sdc:SelectDerivedColumns): SelectDerivedColumns = new SelectDerivedColumns {
      val selectList = SelectDerivedColumns.this.selectList ::: sdc.selectList;
      val selectTypes =
	if (SelectDerivedColumns.this.selectTypes.isEmpty | sdc.selectTypes.isEmpty)
	  Nil
	else
	  SelectDerivedColumns.this.selectTypes ::: sdc.selectTypes;
    }
  }

  implicit def view3 (sdf:SelectDerivedField): SelectDerivedColumns = new SelectDerivedColumns {
    val selectList = List(new statement.DerivedColumn {
      val valueExpression = sdf.fieldValue.toStatement;
      val asClause = sdf.fieldRename;
    });
    val selectTypes = if (sdf.fieldType.isEmpty) Nil else List(sdf.fieldType.get);
  }

  implicit def view4 (sdfs:String): SelectDerivedColumns = {
    val sdf: SelectDerivedField = sdfs;
    view3(sdf);
  }

  // SELECT OF ...

  abstract class SelectOf {
    def setQuantifier: Option[statement.SetQuantifier];
    def selectList: List[statement.DerivedColumn];
    def selectTypes: List[DataType];
    def from (sst:SelectSourceTables): SelectBeyond = new SelectBeyond {
      val setQuantifier = SelectOf.this.setQuantifier;
      val selectList = SelectOf.this.selectList;
      val selectTypes = SelectOf.this.selectTypes;
      val fromClause = sst.fromClause;
      val whereClause = None;
      val groupByClause = None;
      val havingClause = None;
    }
  }

  abstract class SelectSourceTable {
    def fromRelation: statement.Relation;
    def innerJoin (sst: SelectSourceTable): SelectSourceTable = new SelectSourceTable {
      val fromRelation = new statement.Jointure {
	val leftRelation = SelectSourceTable.this.fromRelation;
	val rightRelation = sst.fromRelation;
	val joinType = statement.JoinType.Inner;
	val joinCondition = None;
	val fieldTypes = leftRelation.fieldTypes ::: rightRelation.fieldTypes;
      }
    }
    def leftOuterJoin (sst: SelectSourceTable): SelectSourceTable = new SelectSourceTable {
      val fromRelation = new statement.Jointure {
	val leftRelation = SelectSourceTable.this.fromRelation;
	val rightRelation = sst.fromRelation;
	val joinType = statement.JoinType.Outer.Left;
	val joinCondition = None;
	val fieldTypes = leftRelation.fieldTypes ::: rightRelation.fieldTypes;
      }
    }
    def rightOuterJoin (sst: SelectSourceTable): SelectSourceTable = new SelectSourceTable {
      val fromRelation = new statement.Jointure {
	val leftRelation = SelectSourceTable.this.fromRelation;
	val rightRelation = sst.fromRelation;
	val joinType = statement.JoinType.Outer.Right;
	val joinCondition = None;
	val fieldTypes = leftRelation.fieldTypes ::: rightRelation.fieldTypes;
      }
    }
    def fullOuterJoin (sst: SelectSourceTable): SelectSourceTable = new SelectSourceTable {
      val fromRelation = new statement.Jointure {
	val leftRelation = SelectSourceTable.this.fromRelation;
	val rightRelation = sst.fromRelation;
	val joinType = statement.JoinType.Outer.Full;
	val joinCondition = None;
	val fieldTypes = leftRelation.fieldTypes ::: rightRelation.fieldTypes;
      }
    }
  }

  implicit def view5 (sct:String): SelectSourceTable = new SelectSourceTable {
    val fromRelation = new statement.Table {
      val tableName = sct;
      val tableRename = None;
      val fieldTypes = Nil;
    }
  }

  implicit def view6 (sct:statement.Select): SelectSourceTable = new SelectSourceTable {
    val fromRelation = sct;
  }

  abstract class SelectSourceTables {
    def fromClause: List[statement.Relation];
    def join (sct:SelectSourceTable): SelectSourceTables = new SelectSourceTables {
      val fromClause = SelectSourceTables.this.fromClause ::: List(sct.fromRelation);
    }
  }

  implicit def view7 (sct:String): SelectSourceTables = new SelectSourceTables {
    val fromClause = List(new statement.Table {
      val tableName = sct;
      val tableRename = None;
      val fieldTypes = Nil;
    });
  }

  implicit def view8 (sct:statement.Select): SelectSourceTables = new SelectSourceTables {
    val fromClause = List(sct);
  }

  implicit def view9 (sct:SelectSourceTable): SelectSourceTables = new SelectSourceTables {
    val fromClause = List(sct.fromRelation);
  }

  // SELECT BEYOND ...

  abstract class SelectBeyond {
    def setQuantifier: Option[statement.SetQuantifier];
    def selectList: List[statement.DerivedColumn];
    def selectTypes: List[DataType];
    def fromClause: List[statement.Relation];
    def whereClause: Option[statement.Expression];
    def groupByClause: Option[List[statement.Expression]];
    def havingClause: Option[statement.Expression];
    def where (se:StatementExpression): SelectBeyond = new SelectBeyond {
      val setQuantifier = SelectBeyond.this.setQuantifier;
      val selectList = SelectBeyond.this.selectList;
      val selectTypes = SelectBeyond.this.selectTypes;
      val fromClause = SelectBeyond.this.fromClause;
      val whereClause = Some(se.toStatement);
      val groupByClause = SelectBeyond.this.groupByClause;
      val havingClause = SelectBeyond.this.havingClause;
    }
    def groupBy (sgb:SelectGroupBy): SelectBeyond = new SelectBeyond {
      val setQuantifier = SelectBeyond.this.setQuantifier;
      val selectList = SelectBeyond.this.selectList;
      val selectTypes = SelectBeyond.this.selectTypes;
      val fromClause = SelectBeyond.this.fromClause;
      val whereClause = SelectBeyond.this.whereClause;
      val groupByClause = Some(sgb.groupByClause);
      val havingClause = SelectBeyond.this.havingClause;
    }
    def having (se:StatementExpression): SelectBeyond = new SelectBeyond {
      val setQuantifier = SelectBeyond.this.setQuantifier;
      val selectList = SelectBeyond.this.selectList;
      val selectTypes = SelectBeyond.this.selectTypes;
      val fromClause = SelectBeyond.this.fromClause;
      val whereClause = SelectBeyond.this.whereClause;
      val groupByClause = SelectBeyond.this.groupByClause;
      val havingClause = Some(se.toStatement);
    }
  }

  implicit def view10 (sb:SelectBeyond): statement.Select = new statement.Select {
    val setQuantifier = sb.setQuantifier;
    val selectList = sb.selectList;
    val fromClause = sb.fromClause;
    val whereClause = sb.whereClause;
    val groupByClause = sb.groupByClause;
    val havingClause = sb.havingClause;
    val fieldTypes = sb.selectTypes;
  }

  abstract class SelectGroupBy {
    def groupByClause: List[statement.Expression];
    def then (se:StatementExpression): SelectGroupBy = new SelectGroupBy {
      val groupByClause =
	SelectGroupBy.this.groupByClause ::: List(se.toStatement);
    }
    def then (se:String): SelectGroupBy = new SelectGroupBy {
      val groupByClause =
	SelectGroupBy.this.groupByClause ::: List(new statement.expression.Field {
	  val tableName = None;
	  val fieldName = se;
	});
    }
  }

  implicit def view11 (se:StatementExpression): SelectGroupBy = new SelectGroupBy {
    val groupByClause = List(se.toStatement);
  }

  implicit def view12 (se:String): SelectGroupBy = new SelectGroupBy {
    val groupByClause = List(new statement.expression.Field {
      val tableName = None;
      val fieldName = se;
    });
  }

}

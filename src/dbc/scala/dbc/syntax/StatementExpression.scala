/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.dbc
package syntax;


import java.math.BigDecimal;
import java.math.BigInteger;

abstract class StatementExpression {

  def toStatement: statement.Expression;

  def and (se:StatementExpression): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.BinaryOperator {
      val operator = "AND";
      val leftOperand = StatementExpression.this.toStatement;
      val rightOperand = se.toStatement;
    }
  }
  def or (se:StatementExpression): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.BinaryOperator {
      val operator = "OR";
      val leftOperand = StatementExpression.this.toStatement;
      val rightOperand = se.toStatement;
    }
  }
  def == (se:StatementExpression): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.BinaryOperator {
      val operator = "=";
      val leftOperand = StatementExpression.this.toStatement;
      val rightOperand = se.toStatement;
    }
  }
  def < (se:StatementExpression): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.BinaryOperator {
      val operator = "<";
      val leftOperand = StatementExpression.this.toStatement;
      val rightOperand = se.toStatement;
    }
  }
  def > (se:StatementExpression): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.BinaryOperator {
      val operator = ">";
      val leftOperand = StatementExpression.this.toStatement;
      val rightOperand = se.toStatement;
    }
  }
  def <= (se:StatementExpression): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.BinaryOperator {
      val operator = "<=";
      val leftOperand = StatementExpression.this.toStatement;
      val rightOperand = se.toStatement;
    }
  }
  def >= (se:StatementExpression): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.BinaryOperator {
      val operator = ">=";
      val leftOperand = StatementExpression.this.toStatement;
      val rightOperand = se.toStatement;
    }
  }
  def <> (se:StatementExpression): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.BinaryOperator {
      val operator = "<>";
      val leftOperand = StatementExpression.this.toStatement;
      val rightOperand = se.toStatement;
    }
  }
  def isNull: StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.UnaryOperator {
      val operator = "IS NULL";
      val operatorIsLeft = false;
      val operand = StatementExpression.this.toStatement;
    }
  }
  def isNotNull: StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.UnaryOperator {
      val operator = "IS NOT NULL";
      val operatorIsLeft = false;
      val operand = StatementExpression.this.toStatement;
    }
  }
  def + (se:StatementExpression): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.BinaryOperator {
      val operator = "+";
      val leftOperand = StatementExpression.this.toStatement;
      val rightOperand = se.toStatement;
    }
  }
  def - (se:StatementExpression): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.BinaryOperator {
      val operator = "-";
      val leftOperand = StatementExpression.this.toStatement;
      val rightOperand = se.toStatement;
    }
  }
  def * (se:StatementExpression): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.BinaryOperator {
      val operator = "*";
      val leftOperand = StatementExpression.this.toStatement;
      val rightOperand = se.toStatement;
    }
  }
  def / (se:StatementExpression): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.BinaryOperator {
      val operator = "/";
      val leftOperand = StatementExpression.this.toStatement;
      val rightOperand = se.toStatement;
    }
  }
  def % (se:StatementExpression): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.BinaryOperator {
      val operator = "%";
      val leftOperand = StatementExpression.this.toStatement;
      val rightOperand = se.toStatement;
    }
  }
  def ^ (se:StatementExpression): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.BinaryOperator {
      val operator = "^";
      val leftOperand = StatementExpression.this.toStatement;
      val rightOperand = se.toStatement;
    }
  }
  def not : StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.UnaryOperator {
      val operator = "!";
      val operatorIsLeft = false;
      val operand = StatementExpression.this.toStatement;
    }
  }
  def || (se:StatementExpression): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.BinaryOperator {
      val operator = "||";
      val leftOperand = StatementExpression.this.toStatement;
      val rightOperand = se.toStatement;
    }
  }
  def like (se:StatementExpression): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.BinaryOperator {
      val operator = "LIKE";
      val leftOperand = StatementExpression.this.toStatement;
      val rightOperand = se.toStatement;
    }
  }
  def similar (se:StatementExpression): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.BinaryOperator {
      val operator = "SIMILAR";
      val leftOperand = StatementExpression.this.toStatement;
      val rightOperand = se.toStatement;
    }
  }
  def in  (se:statement.Select): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.BinaryOperator {
      val operator = "IN";
      val leftOperand = StatementExpression.this.toStatement;
      val rightOperand = new statement.expression.Select {
        val selectStatement = se;
      };
    }
  }

}

object StatementExpression {

  def not (se:StatementExpression): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.UnaryOperator {
      val operator = "NOT";
      val operatorIsLeft = true;
      val operand = se.toStatement;
    }
  }
  def abs (se:StatementExpression): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.UnaryOperator {
      val operator = "@";
      val operatorIsLeft = true;
      val operand = se.toStatement;
    }
  }
  def exists (se:statement.Select): StatementExpression = new StatementExpression {
    val toStatement = new statement.expression.UnaryOperator {
      val operator = "EXISTS";
      val operatorIsLeft = true;
      val operand = new statement.expression.Select {
        val selectStatement = se;
      };
    }
  }

  abstract class StatementField extends StatementExpression {
    def fieldName: String;
    def tableName: Option[String] = None;
    def in (tn:String): StatementField = new StatementField {
      val fieldName = StatementField.this.fieldName;
      override val tableName = Some(tn);
    }
    def toStatement: statement.expression.Field = new statement.expression.Field {
      override val schemaName = None;
      val tableName = StatementField.this.tableName;
      val fieldName = StatementField.this.fieldName;
    }
  }

  implicit def stringToStatementField (ef:String): StatementField = new StatementField {
    val fieldName = ef;
  }




}

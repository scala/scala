//############################################################################
// Serialization
//############################################################################
// $Id$

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.lang.System;

//############################################################################
// Test classes in package "scala"

object Test1 {
  private def arrayToString[A](arr: Array[A]): String = {
    List.fromArray(arr).mkString("Array[",",","]");
  }
  private def arrayEquals[A,B](a1: Array[A], a2: Array[B]) =
     (a1.length == a2.length) &&
     (Iterator.range(0, a1.length) forall { i => a1(i) == a2(i) });
  val x1 = Nil;
  val x2 = None;
  val x3 = Array(1, 2, 3);
  val x4 = x: Int => 2 * x;

  try {
    val dataFile = java.io.File.createTempFile("test1", ".ser");
    val out = new ObjectOutputStream(new FileOutputStream(dataFile));
    out.writeObject(x1);
    out.writeObject(x2);
    out.writeObject(x3);
    out.writeObject(x4);
    out.close();

    val in = new ObjectInputStream(new FileInputStream(dataFile));
    val y1 = in.readObject().asInstanceOf[Nil.type];
    val y2 = in.readObject().asInstanceOf[Option[All]];
    val y3 = in.readObject().asInstanceOf[Array[Int]];
    val y4 = in.readObject().asInstanceOf[Int => Int];
    in.close();

    System.out.println("x1 = " + x1);
    System.out.println("y1 = " + y1);
    System.out.println("x1 eq y1: " + (x1 eq y1) + " - y1 eq x1: " + (y1 eq x1));
    System.out.println();
    System.out.println("x2 = " + x2);
    System.out.println("y2 = " + y2);
    System.out.println("x2 eq y2: " + (x2 eq y2) + " - y2 eq x2: " + (y2 eq x2));
    System.out.println();
    System.out.println("x3 = " + arrayToString(x3));
    System.out.println("y3 = " + arrayToString(y3));
    System.out.println("arrayEquals(x3, y3): " + arrayEquals(x3, y3));
    System.out.println();
    System.out.println("x4 = <na>");
    System.out.println("y4 = <na>");
    System.out.println("x4(2): " + x4(2) + " - y4(2): " + y4(2));
    System.out.println();
    dataFile.deleteOnExit()
  }
  catch {
    case e: Exception =>
      e.printStackTrace();
      System.out.println("Error in Test1: " + e);
  }
}

//############################################################################
// Test classes in package "scala.collection.immutable"

object Test2 {
  import scala.collection.immutable.BitSet;
  import scala.collection.immutable.ListMap;

  val x1 = List(
    Pair("buffers", 20),
    Pair("layers", 2),
    Pair("title", 3)
  );

  val x2 = new ListMap[String, Int]
    .incl(Pair("buffers", 20))
    .incl(Pair("layers", 2))
    .incl(Pair("title", 3));

  val x3 = new BitSet(4, Array(2), true);

  try {
    val dataFile = java.io.File.createTempFile("test2", ".ser");
    val out = new ObjectOutputStream(new FileOutputStream(dataFile));
    out.writeObject(x1);
    out.writeObject(x2);
    out.writeObject(x3);
    out.close();

    val in = new ObjectInputStream(new FileInputStream(dataFile));
    val y1 = in.readObject().asInstanceOf[List[Pair[String,Int]]];
    val y2 = in.readObject().asInstanceOf[ListMap[String, Int]];
    val y3 = in.readObject().asInstanceOf[BitSet];
    in.close();

    System.out.println("x1 = " + x1);
    System.out.println("y1 = " + y1);
    System.out.println("x1 equals y1: " + (x1 equals y1) + " - y1 equals x1: " + (y1 equals x1));
    System.out.println();
    System.out.println("x2 = " + x2);
    System.out.println("y2 = " + y2);
    System.out.println("x2 equals y2: " + (x2 equals y2) + " - y2 equals x2: " + (y2 equals x2));
    System.out.println();
    System.out.println("x3 = " + x3);
    System.out.println("y3 = " + y3);
    System.out.println("x3 equals y3: " + (x3 equals y3) + " - y3 equals x3: " + (y3 equals x3));
    System.out.println();
    dataFile.deleteOnExit()
  }
  catch {
    case e: Exception =>
      System.out.println("Error in Test2: " + e);
  }
}

//############################################################################
// Test classes in package "scala.collection.mutable"

object Test3 {
  import scala.collection.mutable.BitSet;
  import scala.collection.mutable.HashMap;

  val x1 = new HashMap[String, Int];
  x1 ++= Test2.x1;

  val x2 = new BitSet();
  x2.set(0);
  x2.set(8);
  x2.set(9);

  try {
    val dataFile = java.io.File.createTempFile("test3", ".ser");
    val out = new ObjectOutputStream(new FileOutputStream(dataFile));
    out.writeObject(x1);
    out.writeObject(x2);
    out.close();

    val in = new ObjectInputStream(new FileInputStream(dataFile));
    val y1 = in.readObject().asInstanceOf[HashMap[String, Int]];
    val y2 = in.readObject().asInstanceOf[BitSet];
    in.close();

    System.out.println("x1 = " + x1);
    System.out.println("y1 = " + y1);
    System.out.println("x1 equals y1: " + (x1 equals y1) + " - y1 equals x1: " + (y1 equals x1));
    System.out.println();
    System.out.println("x2 = " + x2);
    System.out.println("y2 = " + y2);
    System.out.println("x2 equals y2: " + (x2 equals y2) + " - y2 equals x2: " + (y2 equals x2));
    System.out.println();
    dataFile.deleteOnExit()
  }
  catch {
    case e: Exception =>
      System.out.println("Error in Test3: " + e);
  }
}

//############################################################################
// Test user-defined classes WITHOUT nesting

class Person(_name: String) with java.io.Serializable {
  private var name = _name;
  override def toString() = name;
  override def equals(that: Any): Boolean =
    that.isInstanceOf[Person] &&
    (name == that.asInstanceOf[Person].name);
}

class Employee(_name: String) with java.io.Serializable {
  private var name = _name;
  override def toString() = name;
}
object bob extends Employee("Bob");

object Test4 {
  val x1 = new Person("Tim");
  val x2 = bob;

  try {
    val dataFile = java.io.File.createTempFile("test4", ".ser");
    val out = new ObjectOutputStream(new FileOutputStream(dataFile));
    out.writeObject(x1);
    out.writeObject(x2);
    out.close();

    val in = new ObjectInputStream(new FileInputStream(dataFile));
    val y1 = in.readObject().asInstanceOf[Person];
    val y2 = in.readObject().asInstanceOf[Employee];
    in.close();

    System.out.println("x1 = " + x1);
    System.out.println("y1 = " + y1);
    System.out.println("x1 equals y1: " + (x1 equals y1) + " - y1 equals x1: " + (y1 equals x1));
    System.out.println();
    System.out.println("x2 = " + x2);
    System.out.println("y2 = " + y2);
    System.out.println("x2 equals y2: " + (x2 equals y2) + " - y2 equals x2: " + (y2 equals x2));
    System.out.println();
    dataFile.deleteOnExit()
  }
  catch {
    case e: Exception =>
      System.out.println("Error in Test4: " + e);
  }
}

//############################################################################
// Test user-defined classes WITH nesting

object Test5 with java.io.Serializable {
  object bill extends Employee("Bill") {
    val x = paul;
  }
  object paul extends Person("Paul") {
    val x = 4; //  bill; => StackOverflowException !!!
  }
  val x1 = new Person("John");
  val x2 = bill;
  val x3 = paul;

  try {
    val dataFile = java.io.File.createTempFile("test5", ".ser");
    val out = new ObjectOutputStream(new FileOutputStream(dataFile));
    out.writeObject(x1);
    out.writeObject(x2);
    out.writeObject(x3);
    out.close();

    val in = new ObjectInputStream(new FileInputStream(dataFile));
    val y1 = in.readObject().asInstanceOf[Person];
    val y2 = in.readObject().asInstanceOf[Employee];
    val y3 = in.readObject().asInstanceOf[Person];
    in.close();

    System.out.println("x1 = " + x1);
    System.out.println("y1 = " + y1);
    System.out.println("x1 equals y1: " + (x1 equals y1) + " - y1 equals x1: " + (y1 equals x1));
    System.out.println();
    System.out.println("x2 = " + x2);
    System.out.println("y2 = " + y2);
    System.out.println("x2 equals y2: " + (x2 equals y2) + " - y2 equals x2: " + (y2 equals x2));
    System.out.println();
    System.out.println("x3 = " + x3);
    System.out.println("y3 = " + y3);
    System.out.println("x3 equals y3: " + (x3 equals y3) + " - y3 equals x3: " + (y3 equals x3));
    System.out.println();
    dataFile.deleteOnExit()
  }
  catch {
    case e: Exception =>
      System.out.println("Error in Test5: " + e);
  }
}

//############################################################################
// Test code

object Test {
  def main(args: Array[String]): Unit = {
    Test1;
    Test2;
    Test3;
    Test4;
    Test5
  }
}

//############################################################################

//############################################################################
// Serialization
//############################################################################
// $Id$

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.lang.System;

object EqualityTest {
  def check[A,B](x: A, y: B): Unit = {
    System.out.println("x = " + x);
    System.out.println("y = " + y);
    System.out.println("x equals y: " + (x equals y) + " - y equals x: " + (y equals x));
    System.out.println();
  }
}

//############################################################################
// Test classes in package "scala"

object Test1_scala {
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
      System.out.println("Error in Test1_scala: " + e);
  }
}

//############################################################################
// Test classes in package "scala.collection.immutable"

object Test2_immutable {
  import scala.collection.immutable.{BitSet,ListMap,ListSet,Queue,Stack,
    TreeSet,TreeMap};

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

  val x4 = new ListSet[Int]().incl(3).incl(5);

  val x5 = new Queue("a", "b", "c");

  val x6 = new Stack().push("a", "b", "c");
/* !!! InvalidClassException !!!
  val x7 = new TreeMap[Int, String] + 42 -> "FortyTwo";

  val x8 = new TreeSet[Int]().incl(2).incl(0);
*/
  try {
    val dataFile = java.io.File.createTempFile("test2", ".ser");
    val out = new ObjectOutputStream(new FileOutputStream(dataFile));
    out.writeObject(x1);
    out.writeObject(x2);
    out.writeObject(x3);
    out.writeObject(x4);
    out.writeObject(x5);
    out.writeObject(x6);
/*
    out.writeObject(x7);
    out.writeObject(x8);
*/
    out.close();

    val in = new ObjectInputStream(new FileInputStream(dataFile));
    val y1 = in.readObject().asInstanceOf[List[Pair[String,Int]]];
    val y2 = in.readObject().asInstanceOf[ListMap[String, Int]];
    val y3 = in.readObject().asInstanceOf[BitSet];
    val y4 = in.readObject().asInstanceOf[ListSet[Int]];
    val y5 = in.readObject().asInstanceOf[Queue[String]];
    val y6 = in.readObject().asInstanceOf[Stack[String]];
/*
    val y7 = in.readObject().asInstanceOf[TreeMap[Int, String]];
    val y8 = in.readObject().asInstanceOf[TreeSet[Int]];
*/
    in.close();

    EqualityTest.check(x1, y1);
    EqualityTest.check(x2, y2);
    EqualityTest.check(x3, y3);
    EqualityTest.check(x4, y4);
    EqualityTest.check(x5, y5);
    EqualityTest.check(x6, y6);
/*
    EqualityTest.check(x7, y7);
    EqualityTest.check(x8, y8);
*/
    dataFile.deleteOnExit()
  }
  catch {
    case e: Exception =>
      System.out.println("Error in Test2_immutable: " + e);
  }
}

//############################################################################
// Test classes in package "scala.collection.mutable"

object Test3_mutable {
  import scala.collection.mutable.{BitSet,HashMap,HashSet,LinkedList,
    Queue,Stack};

  val x1 = new HashMap[String, Int];
  x1 ++= Test2_immutable.x1;

  val x2 = new BitSet();
  x2.set(0);
  x2.set(8);
  x2.set(9);

  val x3 = new HashSet[String];
  x3 ++= Test2_immutable.x1.map(p => p._1);

  val x4 = new LinkedList[Int](2, null);
  x4.append(new LinkedList(3, null));

  val x5 = new Queue[Int];
  x5 ++= Test2_immutable.x1.map(p => p._2);

  val x6 = new Stack[Int];
  x6 ++= x5;

  try {
    val dataFile = java.io.File.createTempFile("test3", ".ser");
    val out = new ObjectOutputStream(new FileOutputStream(dataFile));
    out.writeObject(x1);
    out.writeObject(x2);
    out.writeObject(x3);
    out.writeObject(x4);
    out.writeObject(x5);
    out.writeObject(x6);
    out.close();

    val in = new ObjectInputStream(new FileInputStream(dataFile));
    val y1 = in.readObject().asInstanceOf[HashMap[String, Int]];
    val y2 = in.readObject().asInstanceOf[BitSet];
    val y3 = in.readObject().asInstanceOf[HashSet[String]];
    val y4 = in.readObject().asInstanceOf[LinkedList[Int]];
    val y5 = in.readObject().asInstanceOf[Queue[Int]];
    val y6 = in.readObject().asInstanceOf[Stack[Int]];
    in.close();

    EqualityTest.check(x1, y1);
    EqualityTest.check(x2, y2);
    EqualityTest.check(x3, y3);
    EqualityTest.check(x4, y4);
    EqualityTest.check(x5, y5);
    EqualityTest.check(x6, y6);
    dataFile.deleteOnExit()
  }
  catch {
    case e: Exception =>
      System.out.println("Error in Test3_mutable: " + e);
  }
}

//############################################################################
// Test classes in package "scala.xml"

object Test4_xml {
  import scala.xml.{Elem};

  val x1 = <html><title>title</title><body></body></html>;

  try {
    val dataFile = java.io.File.createTempFile("test4", ".ser");
    val out = new ObjectOutputStream(new FileOutputStream(dataFile));
    out.writeObject(x1);
    out.close();

    val in = new ObjectInputStream(new FileInputStream(dataFile));
    val y1 = in.readObject().asInstanceOf[Elem];
    in.close();

    EqualityTest.check(x1, y1);
    dataFile.deleteOnExit()
  }
  catch {
    case e: Exception =>
      System.out.println("Error in Test4_xml: " + e);
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

object Test5 {
  val x1 = new Person("Tim");
  val x2 = bob;

  try {
    val dataFile = java.io.File.createTempFile("test5", ".ser");
    val out = new ObjectOutputStream(new FileOutputStream(dataFile));
    out.writeObject(x1);
    out.writeObject(x2);
    out.close();

    val in = new ObjectInputStream(new FileInputStream(dataFile));
    val y1 = in.readObject().asInstanceOf[Person];
    val y2 = in.readObject().asInstanceOf[Employee];
    in.close();

    EqualityTest.check(x1, y1);
    EqualityTest.check(x2, y2);
    dataFile.deleteOnExit()
  }
  catch {
    case e: Exception =>
      System.out.println("Error in Test5: " + e);
  }
}

//############################################################################
// Test user-defined classes WITH nesting

object Test6 with java.io.Serializable {
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
    val dataFile = java.io.File.createTempFile("test6", ".ser");
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

    EqualityTest.check(x1, y1);
    EqualityTest.check(x2, y2);
    EqualityTest.check(x3, y3);
    dataFile.deleteOnExit()
  }
  catch {
    case e: Exception =>
      System.out.println("Error in Test6: " + e);
  }
}

//############################################################################
// Test code

object Test {
  def main(args: Array[String]): Unit = {
    Test1_scala;
    Test2_immutable;
    Test3_mutable;
    Test4_xml;
    Test5;
    Test6
  }
}

//############################################################################

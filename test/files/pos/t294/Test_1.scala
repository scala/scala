// also test pickling of java annotations; Test_2.scala will
// read this class file
@Ann(nested = Array(new Ann2(10))) class Test {
  @Ann2(100) var ctx: Object = _
  @Ann(nested = Array()) def foo = 10
  @Ann(nested = Array(new Ann2(10), new Ann2(23))) val bam = -3
}

//Generate the classes and assertions under test.
case class TestCase(classType: String, p1: ParamConfig, p2: ParamConfig) {
  val className = s"${classType.headOption.getOrElse('r')}${abbr(p1)}_${abbr(p2)}".capitalize
  val tParams = "[@specialized(Int) A, @specialized(Int) B]"
  def abbr(p: ParamConfig): String = p.modifier.split(' ').toSeq.map(_.headOption.getOrElse('n')).mkString
  def decl(param: ParamConfig): String = s"val ${param.aliasName} = ${param.constructorName}"
  def renderClass: String = s"""$classType class $className$tParams(${p1.modifier} ${p1.constructorName}: A, ${p2.modifier} a: B){
                               |  ${decl(p1)}
                               |  ${decl(p2)}
                               |}""".stripMargin
  
  def accessConstr(p: ParamConfig) = Option(p).filterNot(p => p.modifier == "private val").filterNot(p => p.modifier == "" && classType == "").map(_.constructorName)
  def testConstrCh(p: ParamConfig, expected: String) = accessConstr(p).map(name => s"assert($expected == ch_$className.$name)")
  def testConstrI(p: ParamConfig, expected: String) = accessConstr(p).map(name => s"assert($expected == i_$className.$name)")
  def testAliasCh(p: ParamConfig, expected: String) = Some(p.aliasName).map(name => s"assert($expected == ch_$className.$name)")
  def testAliasI(p: ParamConfig, expected: String) = Some(p.aliasName).map(name => s"assert($expected == i_$className.$name)")
  def testExtractors = Some(s"""val $className(extracted1i_$className, extracted2i_$className) = i_$className
  |val $className(extracted1ch_$className, extracted2ch_$className) = ch_$className
  |assert(1 == extracted1i_$className)
  |assert(2 == extracted2i_$className)
  |assert('a' == extracted1ch_$className)
  |assert('b' == extracted2ch_$className)
  |""".stripMargin).filter(_ => classType == "case")
  val assertions = List(
    testExtractors,
    testConstrI(p1, "1"),
    testConstrI(p2, "2"),
    testConstrCh(p1, "'a'"),
    testConstrCh(p2, "'b'"),
    testAliasI(p1, "1"),
    testAliasI(p2, "2"),
    testAliasCh(p1, "'a'"),
    testAliasCh(p2, "'b'"),
  ).collect{ case Some(t) => t }
  def renderTests: String = (instantiateChar :: instantiateInt :: assertions).mkString("\n", "\n", "\n")
  def instantiateChar = s"val ch_$className = new $className('a', 'b')" //non-specialized variety
  def instantiateInt = s"val i_$className = new $className(1, 2)" //specialized variety
}

case class ParamConfig(modifier: String, constructorName: String, aliasName: String)

object Generator {
  def paramConfigurations(constructorName: String, aliasName: String) = for {
    modifier <- List("private val", "val", "")
  } yield ParamConfig(modifier, constructorName, aliasName)

  def hasVal(p1: ParamConfig, p2: ParamConfig) = p1.modifier.contains("val") || p2.modifier.contains("val")

  val configurations = for {
    classConfig <- List("case", "")
    p1config <- paramConfigurations("`a b`", "p1")
    p2config <- paramConfigurations("a", "p2")
  } yield TestCase(classConfig, p1config, p2config)

  def main(args: Array[String]): Unit = {
    import java.io.File
    import java.io.PrintWriter

    val classes = new File("Classes_1.scala")
    val tests = new File("Tests_2.scala")
    val classWriter = new PrintWriter(classes)
    val testWriter = new PrintWriter(tests)
    
    for(testClass <- configurations) {
      classWriter.write(testClass.renderClass)
      classWriter.write("\n")
    }

    //test both separate and joint compilation. 

    testWriter.write("object Test extends App {\n")
    classWriter.write("object TestJoint  {\n def joint(): Unit = {\n")
    for(testClass <- configurations){
      classWriter.write(testClass.renderTests)
      classWriter.write("\n")
      testWriter.write(testClass.renderTests)
      testWriter.write("\n")
    }
    classWriter.write("\n}}\n")
    testWriter.write("TestJoint.joint()")

    testWriter.write("\n}\n")
    classWriter.close()
    testWriter.close()
    
  }
}
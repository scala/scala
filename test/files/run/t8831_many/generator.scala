//Generate the classes and assertions under test.
case class TestCase(classType: String, specialized: Boolean, p1: ParamConfig, p2: ParamConfig) {
  val className = s"${classType.headOption.getOrElse('r')}${specialized.toString().head}${abbr(p1)}${abbr(p2)}".capitalize
  val tparams = if (specialized) "[@specialized(Int) A, @specialized(Int) B]" else "[A, B]"
  def abbr(p: ParamConfig): String = p.modifier.split(' ').toSeq.map(_.headOption.getOrElse('n')).mkString + p.accessed.toString().head
  //def access(param: ParamConfig, name: String) = if(param.accessed)
  def decl(param: ParamConfig): String = param.aliasName.map(n => s"val $n = ${param.constructorName}\n").getOrElse("")
  def renderClass: String = s"""$classType class $className$tparams(${p1.modifier} ${p1.constructorName}: A, ${p2.modifier} a: B){
    ${decl(p1)}${decl(p2)}
  }"""
  
  def accessConstr(p: ParamConfig) = Option(p).filterNot(p => p.modifier == "private val").filterNot(p => p.modifier == "" && classType == "").map(_.constructorName)
  def testConstrCh(p: ParamConfig, expected: String) = accessConstr(p).map(name => s"assert($expected == ch_$className.$name)")
  def testConstrI(p: ParamConfig, expected: String) = accessConstr(p).map(name => s"assert($expected == i_$className.$name)")
  def testAliasCh(p: ParamConfig, expected: String) = p.aliasName.map(name => s"assert($expected == ch_$className.$name)")
  def testAliasI(p: ParamConfig, expected: String) = p.aliasName.map(name => s"assert($expected == i_$className.$name)")
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
  def instantiateChar = s"val ch_$className = new $className('a', 'b')"
  def instantiateInt = s"val i_$className = new $className(1, 2)"
  
}

case class ParamConfig(modifier: String, constructorName: String, aliasName: Option[String]) {
  def accessed = aliasName.isDefined
}

object Generator {
  def paramConfigurations(constructorName: String, aliasName: String) = for {
    modifier <- List("private val", "val", "")
    accessed <- List(true, false)
  } yield ParamConfig(modifier, constructorName, Option(aliasName).filter(_ => accessed))

  def hasVal(p1: ParamConfig, p2: ParamConfig) = p1.modifier.contains("val") || p2.modifier.contains("val")

  val configurations = for {
    classConfig <- List("case", "")
    specialized <- List(true, false)
    p1config <- paramConfigurations("`a b`", "p1")
    p2config <- paramConfigurations("a", "p2")
    if (!(specialized && !(p1config.accessed || p2config.accessed)))
  } yield TestCase(classConfig, specialized, p1config, p2config)

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
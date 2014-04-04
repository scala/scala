package subscript

//import org.scalatest.FunSuite

import org.junit._
import org.junit.runner.RunWith

import subscript.DSL._
import subscript.vm.{TemplateChildNode, N_code_unsure, CallGraphNodeTrait, UnsureExecutionResult, 
                     ScriptExecutor, CommonScriptExecutor, SimpleScriptDebugger}

/**
 * This class is a test suite for the script operators as implemented in the SubScript VM. 
 * 
 * To run the test suite, you can 
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 *  
 * *************************** 
 * High level methods
 * *************************** 
 * 
 * - testBehaviours: 
 *     tests many kinds of behaviors, of script expressions such as
 *     
 *      (-)
 *      a
 *      a;b
 *      a+b
 *      
 *    a, b, c are called "atoms"
 *    
 * - testLogicalOr, testLogicalAnd: 
 *     check logic behavior of operators.
 *     
 *   And-like operators (; & &&) have neutral operand (+)
 *    Or-like operators (+ | || /) have neutral operand (-)
 *   And there is the generic neutral operand (+-).
 *   So it is tested whether script equivalences such as the following hold:
 *     x#n = x
 *     n#x = x
 *     for each operator #, for each neutral operand (+-) and, depending on #, (+) or (-)
 *                       and for x equal to (+) (-) (+-) a
 *  
 * *************************** 
 * Test case specifications
 * *************************** 
 * 
 *   A script test case specification is a tuple of
 *   - a script expression, depicted as a string, e.g., "a+b"
 *   - a "behaviours" string, i.e. a space-separated-list of behaviour cases, e.g. "->a a->b ab"
 *   
 *   A behaviour case either matches one of the following:
 *   - "input"        => the scripts accepts the tokens of the given input and then ends successfully
 *   - "input->result => the script accepts the tokens in input, with the given result
 *      Here "input" is a sequence of tokens "a","b","c", e.g. "aaba"; 
 *      each token is to be eaten by the corresponding atom a,b,c.
 *      "result" is either "0" for deadlock, or a subsequence of "1abc", e.g. "1a" or "b"
 *         "1" means the script may terminate successfully, "a" means an "a" is expected, etc.
 *        
 *   Example test specification: 
 *   
 *      "a|b" -> "->ab a->1b b->1a ab ba"  
 *       
 *   BTW: a behaviour "a" is equivalent to "a->1" etc.
 *
 * *************************** 
 * Low level implementation 
 * *************************** 
 * 
 * testBehaviours, testLogicalOr, testLogicalAnd call testScriptBehaviours 
 * 
 * testScriptBehaviours creates a script structure for the vm, and interprets the specified behaviours 
 * - a referred behaviour "=expr" results in a recursive call to testScriptBehaviours
 * - other behaviours strings are split into single behaviours, and then fed into the method testScriptBehaviour, 
 *   together with the script structure
 * 
 * testScriptBehaviour executes the handed script using an executor and checks the execution with the given specification.
 * The execution should end in success (as witnessed by executor.hasSuccess) if the specification says so,
 * This is a bit tricky.
 * Note that the specified input string is to be eaten by the atoms. 
 * An atom is programmed as the following script expression:
 *   @expect(there,name): {?tryAccept(here, name)?}
 * The "expect(there,name)" annotation puts the atom name in the list "expectedAtoms"; it also makes sure
 * that the atom name is removed from "expectedAtoms" when the atom script expression deactivates (using method "unexpect")
 * 
 * Note that the script execution has 2 kinds of phases that are relevant here:
 * 1 - activation and deactivation, when the expectations are added and removed
 * 2 - try out of the unsure code fragments, with method "tryAccept"
 * 
 * The variable "expectedAtomsAtEndOfInput" should contain the expectedAtoms 
 * as valid just after input processing has stopped (either because the input was at its end, or because of unexpected input)
 * At an activation belonging to a phase 1, i.e. in the "expect" method, this expectedAtomsAtEndOfInput is set to null
 * At the start of a phase 2, i.e. at the first call to "tryAccept", identified by expectedAtomsAtEndOfInput==null,
 * expectedAtomsAtEndOfInput is set to expectedAtoms at that moment.
 * In case this is the last phase2, this version of expectedAtomsAtEndOfInput will be used in testScriptBehaviour
 * to compare with the specified result.
 * 
 * Likewise, the variable "scriptSuccessAtEndOfInput" should contain the contain the success state of the script
 * as valid just after input processing. Both this variable and expectedAtomsAtEndOfInput are only set in case there 
 * is any atom expected; for other scripts such as "(-)" and "(+)" the end value of executor.hasSuccess will do. 
 * 
 * With a given (input,result) specification, the script execution can come to an end in the following ways:
 * 1 - all input has been accepted
 *     the specified result should match expectedAtomsAtEndOfInput and match executor.hasSuccess 
 * 2 - an input token had not been expected; then the test fails
 * In both cases, all calls to tryAccept give the UnsureCodeFragment result status "Failure" so that the 
 * script call graph deactivates totally, so that the script execution ends
 * 
 * In other cases there must be a call to tryAccept for an atom that matches the current input token. 
 * Then the UnsureCodeFragment result status becomes "Success"; the input stream goes to the next token, and the 
 * accepted token is logged in acceptedTokens. The latter variable is used later to see whether the entire specified input
 * has been consumed.
 * 
 * All other calls to tryAccept result in the UnsureCodeFragment result status becoming "Ignore"; these code fragments
 * stay in the queue for the time being so that they may be retried; it is also possible that they are excluded because 
 * a "relatively exclusive" UnsureCodeFragment succeeds (e.g., as with a in "a+b" when b happens).
 * 
 * ********************
 * Notes
 * ********************
 * expectedAtoms etc are lists rather than sets; in principle multiple instances of atom may be expected synchronously.
 * A MultiSet would therefore do better than a Set, but since it is not in the standard Scala library we use a List here.
 * 
 */
//@RunWith(classOf[JUnitRunner])
@Test
class OperatorsSuite {

  /*
   * Behaviour operators characterized by their logic property
   * Logical-Or  means that (-) ("zero") is the neutral element
   * Logical-And means that (+) ("one")  is the neutral element
   */
  val logicalAnd_string = "; & &&"
  val logicalOr_string  = "+ | || /"
    
  val logicalAndOperators = logicalAnd_string.split(" ")
  val logicalOrOperators  = logicalOr_string .split(" ") 
  val behaviorOperators   = logicalAndOperators.toList:::logicalOrOperators.toList
  
  /*
   * Low level stuff
   */
  def testScriptBehaviours(scriptDef: Script, scriptString: String, behaviours: String) {
    
    import scala.util.matching.Regex
    val pattern = new Regex(" +") // replace all multispaces by a single space, just before splitting behaviours:
    for (behaviour<-(pattern replaceAllIn(behaviours," ")).split(" ")) {
      val inputAndResult = behaviour.split("->")
      val input          = inputAndResult(0)
      val expectedResult = if (inputAndResult.length>1) inputAndResult(1) else "1"
      testScriptBehaviour(scriptDef, scriptString, input, expectedResult)
    }
  }

  var acceptedAtoms : String       = null
  var inputStream   : Stream[Char] = null
  var expectedAtoms :   List[Char] = null
  
  var expectedAtomsAtEndOfInput: Option[List[Char]] = None
  var scriptSuccessAtEndOfInput: Option[Boolean]    = None
  var textIndex = 0
  var executor: ScriptExecutor = null

  def testScriptBehaviour(scriptDef: Script, scriptString: String, input: String, expectedResult: String) {
    //println("testScript("+scriptString+", "+input+" -> "+expectedResult+")")
    
    textIndex += 1
    //test(textIndex+". script "+scriptString+"     :     "+input+" -> "+expectedResult) {
    
	  acceptedAtoms         = ""
	  inputStream           = scala.io.Source.fromString(input).toStream
	  expectedAtoms         = Nil
	  expectedAtomsAtEndOfInput = None
	  scriptSuccessAtEndOfInput = None
	   
	  val expectedResultFailure = expectedResult(0)=='0'
	  val expectedResultSuccess = expectedResult(0)=='1'
	  val expectedResultAtoms   = (if (expectedResultSuccess||expectedResultFailure) expectedResult.drop(1) else expectedResult)
	                              .sortWith(_<_).mkString

      Assert.assertTrue("test specification error: no atoms expected after failure (0)", !expectedResultFailure || expectedResultAtoms.isEmpty)

	  executor = new CommonScriptExecutor

	  val debug = false
	  val debugger = if (debug) new SimpleScriptDebugger else null
	  
      _execute(scriptDef, debugger, executor)
      
      val executionSuccess = scriptSuccessAtEndOfInput.getOrElse(executor.hasSuccess)
      
      if (!expectedResultSuccess) {
        Assert.assertFalse("script execution should have no success; accepted="+acceptedAtoms, executionSuccess)
      }
      else { // note: only check for expectedAtoms here (in else branch); otherwise (-)&&a would raise false alarm
        Assert.assertTrue("script execution should have success; accepted="+acceptedAtoms, executionSuccess)
        val    expectedAtomsAtEndOfInputString = expectedAtomsAtEndOfInput.getOrElse(Nil).sortWith(_<_).mkString
        Assert.assertTrue("expectedAtomsAtEndOfInput=" + expectedAtomsAtEndOfInputString + " required=" + expectedResultAtoms,
                            expectedAtomsAtEndOfInputString==expectedResultAtoms) 
      }
      Assert.assertTrue("acceptedAtoms='" + acceptedAtoms + "' required='" + input+"'", acceptedAtoms==input) 
    //}   
  }

  // utility method: remove 1 occurrence of elt from list; see http://stackoverflow.com/a/5640727
  def remove1Element[T](list: List[T], elt: T): List[T] = list diff List(elt)
  
  // add expectation of the given atom; also prepares for the symmetric to unexpect during the inevitable deactivation
  def expect   (where: N_code_unsure, atom: Char) {where.onDeactivate(unexpect(where, atom)); expectedAtoms ::= atom
                                                                                              expectedAtomsAtEndOfInput=None}
  // remove expectation of the given atom
  def unexpect (where: N_code_unsure, atom: Char) {expectedAtoms = remove1Element(expectedAtoms, atom)}
  
  // try to accept the token from the input (if any); match it to the current atom.
  def tryAccept(where: N_code_unsure, atom: Char) {
    if (inputStream.isEmpty || !expectedAtoms.contains(inputStream.head)) {
       where.result = UnsureExecutionResult.Failure; 
       if (expectedAtomsAtEndOfInput== None) {
           expectedAtomsAtEndOfInput = Some(expectedAtoms)
           scriptSuccessAtEndOfInput = Some(executor.hasSuccess)
           if (false) println("inputStream.isEmpty=${inputStream.isEmpty} expectedAtoms=${expectedAtoms.mkString}"
                             +     (if (inputStream.isEmpty) "" else " inputStream.head=${inputStream.head}") 
                             +                                          " scriptSuccess=$scriptSuccessAtEndOfInput")
       }
    }
    else if (inputStream.head==atom) {inputStream = inputStream.drop(1); acceptedAtoms += atom}
    else                             {where.result = UnsureExecutionResult.Ignore}
    
    //println("<<<tryAccept: "+where+" inputStream "+ (if (inputStream.isEmpty) "Empty" else ("head = "+inputStream.head))+"  has success = "+where.hasSuccess)
  } 

  //  script expression structure for an atom. It essentially comes down to the following script:
  def script ..
    atom(name: Char) = @{expect(there,name)}: {?tryAccept(here, name)?}
    a = atom('a')
    b = atom('b')
    c = atom('c')
    d = atom('d')

  /*
   * scriptBehaviourMap: relation between scripts and outcomes
   *   keys are script strings, which should also be keys in the scriptBodyMap, so that bodies can be found
   *   the outcomes are a input traces in relation to their outcomes, see #testScriptBehaviour
   */
  val scriptBehaviourList1 = List( // list, not a map, to remain ordered
     [a&b&c]   -> "a&b&c"   -> "cb->a" // goes wrong since "Ignore" status causes AAToBeReexecuted which may be stored in LIFO order...TBD in ScriptExecutor
   , [(-)]     -> "(-)"     -> "->0"
   , [(+)]     -> "(+)"     -> ""
   , [(+-)]    -> "(+-)"    -> ""
   , [break]   -> "break"   -> "=(+-)"
   , [.]       -> "."       -> "=(+-)"
   , [..]      -> ".."      -> "=(+-)"
   , [...]     -> "..."     -> "=(+-)"
                               
   , [a]       -> "a"       -> "->a a"
  )           
  val scriptBehaviourList = List( // list, not a map, to remain ordered
              
   // simple terms
     [(-)]     -> "(-)"     -> "->0"
   , [(+)]     -> "(+)"     -> ""
   , [(+-)]    -> "(+-)"    -> ""
   , [break]   -> "break"   -> ""
   , [.]       -> "."       -> ""
   , [..]      -> ".."      -> ""
   , [...]     -> "..."     -> ""
                               
   , [a]       -> "a"       -> "->a a"
                               
   //  a op b 
   , [a;b]     -> "a;b"     -> "->a a->b ab"
   , [a+b]     -> "a+b"     -> "->ab a b"
   , [a&b]     -> "a&b"     -> "->ab a->b  b->a  ab ba"
   , [a&&b]    -> "a&&b"    -> "->ab a->b  b->a  ab ba"
   , [a|b]     -> "a|b"     -> "->ab a->1b b->1a ab ba"
   , [a||b]    -> "a||b"    -> "->ab a b"
   , [a/b]     -> "a/b"     -> "->ab a b"
                               
   // a op antineutral
   , [a;(-)]   -> "a;(-)"   -> "->a a->0"
   , [(-);a]   -> "(-);a"   -> "->0"
   , [a&(-)]   -> "a&(-)"   -> "->a a->0"
   , [(-)&a]   -> "(-)&a"   -> "->a a->0"
   , [a&&(-)]  -> "a&&(-)"  -> "->0"
   , [(-)&&a]  -> "(-)&&a"  -> "->0"
                             
   , [a+(+)]   -> "a+(+)"   -> "->1a a"
   , [(+)+a]   -> "(+)+a"   -> "->1a a"
   , [a|(+)]   -> "a|(+)"   -> "->1a a"
   , [(+)|a]   -> "(+)|a"   -> "->1a a"
   , [a||(+)]  -> "a||(+)"  -> ""
   , [(+)||a]  -> "(+)||a"  -> ""
   , [a/(+)]   -> "a/(+)"   -> "->1a a"
   , [(+)/a]   -> "(+)/a"   -> "->1a a"
                               
   // 2 operand sequences with iterator or break or optional break, 
   , [break;a] -> "break;a" -> ""
   , [.;a]     -> ".;a"     -> "->1a a"
   , [..;a]    -> "..;a"    -> "->1a a->1a aa->1a"
   , [...;a]   -> "...;a"   -> "->a  a->a  aa->a"
                               
   , [a;break] -> "a;break" -> "->a  a"
   , [a;.]     -> "a;."     -> "->a  a"
   , [a;..]    -> "a;.."    -> "->a  a->1a aa->1a"
   , [a;...]   -> "a;..."   -> "->a  a->a  aa->a"
   
   // 3 operand sequences with iterator or break or optional break, 
   , [a;b;break]   -> "a;b;break"   -> "->a a->b ab"
   , [a;b;.]       -> "a;b;."       -> "->a a->b ab"
   , [a;b;..]      -> "a;b;.."      -> "->a  a->b ab->1a aba->b abab->1a"
   , [a;b;...]     -> "a;b;..."     -> "->a  a->b ab->a  aba->b abab->a"
                                       
   , [a;break;b]   -> "a;break;b"   -> "->a  a"
   , [a;.;b]       -> "a;.;b"       -> "->a  a->1b ab"
   , [a;..;b]      -> "a;..;b"      -> "->a  a->1b ab->a aba->1b"
   , [a;...;b]     -> "a;...;b"     -> "->a  a->b  ab->a aba->b"
                                       
   , [break;a;b]   -> "break;a;b"   -> "->1"
   , [.;a;b]       -> ".;a;b"       -> "->1a a->b ab"
   , [..;a;b]      -> "..;a;b"      -> "->1a a->b ab->1a aba->b"
   , [...;a;b]     -> "...;a;b"     -> "->a  a->b  ab->a aba->b"
   
   // 2 level nested 2 operand sequences with iterator or break or optional break, 
   , [a;(b;break)] -> "a;(b;break)" -> "->a  a->b ab"
   , [a;(b;.)]     -> "a;(b;.)"     -> "->a  a->b ab"
   , [a;(b;..)]    -> "a;(b;..)"    -> "->a  a->b ab->1b abb->1b"
   , [a;(b;...)]   -> "a;(b;...)"   -> "->a  a->b  ab->b"
                                       
   , [(a;b);break] -> "(a;b);break" -> "->a a->b ab"
   , [(a;b);.]     -> "(a;b);."     -> "->a a->b ab"
   , [(a;b);..]    -> "(a;b);.."    -> "->a  a->b ab->1a aba->b abab->1a"
   , [(a;b);...]   -> "(a;b);..."   -> "->a  a->b ab->a  aba->b abab->a"
                                       
   , [a;(break;b)] -> "a;(break;b)" -> "->a  a"
   , [a;(.;b)]     -> "a;(.;b)"     -> "->a  a->1b ab"
   , [a;(..;b)]    -> "a;(..;b)"    -> "->a  a->1b ab->1b"
   , [a;(...;b)]   -> "a;(...;b)"   -> "->a  a->b  ab->b"
                                       
   , [(a;break);b] -> "(a;break);b" -> "->a  a->b ab"
   , [(a;.);b]     -> "(a;.);b"     -> "->a  a->b ab"
   , [(a;..);b]    -> "(a;..);b"    -> "->a  a->ab aa->ab ab aab"
   , [(a;...);b]   -> "(a;...);b"   -> "->a  a->a  aa->a"
                                       
   , [break;(a;b)] -> "break;(a;b)" -> "->1"
   , [.;(a;b)]     -> ".;(a;b)"     -> "->1a a->b ab"
   , [..;(a;b)]    -> "..;(a;b)"    -> "->1a a->b ab->1a aba->b"
   , [...;(a;b)]   -> "...;(a;b)"   -> "->a  a->b  ab->a aba->b"
                                       
   , [(break;a);b] -> "(break;a);b" -> "b"
   , [(.;a);b]     -> "(.;a);b"     -> "->ab  a->b ab b"
   , [(..;a);b]    -> "(..;a);b"    -> "->ab  a->ab aa->ab b ab aab"
   , [(...;a);b]   -> "(...;a);b"   -> "->a  a->a  aa->a"

   // parallel composition
   , [(...;a)&b]   -> "(...;a)&b"   -> "->ab  a->ab  aa->ab  b->a  ba->a  ab->a  aba->a"
   , [b&(...;a)]   -> "b&(...;a)"   -> "->ab  a->ab  aa->ab  b->a  ba->a  ab->a  aba->a"  // commutative
   , [(...;a)|b]   -> "(...;a)|b"   -> "->ab  a->ab  aa->ab  b->1a  ba->1a  ab->1a  aba->1a"
   , [b|(...;a)]   -> "b|(...;a)"   -> "->ab  a->ab  aa->ab  b->1a  ba->1a  ab->1a  aba->1a"  // commutative
                                       
   , [a&b&c]       -> "a&b&c"       -> "->abc a->bc  b->ac  c->ab  ab->c  ac->b  ba->c  bc->a  ca->b  cb->a  abc acb bac bca cab cba" 
   , [a&&b&&c]     -> "a&&b&&c"     -> "->abc a->bc  b->ac  c->ab  ab->c  ac->b  ba->c  bc->a  ca->b  cb->a  abc acb bac bca cab cba"  
   , [a|b|c]       -> "a|b|c"       -> "->abc a->1bc b->1ac c->1ab ab->1c ac->1b ba->1c bc->1a ca->1b cb->1a abc acb bac bca cab cba"  
   , [a||b||c]     -> "a||b||c"     -> "->abc a b c"  
                                       
   // disruption with compound left hand operand
   , [(a|b)/c]     -> "(a|b)/c"     -> "->abc a->1bc b->1ac c ac bc ab ba"
   , [(a;b)/c]     -> "(a;b)/c"     -> "->ac a->bc ab c ac" 
   , [(a;b)/(+)]   -> "(a;b)/(+)"   -> "->1a a->1b ab"
   
   // optional break
   , [ a / . / b ]             -> " a / . / b "             -> "a"
   , [ a b / . / c d ]         -> " a b / . / c d "         -> "->a a->bc  ab     ac->d  acd"
   , [ a b & . & c d ]         -> " a b & . & c d "         -> "->a a->bc  ab->1c ac->bd abc->d  abcd acb->d  acd->b  acbd acdb"
   , [ a b | . | c d ]         -> " a b | . | c d "         -> "->a a->bc  ab->1c ac->bd abc->1d abcd acb->1d acd->1b acbd acdb"
   , [ . / a b ]               -> " . / a b "               -> "->1a a->b  ab"
   , [ . & a b ]               -> " . & a b "               -> "->1a a->b  ab"
   , [ . | a b ]               -> " . | a b "               -> "->1a a->b  ab"
   , [ . / a b / . / c d ]     -> " . / a b / . / c d "     -> "->1a a->bc ab ac->d acd"
   , [ a b  | .  | (+) ]       -> " a b  | .  | (+) "       -> "->a a->1b ab"
   , [ a b || . || (+) ]       -> " a b || . || (+) "       -> "a"
   , [ a b  & .  & (-) ]       -> " a b  & .  & (-) "       -> "ab0"
   , [ a b && . && (-) ]       -> " a b && . && (-) "       -> "a0"
   , [ (a b+(+))  & .  & (-) ] -> " (a b+(+))  & .  & (-) " -> "->1a a->b ab->0"
   , [ (a b+(+)) && . && (-) ] -> " (a b+(+)) && . && (-) " -> "->1a a->0"

  )

  val scriptBehaviourMap = scriptBehaviourList.toMap
  
  /*
   * Test logic behaviour
   * For each behaviour operator #, a neutral operand (for #) n, another operand x, it should hold:
   *  x#n = x
   *  n#x = x
   *  
   *  (+-) should need only behave neutrally next to an "a": (+-)#a = a#(+-) = a
   */
  //def testLogicalOr  = testLogic(false, logicalOrOperators )
  //def testLogicalAnd = testLogic( true, logicalAndOperators)
  
  /*
  def testLogic(isLogicalAnd: Boolean, operatorStrings: Seq[String]) = {
    val    neutralProcess = if ( isLogicalAnd) _empty else _deadlock
    val notNeutralProcess = if (!isLogicalAnd) _empty else _deadlock
    for (opStr<-operatorStrings) {
      //val op = _op(opStr) _
      testScriptBehaviours(neutralProcess.kindAsString+opStr+"a"         , "=a", _op(opStr)(_neutral,_a)) 
      testScriptBehaviours("a"         +opStr+neutralProcess.kindAsString, "=a", _op(opStr)(_a,_neutral))
      
      for ( (operandStr, operandTemplate)<-Map("a"->_a, _deadlock.kindAsString->_deadlock, _empty.kindAsString->_empty)) {
          testScriptBehaviours(neutralProcess.kindAsString+opStr+operandStr         , "="+operandStr, _op(opStr)(neutralProcess,operandTemplate))
          testScriptBehaviours(operandStr         +opStr+neutralProcess.kindAsString, "="+operandStr, _op(opStr)(operandTemplate,neutralProcess))
      }
    }
  }

  */
  
  def testBehaviours = {
    for ( (key, behaviours) <- scriptBehaviourList) {
      val (aScript, aString) = key.asInstanceOf[(Script,String)]
      testScriptBehaviours(aScript, aString, behaviours.asInstanceOf[String])
    }
  }
  
  /*
   * High level calls that will be tested:
   */
  testBehaviours
  //testLogicalOr
  //testLogicalAnd
}

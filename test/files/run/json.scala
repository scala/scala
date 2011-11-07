import scala.util.parsing.json._
import scala.collection.immutable.TreeMap

object Test extends App {
  /* This method converts parsed JSON back into real JSON notation with objects in
   * sorted-key order. Not required by the spec, but it allows us to to a stable
   * toString comparison. */
  def jsonToString(in : Any) : String = in match {
    case l : List[_] => "[" + l.map(jsonToString).mkString(", ") + "]"
    case m : Map[String,_] => "{" + m.elements.toList
         .sort({ (x,y) => x._1 < y._1 })
         .map({ case (k,v) => "\"" + k + "\": " + jsonToString(v) })
         .mkString(", ") + "}"
    case s : String => "\"" + s + "\""
    case x => x.toString
  }

  /*
   * This method takes input JSON values and sorts keys on objects.
   */
  def sortJSON(in : Any) : Any = in match {
    case l : List[_] => l.map(sortJSON)
    case m : Map[String,_] => TreeMap(m.mapElements(sortJSON).elements.toSeq : _*)
    // For the object versions, sort their contents, ugly casts and all...
    case JSONObject(data) => JSONObject(sortJSON(data).asInstanceOf[Map[String,Any]])
    case JSONArray(data) => JSONArray(sortJSON(data).asInstanceOf[List[Any]])
    case x => x
  }

  // For this one, just parsing should be considered a pass
  def printJSON(given : String) {
    JSON parseRaw given match {
      case None => println("Parse failed for \"%s\"".format(given))
      case Some(parsed) => println("Passed parse  : " + sortJSON(parsed))
    }
  }
   
  // For this usage, do a raw parse (to JSONObject/JSONArray)
  def printJSON(given : String, expected : JSONType) {
    printJSON(given, JSON.parseRaw, expected)
  }

  // For this usage, do a raw parse (to JSONType and subclasses)
  def printJSONFull(given : String, expected : Any) {
    printJSON(given, JSON.parseFull, expected)
  }

  // For this usage, do configurable parsing so that you can do raw if desired
  def printJSON[T](given : String, parser : String => T, expected : Any) {
    parser(given) match {
      case None => println("Parse failed for \"%s\"".format(given))
      case Some(parsed) => if (parsed == expected) {
        println("Passed compare: " + parsed)
      } else {
        val eStr = sortJSON(expected).toString
        val pStr = sortJSON(parsed).toString
        stringDiff(eStr,pStr)
      }
    }
  }

  def stringDiff (expected : String, actual : String) {
    if (expected != actual) {
      // Figure out where the Strings differ and generate a marker
        val mismatchPosition = expected.toList.zip(actual.toList).findIndexOf({case (x,y) => x != y}) match {
          case -1 => Math.min(expected.length, actual.length)
          case x => x
        }
        val reason = (" " * mismatchPosition) + "^"
        println("Expected: %s\nGot     : %s \n          %s".format(expected, actual, reason))

    } else {
      println("Passed compare: " + actual)
    }
  }


  // The library should differentiate between lower case "l" and number "1" (ticket #136)
  printJSON("{\"name\" : \"value\"}", JSONObject(Map("name" -> "value")))
  printJSON("{\"name\" : \"va1ue\"}", JSONObject(Map("name" -> "va1ue")))
  printJSON("{\"name\" : { \"name1\" : \"va1ue1\", \"name2\" : \"va1ue2\" } }",
            JSONObject(Map("name" -> JSONObject(Map("name1" -> "va1ue1", "name2" -> "va1ue2")))))

  // Unicode escapes should be handled properly
  printJSON("{\"name\" : \"\\u0022\"}")

  // The library should return a map for JSON objects (ticket #873)
  printJSONFull("{\"function\" : \"add_symbol\"}", Map("function" -> "add_symbol"))

  // The library should recurse into arrays to find objects (ticket #2207)
  printJSON("[{\"a\" : \"team\"},{\"b\" : 52}]", JSONArray(List(JSONObject(Map("a" -> "team")), JSONObject(Map("b" -> 52.0)))))
  
  // The library should differentiate between empty maps and lists (ticket #3284)
  printJSONFull("{}", Map()) 
  printJSONFull("[]", List())
  
  // Lists should be returned in the same order as specified
  printJSON("[4,1,3,2,6,5,8,7]", JSONArray(List[Double](4,1,3,2,6,5,8,7)))

  // Additional tests
  printJSON("{\"age\": 0}")

  // The library should do a proper toString representation using default and custom renderers (ticket #3605)
  stringDiff("{\"name\" : \"va1ue\"}", JSONObject(Map("name" -> "va1ue")).toString)
  stringDiff("{\"name\" : {\"name1\" : \"va1ue1\", \"name2\" : \"va1ue2\"}}",
             JSONObject(Map("name" -> JSONObject(TreeMap("name1" -> "va1ue1", "name2" -> "va1ue2")))).toString)

  stringDiff("[4.0, 1.0, 3.0, 2.0, 6.0, 5.0, 8.0, 7.0]", JSONArray(List[Double](4,1,3,2,6,5,8,7)).toString)

  // A test method that escapes all characters in strings
  def escapeEverything (in : Any) : String = in match {
    case s : String => "\"" + s.map(c => "\\u%04x".format(c : Int)).mkString + "\""
    case jo : JSONObject => jo.toString(escapeEverything)
    case ja : JSONArray => ja.toString(escapeEverything)
    case other => other.toString
  }

  stringDiff("{\"\\u006e\\u0061\\u006d\\u0065\" : \"\\u0076\\u0061\\u006c\"}", JSONObject(Map("name" -> "val")).toString(escapeEverything))

  println

  // from http://en.wikipedia.org/wiki/JSON
  val sample1 = """
{
    "firstName": "John",
    "lastName": "Smith",
    "address": {
        "streetAddress": "21 2nd Street",
        "city": "New York",
        "state": "NY",
        "postalCode": 10021
    },
    "phoneNumbers": [
        "212 732-1234",
        "646 123-4567"
    ]
}"""

  // Should be equivalent to:
  val sample1Obj = Map(
    "firstName" -> "John",
    "lastName" -> "Smith",
    "address" -> Map(
      "streetAddress" -> "21 2nd Street",
      "city" -> "New York",
      "state" -> "NY",
      "postalCode" -> 10021
    ),
    "phoneNumbers"-> List(
        "212 732-1234",
        "646 123-4567"
    )
  )

  
  printJSONFull(sample1, sample1Obj)
  println

  // from http://www.developer.com/lang/jscript/article.php/3596836
  val sample2 = """
{
   "fullname": "Sean Kelly",
   "org": "SK Consulting",
   "emailaddrs": [
      {"type": "work", "value": "kelly@seankelly.biz"},
      {"type": "home", "pref": 1, "value": "kelly@seankelly.tv"}
   ],
    "telephones": [
      {"type": "work", "pref": 1, "value": "+1 214 555 1212"},
      {"type": "fax", "value": "+1 214 555 1213"},
      {"type": "mobile", "value": "+1 214 555 1214"}
   ],
   "addresses": [
      {"type": "work", "format": "us",
       "value": "1234 Main StnSpringfield, TX 78080-1216"},
      {"type": "home", "format": "us",
       "value": "5678 Main StnSpringfield, TX 78080-1316"}
   ],
    "urls": [
      {"type": "work", "value": "http://seankelly.biz/"},
      {"type": "home", "value": "http://seankelly.tv/"}
   ]
}"""

  printJSON(sample2)
  println

  // from http://json.org/example.html
  val sample3 = """
{"web-app": {
  "servlet": [   
    {
      "servlet-name": "cofaxCDS",
      "servlet-class": "org.cofax.cds.CDSServlet",
      "init-param": {
        "configGlossary:installationAt": "Philadelphia, PA",
        "configGlossary:adminEmail": "ksm@pobox.com",
        "configGlossary:poweredBy": "Cofax",
        "configGlossary:poweredByIcon": "/images/cofax.gif",
        "configGlossary:staticPath": "/content/static",
        "templateProcessorClass": "org.cofax.WysiwygTemplate",
        "templateLoaderClass": "org.cofax.FilesTemplateLoader",
        "templatePath": "templates",
        "templateOverridePath": "",
        "defaultListTemplate": "listTemplate.htm",
        "defaultFileTemplate": "articleTemplate.htm",
        "useJSP": false,
        "jspListTemplate": "listTemplate.jsp",
        "jspFileTemplate": "articleTemplate.jsp",
        "cachePackageTagsTrack": 200,
        "cachePackageTagsStore": 200,
        "cachePackageTagsRefresh": 60,
        "cacheTemplatesTrack": 100,
        "cacheTemplatesStore": 50,
        "cacheTemplatesRefresh": 15,
        "cachePagesTrack": 200,
        "cachePagesStore": 100,
        "cachePagesRefresh": 10,
        "cachePagesDirtyRead": 10,
        "searchEngineListTemplate": "forSearchEnginesList.htm",
        "searchEngineFileTemplate": "forSearchEngines.htm",
        "searchEngineRobotsDb": "WEB-INF/robots.db",
        "useDataStore": true,
        "dataStoreClass": "org.cofax.SqlDataStore",
        "redirectionClass": "org.cofax.SqlRedirection",
        "dataStoreName": "cofax",
        "dataStoreDriver": "com.microsoft.jdbc.sqlserver.SQLServerDriver",
        "dataStoreUrl": "jdbc:microsoft:sqlserver://LOCALHOST:1433;DatabaseName=goon",
        "dataStoreUser": "sa",
        "dataStorePassword": "dataStoreTestQuery",
        "dataStoreTestQuery": "SET NOCOUNT ON;select test='test';",
        "dataStoreLogFile": "/usr/local/tomcat/logs/datastore.log",
        "dataStoreInitConns": 10,
        "dataStoreMaxConns": 100,
        "dataStoreConnUsageLimit": 100,
        "dataStoreLogLevel": "debug",
        "maxUrlLength": 500}},
    {
      "servlet-name": "cofaxEmail",
      "servlet-class": "org.cofax.cds.EmailServlet",
      "init-param": {
      "mailHost": "mail1",
      "mailHostOverride": "mail2"}},
    {
      "servlet-name": "cofaxAdmin",
      "servlet-class": "org.cofax.cds.AdminServlet"},
 
    {
      "servlet-name": "fileServlet",
      "servlet-class": "org.cofax.cds.FileServlet"},
    {
      "servlet-name": "cofaxTools",
      "servlet-class": "org.cofax.cms.CofaxToolsServlet",
      "init-param": {
        "templatePath": "toolstemplates/",
        "log": 1,
        "logLocation": "/usr/local/tomcat/logs/CofaxTools.log",
        "logMaxSize": "",
        "dataLog": 1,
        "dataLogLocation": "/usr/local/tomcat/logs/dataLog.log",
        "dataLogMaxSize": "",
        "removePageCache": "/content/admin/remove?cache=pages&id=",
        "removeTemplateCache": "/content/admin/remove?cache=templates&id=",
        "fileTransferFolder": "/usr/local/tomcat/webapps/content/fileTransferFolder",
        "lookInContext": 1,
        "adminGroupID": 4,
        "betaServer": true}}],
  "servlet-mapping": {
    "cofaxCDS": "/",
    "cofaxEmail": "/cofaxutil/aemail/*",
    "cofaxAdmin": "/admin/*",
    "fileServlet": "/static/*",
    "cofaxTools": "/tools/*"},
 
  "taglib": {
    "taglib-uri": "cofax.tld",
    "taglib-location": "/WEB-INF/tlds/cofax.tld"}
  }
}"""

  printJSON(sample3)
  println
}

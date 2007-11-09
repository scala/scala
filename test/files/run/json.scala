import scala.util.parsing.json._

object Test extends Application {
  def printJSON(s: String) {
    println(JSON parse s)
  }
  printJSON("{\"name\": \"value\"}")
  printJSON("{\"name\": \"va1ue\"}")  // ticket #136
  printJSON("{\"name\": { \"name1\": \"va1ue1\", \"name2\": \"va1ue2\" } }")
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
}""".mergeLines
  //println(sample1)
  printJSON(sample1)
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
}""".mergeLines
  //println(sample2)
  printJSON(sample2)
  println
}

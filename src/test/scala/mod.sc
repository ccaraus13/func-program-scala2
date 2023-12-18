import scala.util.matching.Regex
////4 % 0
// 1 % 4 //(0x4 +1)
// 2 % 4
// 3 % 4 //(0x4 +3)
// 4 % 4 //(1x4 +0)
// 5 % 4 //(1x4 +1)
//
//
//val regex = "^\"[a-zA-Z0-9]+\"$".r
//regex.matches("\"0sasa899\"")
//
//"null".matches("null")
//"null".matches("NULL")
//
val line = """"background-color":"#A03300""""
//
val propNameRegex = "\".+\"".r
val valueRegex = "\".*\"".r
val colonRegex = ":".r
val lineRegex = s"(${propNameRegex.regex})(${colonRegex.regex})(${propNameRegex.regex})".r
//val lineRegex = "(\".*\")(:)(\".*\")".r
//lineRegex.findAllMatchIn(line).map(aMatch => (aMatch.group(1),aMatch.group(3)) ).toList
//
//
lineRegex.matches(line)
for patternMatch <- lineRegex.findAllMatchIn(line) do
 println(s"key: ${patternMatch.group(1)} => ${patternMatch.group(3)} ")

// val keyValPattern: Regex = "([0-9a-zA-Z- ]+): ([0-9a-zA-Z-#()/. ]+)".r
//
// val input: String =
//  """background-color: #A03300;
//    |background-image: url(img/header100.png);
//    |background-position: top center;
//    |background-repeat: repeat-x;
//    |background-size: 2160px 108px;
//    |margin: 0;
//    |height: 108px;
//    |width: 100%;""".stripMargin
//
// for patternMatch <- keyValPattern.findAllMatchIn(input) do
//  println(s"key: ${patternMatch.group(1)} value: ${patternMatch.group(2)}")

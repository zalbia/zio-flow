import java.util.Locale

"abc".substring(0)
"abc".substring(1)
"abc".substring(2)

List(1, 2, 3, 2, 3).lastIndexOfSlice(List(2, 3))

"Bugün nasılsın!".toLowerCase(Locale.ROOT)
"Bugün nasılsın!".toLowerCase
"Bugün nasılsın!".toUpperCase(Locale.ROOT)
"Bugün nasılsın!".toLowerCase

val a = "Bugün nasılsın".toUpperCase(Locale.forLanguageTag("tr"))
val b = a.toLowerCase(Locale.forLanguageTag("tr"))

//  def regionMatches(toffset: Remote[Int], other: Remote[String], ooffset: Remote[Int], len: Remote[Int]): Remote[Boolean] =
//    !(ooffset < 0) || (toffset < 0) || (toffset > (Remote(0L) + Numeric.NumericLong.f)) ||

def replace(str: String, target: String, replacement: String): String =
  if (target == "") {
    replacement + str.headOption.map(_.toString).getOrElse("") +
      (if (str.isEmpty) str
       else replace(str.drop(1), target, replacement))
  } else {
    val occurrence = str.indexOf(target)
    if (occurrence == -1)
      str
    else
      str.take(occurrence) ++ replacement ++ replace(str.drop(occurrence + target.length), target, replacement)
  }

"foooo".replace("oo", "")
"foooo".replace("o", "a")
"faadaabaa".replace("aa", "")
"abcbcdbcbca".replace("bc", "de")
"foo".replace("", "a")
"foo".replace("a", "b")

"foo".indexOf("")

replace("foooo", "oo", "")
replace("foooo", "o", "a")
replace("faadaabaa", "aa", "")
replace("abcbcdbcbca", "bc", "de")
replace("foo", "", "a")
replace("foo", "a", "b")

"abcbcdbcbca"
"a" + "de"
5 > (10L - 1)

def regionMatches(str: String, toffset: Int, other: String, ooffset: Int, len: Int): Boolean = {
  val invalidParams = (ooffset < 0) || (toffset < 0) ||
    (toffset > str.length.toLong - len) ||
    (ooffset > other.length.toLong - len)
  if (invalidParams)
    false
  else
    str.slice(toffset, toffset + len) == other.slice(ooffset, ooffset + len)
}

"foobar".regionMatches(3, "bar", 0, 3)
"foobar".regionMatches(0, "bar", 0, 3)
"".regionMatches(0, "", 0, 1)

regionMatches("foobar", 3, "bar", 0, 3)
regionMatches("foobar", 0, "bar", 0, 3)
regionMatches("foobar", 0, "", 0, 1)

def repeat(str: String, count: Int): String =
  if (count > 0)
    str + repeat(str, count - 1)
  else
    ""

repeat("foo", -1)
repeat("foo", 0)
repeat("foo", 1)
repeat("foo", 5)

"".split("", 0)

"foobar".split('o')
"foobar".split(Array('o', 'b'))

"".indexWhere(_ == ' ')

val trimString = "    al; kjsv    "

trimString.indexWhere(_ > ' ')
trimString.lastIndexWhere(_ > ' ')

def trim(str: String): String =
  str.slice(str.indexWhere(_ > ' '), str.lastIndexWhere(_ > ' ') + 1)

" ".trim
trimString.trim
"a".trim
" a".trim
" a   ".trim
"\tabc   fo\t\t\t\nfoo ".trim

trim(" ")
trim(trimString)
trim("a")
trim(" a")
trim(" a   ")
trim("\tabc   fo\t\t\t\nfoo ")



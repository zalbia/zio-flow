import java.util.Locale
import scala.util.Try

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

def indexWhere[A](l: List[A])(p: A => Boolean, from: Int = 0): Int = {
  def loop(list: List[A], i: Int, acc: Int): Int =
    list match {
      case Nil          =>
        acc
      case head :: next =>
        if (p(head)) i else loop(next, i + 1, acc)
    }

  val index = loop(l.drop(from), 0, -1)
  if (index == -1) index else index + from
}

def indexOf[A](l: List[A], a: A, from: Int = 0): Int =
  indexWhere(l)(a == _, from)

List.empty[Int].indexWhere(_ < 3)
List(1, 2, 3).indexWhere(_ > 1)
List(1, 2, 3, 4, 5).indexWhere(_ < 5, 3)

indexWhere(List.empty[Int])(_ < 3)
indexWhere(List(1, 2, 3))(_ > 1)
indexWhere(List(1, 2, 3, 4, 5))(_ < 5, 3)

indexOf(List.empty[Int], 3)
indexOf(List(1, 2, 3), 4)
indexOf(List(1, 2, 3), 1)
indexOf(List(1, 2, 3, 4, 5), 5, 0)
indexOf(List(1, 2, 3, 4, 5), 5, 3)
indexOf(List(1, 2, 3, 4, 5), 5, 5)

List.empty[Int].indexOf(3)
List(1, 2, 3).indexOf(4)
List(1, 2, 3).indexOf(1)
List(1, 2, 3, 4, 5).indexOf(5, 0)
List(1, 2, 3, 4, 5).indexOf(5, 3)
List(1, 2, 3, 4, 5).indexOf(5, 5)

def lastIndexWhere[A](l: List[A])(p: A => Boolean, end: Int = l.length - 1): Int = {
  val lastIndex = indexWhere(l.reverse)(p, l.length - end - 1)
  if (lastIndex == -1) lastIndex else l.length - lastIndex - 1
}

def lastIndexOf[A](l: List[A], a: A)(end: Int = l.length - 1): Int =
  lastIndexWhere(l)(a == _, end)

List.empty[Int].lastIndexOf(3)
List(1, 2, 3).lastIndexOf(4)
List(1, 2, 3).lastIndexOf(1)
List(1, 2, 3, 2, 1).lastIndexOf(1)
List(5, 4, 5, 4, 3).lastIndexOf(5)

lastIndexOf(List.empty[Int], 3)()
lastIndexOf(List(1, 2, 3), 4)()
lastIndexOf(List(1, 2, 3), 1)()
lastIndexOf(List(1, 2, 3, 2, 1), 1)()
lastIndexOf(List(5, 4, 5, 4, 3), 5)()

def fill[A](size: Int)(elem: A): List[A] = {
  def loop(size: Int, list: List[A]): List[A] =
    if (size > 0)
      elem :: fill(size - 1)(elem)
    else
      list
  loop(size, Nil)
}

def padTo(s: String, len: Int, elem: Char): String =
  if (s.length >= len)
    s
  else
    s ++ fill(len - s.length)(elem).mkString

"a".padTo(-1, '*')
"a".padTo(Int.MinValue, '*')
"a".padTo(4, '*')
"a".padTo(5, '*')

padTo("a", -1, '*')
padTo("a", Int.MinValue, '*')
padTo("a", 4, '*')
padTo("a", 5, '*')

def substringOption(s: String, beginIndex: Int)(endIndex: Int = s.length): Option[String] =
  if ((beginIndex < 0) || (endIndex > s.length) || (beginIndex > endIndex))
    None
  else
    Some(s.slice(beginIndex, beginIndex + endIndex))

Try("food".substring(1))
Try("food".substring(0, 2))
Try("".substring(0))
Try("".substring(1))

substringOption("food", 1)()
substringOption("food", 0)(2)
substringOption("", 0)()
substringOption("", 1)()

def patch(s: String, from: Int, other: String, replaced: Int): String = {
  val actualFrom =
    if (from < 0)
      0
    else if (from > s.length)
      s.length
    else
      from
  s.take(actualFrom) ++ other ++ s.drop(actualFrom + replaced)
}

"foobar".patch(Int.MinValue, "x", 0)
"foobar".patch(0, "x", 0)
"foobar".patch(0, "x", 2)
"foobar".patch(0, "xx", 2)
"foobar".patch(0, "xxx", 2)
"foobar".patch(1, "xxx", 2)
"foobar".patch(3, "xxx", 2)
"foobar".patch(0, "xxx", 1)
"foobar".patch(0, "xxxxxx", 5)
"foobar".patch(0, "xxxxxx", 6)
"foobar".patch(3, "xxxxxx", 0)
"foobar".patch(3, "xxxxxx", 1)
"foobar".patch(3, "xxxxxx", 6)
"foobar".patch(Int.MaxValue, "xxxxxx", 6)
"foobar".patch(Int.MinValue, "xxx", 6)

patch("foobar", Int.MinValue, "x", 0)
patch("foobar", 0, "x", 0)
patch("foobar", 0, "x", 2)
patch("foobar", 0, "xx", 2)
patch("foobar", 0, "xxx", 2)
patch("foobar", 1, "xxx", 2)
patch("foobar", 3, "xxx", 2)
patch("foobar", 0, "xxx", 1)
patch("foobar", 0, "xxxxxx", 5)
patch("foobar", 0, "xxxxxx", 6)
patch("foobar", 3, "xxxxxx", 0)
patch("foobar", 3, "xxxxxx", 1)
patch("foobar", 3, "xxxxxx", 6)
patch("foobar", Int.MaxValue, "xxxxxx", 6)
patch("foobar", Int.MinValue, "xxx", 6)

def updated(s: String, index: Int, elem: Char): Option[String] =
  if ((0 <= index) && (index < s.length))
    Some(patch(s, index, elem.toString, 1))
  else
    None

Try("foo".updated(0, 'r'))
Try("boo".updated(1, 'r'))
Try("".updated(0, 'a'))

updated("foo", 0, 'r')
updated("boo", 1, 'r')
updated("", 0, 'a')

def mkString2(s: String, start: String, sep: String, end: String): String = {
  val sepChars = sep.toList.reverse
  start ++ s
    .foldLeft(List.empty[Char]) { (chars, ch) =>
      ch :: sepChars ::: chars
    }
    .reverse
    .drop(sep.length)
    .mkString ++ end
}

def mkString(s: String, sep: String): String =
  if (sep.isEmpty || s.length < 2)
    s
  else
    mkString2(s, "", sep, "")

"".mkString("a")
"foo".mkString("ab")

mkString("", "a")
mkString("foo", "ab")

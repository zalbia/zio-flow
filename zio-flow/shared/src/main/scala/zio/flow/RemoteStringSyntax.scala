package zio.flow

class RemoteStringSyntax(self: Remote[String]) {
  def ++(suffix: Remote[String]): Remote[String] = self.concat(suffix)

  def charAtOption(index: Remote[Int]): Remote[Option[Char]] =
    Remote.CharAtOption(self, index)

  def codepointAtOption(index: Remote[Int]): Remote[Option[Int]] =
    Remote.CodepointAtOption(self, index)

  def codepointBeforeOption(index: Remote[Int]): Remote[Option[Int]] =
    Remote.CodepointBeforeOption(self, index)

  def compareToIgnoreCase(that: Remote[String]): Remote[Int] =
    Remote.CompareIgnoreCase(self, that)

  def concat(suffix: Remote[String]): Remote[String] =
    Remote.ListToString(toList ++ suffix.toList)

  def contains(char: Remote[Char])(implicit d: DummyImplicit): Remote[Boolean] =
    toList.contains(char)

  def contains(substring: Remote[String]): Remote[Boolean] =
    toList.containsSlice(substring.toList)

  def indexOf(ch: Remote[Char]): Remote[Int] =
    indexOf(ch, 0)

  def indexOf(ch: Remote[Char], fromIndex: Remote[Int]): Remote[Int] =
    Remote.IndexOfCharFromIndex(self, ch.toInt, fromIndex)

  def indexOf(ch: Remote[Int])(implicit d: DummyImplicit): Remote[Int] =
    indexOf(ch, 0)

  def indexOf(ch: Remote[Int], fromIndex: Remote[Int])(implicit d: DummyImplicit): Remote[Int] =
    Remote.IndexOfCharFromIndex(self, ch, fromIndex)

  def indexOf(str: Remote[String])(implicit d: DummyImplicit, e: DummyImplicit): Remote[Int] =
    indexOf(str, 0)(d, e)

  def indexOf(str: Remote[String], fromIndex: Remote[Int])(implicit d: DummyImplicit, e: DummyImplicit): Remote[Int] =
    Remote.IndexOfStringFromIndex(self, str, fromIndex)

  def isEmpty: Remote[Boolean] =
    length === 0

  def lastIndexOf(ch: Remote[Char]): Remote[Int] =
    lastIndexOf(ch, length)

  def lastIndexOf(ch: Remote[Char], fromIndex: Remote[Int]): Remote[Int] =
    Remote.LastIndexOfCharFromIndex(self, ch.toInt, fromIndex)

  def lastIndexOf(ch: Remote[Int])(implicit d: DummyImplicit): Remote[Int] =
    lastIndexOf(ch, length)

  def lastIndexOf(ch: Remote[Int], fromIndex: Remote[Int])(implicit d: DummyImplicit): Remote[Int] =
    Remote.LastIndexOfCharFromIndex(self, ch, fromIndex)

  def lastIndexOf(str: Remote[String])(implicit d: DummyImplicit, e: DummyImplicit): Remote[Int] =
    Remote.LastIndexOfStringFromIndex(self, str, length)

  def lastIndexOf(str: Remote[String], fromIndex: Remote[Int])(implicit
    d: DummyImplicit,
    e: DummyImplicit
  ): Remote[Int] =
    Remote.LastIndexOfStringFromIndex(self, str, fromIndex)

  def length: Remote[Int] = Remote.Length(self)

  def reverse: Remote[String] = toList.reverse

  def toList: Remote[List[Char]] = Remote.StringToList(self)
}

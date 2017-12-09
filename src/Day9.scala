object Day9 {

  def main(args: Array[String]): Unit = {
    val input: String = readChallenge()
    testPart1()
    testPart2()
    println("The result for part 1 is: " + solvePartOne(input))
    println("The result for part 2 is: " + solvePartTwo(input))
  }

  def solvePartOne(input: String): Int = {
    val removeIgnoredRegex = """!.""".r
    val removeCarbageRegex = """(<.*?>)|(,)""".r
    val cleaned = removeCarbageRegex.replaceAllIn(removeIgnoredRegex.replaceAllIn(input, ""), "")

    val reduced = cleaned.foldLeft(0, 1) {
      (carry: (Int, Int), c) =>
        val (totalScore, base) = carry
        if(c == '{') (totalScore + base, base + 1)
        else (totalScore, base - 1)
    }

    reduced._1
  }

  def solvePartTwo(input: String): Int = {
    val removeIgnoredRegex = """!.""".r
    val cleaned = removeIgnoredRegex.replaceAllIn(input, "")

    val matchGarbge = """<(.*?)>""".r
    val garbage = matchGarbge.findAllIn(cleaned)

    garbage.map(_.length - 2).sum
  }

  def readChallenge(): String = {
    scala.io.Source.fromFile("./input/day9.txt").getLines.toList(0)
  }

  /**
    * For what is done here this is ok for testing, I know that this is not how it should be done for production code.
    */
  def testPart1(): Unit = {
    assert(1 == solvePartOne("""{<>}"""))
    assert(1 == solvePartOne("""{<random characters>}"""))
    assert(1 == solvePartOne("""{<<<<>}"""))
    assert(1 == solvePartOne("""{<{!>}>}"""))
    assert(1 == solvePartOne("""{<!!>}"""))
    assert(1 == solvePartOne("""{<!!!>>}"""))
    assert(1 == solvePartOne("""{<{o"i!a,<{i<a>}"""))
    assert(6 == solvePartOne("""{{{}}}"""))
    assert(5 == solvePartOne("""{{},{}}"""))
    assert(16 == solvePartOne("""{{{},{},{{}}}}"""))
    assert(1 == solvePartOne("""{<a>,<a>,<a>,<a>}"""))
    assert(9 == solvePartOne("""{{<ab>},{<ab>},{<ab>},{<ab>}}"""))
    assert(9 == solvePartOne("""{{<!!>},{<!!>},{<!!>},{<!!>}}"""))
    assert(3 == solvePartOne("""{{<a!>},{<a!>},{<a!>},{<ab>}}"""))
  }

  def testPart2(): Unit = {
    assert(0 == solvePartTwo("""{<>}"""))
    assert(17 == solvePartTwo("""{<random characters>}"""))
    assert(3 == solvePartTwo("""{<<<<>}"""))
    assert(2 == solvePartTwo("""{<{!>}>}"""))
    assert(0 == solvePartTwo("""{<!!>}"""))
    assert(0 == solvePartTwo("""{<!!!>>}"""))
    assert(10 == solvePartTwo("""{<{o"i!a,<{i<a>}"""))
  }
}

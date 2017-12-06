object Day5 {

  def main(args: Array[String]): Unit = {
    println("The result for part 1 is: " + solvePartOne(readChallenge()))
    println("The result for part 2 is: " + solvePartTwo(readChallenge()))
  }

  def solvePartOne(input: Array[Int]): Any = {
    var curPos: Int = 0;
    var steps: Int = 0;
    while (curPos < input.size) {
      val value = input(curPos)
      input.update(curPos, value + 1)
      curPos += value
      steps += 1
    }
    steps
  }

  def solvePartTwo(input: Array[Int]): Any = {
    var curPos: Int = 0;
    var steps: Int = 0;
    while (curPos < input.size) {
      val value = input(curPos)
      if (value < 3)
        input.update(curPos, value + 1)
      else
        input.update(curPos, value - 1)
      curPos += value
      steps += 1
    }
    steps
  }

  def readChallenge(): Array[Int] = {
    scala.io.Source.fromFile("./input/day5.txt").getLines.map(_.toInt).toArray
  }
}
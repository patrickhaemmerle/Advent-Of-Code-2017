object Day2 {
  def main(args: Array[String]): Unit = {
    println("The result for part one is: " + checksumPart1())
    println("The result for part two is: " + checksumPart2())
  }

  def checksumPart1(): Int = {
    readChallenge().map(line => line.max - line.min).sum
  }

  def checksumPart2(): Int = {
    val numbers = readChallenge()
    val sortedNumbers = numbers.map(_.sorted)
    sortedNumbers.map(findEvenDivision(_)).sum
  }

  def findEvenDivision(numbers: Array[Int]): Int = {
    numbers.map(divisor => {
      numbers.foldLeft(0) { (sum: Int, dividend: Int) =>
        if (dividend % divisor == 0 && dividend != divisor) (sum + dividend / divisor)
        else sum
      }
    }).sum
  }

  def readChallenge(): Array[Array[Int]] = {
    val challenge = scala.io.Source.fromFile("./input/day2.txt").getLines.toArray
    val strings = challenge.map(_.split("\\s"))
    strings.map(_.map(_.toInt))
  }

}

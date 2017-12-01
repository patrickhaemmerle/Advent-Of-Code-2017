object Day1 {
  def main(args: Array[String]): Unit = {
    val challenge = readChallenge()
    val answer1 = solveCaptcha(challenge, 1)
    val answer2 = solveCaptcha(challenge, challenge.length / 2)

    println("Result for part one is: " + answer1)
    println("Result for part two is: " + answer2)
  }

  def solveCaptcha(challenge: String, stepSize: Int): Int = {

    val list = {
      val arrayOfStrings = challenge.split("")
      arrayOfStrings.map(_.toInt).toList
    }

    def captchaStep(carry: (Int, Int), x: Int): (Int, Int) = {
      val (sum, lastIndex) = carry

      val lastIndexNew = {
        if (lastIndex + 1 < list.size) lastIndex + 1
        else 0
      }

      if (list(lastIndex) == x) (sum + x, lastIndexNew)
      else (sum, lastIndexNew)
    }

    val firstIndex = list.size - stepSize
    val carry = (0, firstIndex)
    val result = list.foldLeft(carry) {
      captchaStep
    }

    result._1
  }

  def readChallenge(): String = {
    scala.io.Source.fromFile("./input/day1.txt").getLines.reduceLeft(_ + _)
  }

}

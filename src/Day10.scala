object Day10 {

  def main(args: Array[String]): Unit = {
    test()
    println("The result for part 1 is: " + part1(challenge(), 256))
    println("The result for part 2 is: " + part2(challengeFromAscii(), 256))
  }

  def part1(input: Array[Int], size: Int): Int = {
    val numbers: Array[Int] = (0 to size - 1).toArray
    val newNumbers = oneHashRound(input, numbers)._1

    newNumbers(0) * newNumbers(1)
  }

  def part2(input: Array[Int], size: Int): String = {
    val numbers: Array[Int] = (0 to size - 1).toArray

    val result = (1 to 64).foldLeft((numbers, 0, 0))((carry: (Array[Int], Int, Int), _) => {
      oneHashRound(input, carry._1, carry._2, carry._3)
    })

    result._1.grouped(16).map(_.reduceLeft(_ ^ _)).map(x => {
      val h = x.toHexString
      if (h.length == 1) "0" + h
      else h
    }).reduceLeft(_ + _)
  }

  def oneHashRound(input: Array[Int], numbers: Array[Int], position: Int = 0, skipSize: Int = 0): (Array[Int], Int, Int) = {
    input.foldLeft((numbers, position, skipSize))(
      (carry: (Array[Int], Int, Int), length: Int) => {
        val (numbers, position, skipSize) = carry

        val newNumbers = reverse(numbers, position, length)
        val newPosition = (position + length + skipSize) % numbers.length

        (newNumbers, newPosition, skipSize + 1)
      })
  }

  def reverse(numbers: Array[Int], position: Int, length: Int): Array[Int] = {
    val reordered = numbers.slice(position, numbers.length) ++ numbers.slice(0, position)
    val reversed = reordered.slice(0, length).reverse ++ reordered.slice(length, reordered.length)

    reversed.slice(reversed.length - position, reversed.length) ++ reversed.slice(0, reversed.length - position)
  }

  def challenge(challenge: String = null): Array[Int] = {
    scala.io.Source.fromFile("./input/day10.txt").getLines.toList(0).split(",").map(_.toInt)
  }

  def challengeFromAscii(challenge: String = null): Array[Int] = {
    {
      if (challenge == null) scala.io.Source.fromFile("./input/day10.txt").getLines.toList(0)
      else challenge
    }.toCharArray().map(_.toInt) ++ Array(17, 31, 73, 47, 23)
  }

  def test(): Unit = {
    // Part 1
    assert(Array(2, 1, 0, 3, 4).deep == reverse(Array(0, 1, 2, 3, 4), 0, 3).deep)
    assert(Array(4, 3, 0, 1, 2).deep == reverse(Array(2, 1, 0, 3, 4), 3, 4).deep)
    assert(Array(4, 3, 0, 1, 2).deep == reverse(Array(4, 3, 0, 1, 2), 1, 1).deep)
    assert(Array(3, 4, 2, 1, 0).deep == reverse(Array(4, 3, 0, 1, 2), 1, 5).deep)
    assert(Array(4, 3, 0, 1, 2).deep == reverse(Array(4, 3, 0, 1, 2), 3, 0).deep)
    assert(12 == part1(Array(3, 4, 1, 5), 5))

    // Part 2
    assert(Array(49, 44, 50, 44, 51, 17, 31, 73, 47, 23).deep == challengeFromAscii("1,2,3").deep)
    assert("a2582a3a0e66e6e86e3812dcb672a272" == part2(challengeFromAscii(""), 256))
    assert("33efeb34ea91902bb2f59c9920caa6cd" == part2(challengeFromAscii("AoC 2017"), 256))
    assert("3efbe78a8d82f29979031a4aa0b16a9d" == part2(challengeFromAscii("1,2,3"), 256))
    assert("63960835bcdc130f0b66d7ff4f6a5a8e" == part2(challengeFromAscii("1,2,4"), 256))
  }

}
import scala.collection.mutable

object Day3 {

  def main(args: Array[String]): Unit = {
    println("The result for part one is: " + distancePart1(readChallenge()))
    println("The result for part two is: " + numberPart2(readChallenge()))
  }

  def distancePart1(n: Int): Int = {
    /**
      * This solution iterates over all fields in the "Spiral-Memory" and calculates its distance to the root => O(n)
      * complexity. k denotes the the circle number (counted from the root field as circle number 0) and is at the same
      * time the shortest path possible for a number on this circle. The longest path on this circle is 2*k.
      *
      *   - The first number on circle k has a path length of 2*k-1 to the root field.
      *   - The next 2*k-2, ... , k, k+1, k+2, ... , 2*k, k, 2*k ...
      *   - This pattern goes up and down until it ends at the last field of the circle with a path of 2*k.
      *
      * The first field of every circle has the number ((k-1)*k/2)*8 + 2 and the last field on a circle has the number
      * ((k+1)*k/2)*8 + 1. Basically I think it should be possible to exploit this knowledge to not visit every field
      * in the "Spiral Memory" to calculate the distance of the wanted field, thus reaching a complexity which is
      * below O(n).
      *
      * Basically a simpler version of this also would have worked for the first task. When knowing the position of the
      * target field on the matrix it's easy to calculate the manhattan distance.
      */
    var k = 0
    var distance = 0
    var currentNumber = 1
    var descending = true
    for (currentNumber <- 1 to n) {
      if (((k + 1) * k / 2) * 8 + 2 == currentNumber) {
        k += 1
        distance = 2 * k - 1
        descending = true
      } else {
        if (distance == k) descending = false
        if (distance == 2 * k) descending = true

        if (descending) distance -= 1
        else distance += 1
      }
    }
    return distance
  }

  def numberPart2(max: Int): Int = {
    /**
      * To get the solution of this its obviously not possible to find an algorithm below a complexity of O(n).
      * Nevertheless the algorithm is more straightforward than the first task. Starting at the root field we make
      * the following moves:
      *  - 1 right
      *  - 1 up
      *  - 2 left
      *  - 2 down
      *  - 3 right
      *  - 3 up
      *  - 4 left
      *  - 4 down
      *  - ...
      *
      *  While moving, we fill the matrix with the values and at the first vale greater than the challenge value, we
      *  finish and return that value.
      */
    var field = (0, 0)
    var stepCount = 1
    var direction = 0
    var matrix: mutable.Map[(Int, Int), Int] = mutable.Map[(Int, Int), Int]((0, 0) -> 1)
    var n = 0

    while (true) {
      for (_ <- 1 to stepCount) {
        if (direction == 0) field = (field._1, field._2 + 1) // right
        if (direction == 1) field = (field._1 - 1, field._2) // up
        if (direction == 2) field = (field._1, field._2 - 1) // left
        if (direction == 3) field = (field._1 + 1, field._2) // down

        val value = calculateFieldValue(field, matrix)

        /** ************************************/
        if (value > max) return value
        /** ************************************/

        matrix += (field -> value)
      }

      direction += 1

      if (direction == 2) {
        stepCount += 1
      }
      if (direction == 4) {
        direction = 0
        stepCount += 1
      }
    }

    -1
  }

  def calculateFieldValue(pos: (Int, Int), map: mutable.Map[(Int, Int), Int]): Int = {
    val list = List(
      map.getOrElse((pos._1 - 1, pos._2 - 1), 0),
      map.getOrElse((pos._1, pos._2 - 1), 0),
      map.getOrElse((pos._1 + 1, pos._2 - 1), 0),
      map.getOrElse((pos._1 - 1, pos._2), 0),
      map.getOrElse((pos._1 + 1, pos._2), 0),
      map.getOrElse((pos._1 - 1, pos._2 + 1), 0),
      map.getOrElse((pos._1, pos._2 + 1), 0),
      map.getOrElse((pos._1 + 1, pos._2 + 1), 0)
    )
    list.sum
  }

  def readChallenge(): Int = {
    scala.io.Source.fromFile("./input/day3.txt").getLines.next.toInt
  }
}

import scala.collection.mutable

object Day6 {

  def main(args: Array[String]): Unit = {
    println("The result for part 1 is: " + solvePartOne(readChallenge()))
    println("The result for part 2 is: " + solvePartTwo(readChallenge()))
  }

  def solvePartOne(input: Array[Int]): String = {
    val steps: Int = debug(input)
    return steps.toString
  }

  def solvePartTwo(input: Array[Int]): String = {
    var steps: Int = debug(input, true)
    return steps.toString
  }

  def debug(input: Array[Int], doSecondLoop: Boolean = false) : Int = {
    val seen: mutable.MutableList[Array[Int]] = mutable.MutableList()
    var steps = 0
    var allocation: Array[Int] = input
    while (!haveISeenThis(allocation, seen)) {
      steps += 1
      seen += allocation.clone()
      allocation = reAllocate(input)
    }
    if(doSecondLoop) debug(allocation)
    else steps
  }

  def findNext(input: Array[Int]) : Int = {
    val max: Int = input.max
    for(i:Int <- 0 to input.size-1) {
      if(input(i) == max) return i
    }
    throw new IllegalStateException()
  }

  def haveISeenThis(current: Array[Int], seen: Iterable[Array[Int]]): Boolean = {
    seen.foreach(cmp => {
      if(current.deep == cmp.deep) return true
    })
    false
  }

  def reAllocate(input: Array[Int]) : Array[Int] = {
    var index: Int = findNext(input)
    var bag: Int = input(index)
    input(index) = 0
    while(bag > 0) {
      index += 1
      if(index > input.length - 1) index = 0
      bag -= 1
      input(index) += 1
    }
    input
  }

  def readChallenge(): Array[Int] = {
    val lines = scala.io.Source.fromFile("./input/day6.txt").getLines.toArray
    lines(0).split("\t").map(_.toInt)
  }

}
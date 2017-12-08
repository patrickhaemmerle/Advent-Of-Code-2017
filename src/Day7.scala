import scala.collection.mutable

object Day7 {

  class Node {
    var name: String = null
    var parent: Node = null
    var weight: Int = 0
    var cumulativeWeight: Int = 0
    val children: mutable.MutableList[Node] = mutable.MutableList()

    override def toString(): String = {
      "Node -> Name: " + name + " - Parent: " + {
        if (parent != null) parent.name
      } + " - Weight: " + weight + " - CumulativeWeight: " + cumulativeWeight + " - Children: " + children.map(x => {
        x.name + " - " + x.cumulativeWeight
      })
    }
  }

  def main(args: Array[String]): Unit = {
    val nodes: List[Node] = readChallenge()
    println("The result for part 1 is: " + solvePartOne(nodes))
    println("The result for part 2 is: " + solvePartTwo(nodes))
  }

  def solvePartOne(nodes: List[Node]): String = {
    return findRoot(nodes).name
  }

  def solvePartTwo(nodes: List[Node]): String = {
    val root: Node = findRoot(nodes)
    return findDifference(root).toString
  }

  def findDifference(node: Node) : Int = {
    if(isBalanced(node)) {
      return 0
    } else {
      return {
        if(node.children.map(isBalanced(_)).reduceLeft(_ && _)) {
          val cumulativeWeight = {
            if(node.parent.children(0).cumulativeWeight != node.cumulativeWeight) {
              node.parent.children(0).cumulativeWeight
            } else {
              node.parent.children(1).cumulativeWeight
            }
          }

          val diff = node.cumulativeWeight - cumulativeWeight
          node.children.reduceLeft((l, r) => {
          if(diff > 0) {
              if(l.cumulativeWeight < r.cumulativeWeight) r
              else l
            } else {
              if(l.cumulativeWeight > r.cumulativeWeight) r
              else l
            }
          }).weight - diff
        } else {
          node.children.map(findDifference(_)).sum
        }
      }
    }
  }

  def isBalanced(node: Node) : Boolean = {
      if (node.children.isEmpty) true
      else {
        var balance = true
        val w = node.children(0).cumulativeWeight
        node.children.map(_.cumulativeWeight).foreach(x => {
          if (x != w) balance = false
        })
        balance
      }
  }

  def findRoot(nodes: List[Node]): Node = {
    nodes.foreach(node => {
      if (node.parent == null) return node
    })
    throw new IllegalStateException
  }

  def readChallenge(): List[Node] = {
    val regex = """(\S*) \((\d*)\)( -> (.*))?""".r
    val lines = scala.io.Source.fromFile("./input/day7.txt").getLines.toList

    // Initial Read of Nodes
    val nodes: List[Node] = lines.map(line => {
      val m = regex.findAllIn(line)
      val node: Node = new Node()
      node.name = m.group(1)
      node.weight = m.group(2).toInt
      node
    })

    // Now fill the parents and the children
    lines.map(line => {
      val m = regex.findAllIn(line)
      val children = {
        if (m.group(4) != null) m.group(4).split(", ").toList
        else List()
      }
      val node: Node = findNode(m.group(1), nodes)
      for (child <- children) {
        val childNode = findNode(child, nodes)
        childNode.parent = node
        node.children += childNode
      }
      node
    })

    // And calculate the cumulative weights
    calculateCumulativeWeightOnTree(findRoot(nodes))

    def calculateCumulativeWeightOnTree(node: Node): Unit = {
      node.children.foreach(calculateCumulativeWeightOnTree(_))
      node.cumulativeWeight = node.weight + node.children.map(_.cumulativeWeight).sum
    }

    nodes
  }

  def findNode(name: String, nodes: List[Node]): Node = {
    nodes.foreach(node => {
      if (node.name == name) return node
    })
    return null
  }
}
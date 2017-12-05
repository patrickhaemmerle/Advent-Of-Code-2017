object Day4 {

  def main(args: Array[String]) : Unit = {

    val policy1Count = countValidPassphrasesPolicy1(readChallenge())
    println("According to policy one there are " + policy1Count + " valid passphrases")

    val policy2Count = countValidPassphrasesPolicy2(readChallenge())
    println("According to policy two there are " + policy2Count + " valid passphrases")

  }

  def countValidPassphrasesPolicy1(passphrases: Iterator[String]) : Int = {
    passphrases.foldLeft(0){
      (count: Int, pass: String) => {
        if(isPassphraseValidPolicy1(pass)) count + 1
        else count
      }
    }
  }

  def isPassphraseValidPolicy1(pass: String) : Boolean = {
    pass.split(" ").sorted.reduceLeft((last, current) => {
      if(last == current) return false
      else current
    })
    true
  }

  def countValidPassphrasesPolicy2(passphrases: Iterator[String]) = {
    passphrases.foldLeft(0){
      (count: Int, pass: String) => {
        if(isPassphraseValidPolicy2(pass)) count + 1
        else count
      }
    }
  }

  def isPassphraseValidPolicy2(pass: String) : Boolean = {
    val sorted = pass.split(" ").map(_.sorted).sorted
    sorted.reduceLeft((last, current) => {
      if(last == current) return false
      else current
    })
    true
  }

  def readChallenge(): Iterator[String] = {
    scala.io.Source.fromFile("./input/day4.txt").getLines
  }
}

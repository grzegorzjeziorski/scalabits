package grzegorzjeziorski

// Link to problem: https://www.hackerrank.com/challenges/valid-bst
object ValidBST {

  def splitListByElem(list: List[Int], split: Int): (List[Int], List[Int]) = {
    (list.takeWhile(_ <= split), list.dropWhile(_ <= split))
  }

  def hasToSmallElement(list: List[Int], elem: Int): Boolean = {
    list match {
      case Nil => false
      case h :: t =>
        if (h <= elem) {
          return true
        } else {
          hasToSmallElement(t, elem)
        }
    }
  }

  def isTreeValid(list: List[Int]): Boolean = {
    list match {
      case Nil => true
      case h :: Nil => true
      case h :: t => {
        val subtries = splitListByElem(t, h)
        if (hasToSmallElement(subtries._2, h)) {
          false
        } else {
          isTreeValid(subtries._1) && isTreeValid(subtries._2)
        }
      }
    }
  }

  def validateSingleTree(): Unit = {
    val size = readInt
    val input = readLine.trim().split(" ").toList.map(_.toInt)
    if (isTreeValid(input)) {
      println("YES")
    } else {
      println("NO")
    }
  }

  def main(args: Array[String]) {
    val N = readInt
    for (i <- 1 to N) {
      validateSingleTree
    }
  }

}
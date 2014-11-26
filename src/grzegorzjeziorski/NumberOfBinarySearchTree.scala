package grzegorzjeziorski

// Link to problem: https://www.hackerrank.com/challenges/number-of-binary-search-tree
object NumberOfBinarySearchTree {
  
  val limit: Int = 1001
  val modulo: Long = 100000007
  var bsts: Array[Long] = new Array[Long](limit);

  def populateBSTs(n: Int): Unit = {
    var res: Long = 0
    for (i <- 1 to n) {
      res = ((res % modulo) + ((bsts(i - 1) * bsts(n - i)) % modulo)) % modulo
    }
    bsts(n) = res
  }

  def populateAllBSTs(n: Int): Unit = {
    if (n == 0) {
      bsts(0) = 1
      populateAllBSTs(1)
    } else if (n < limit) {
      populateBSTs(n)
      populateAllBSTs(n + 1)
    }
  }

  def readInput(n: Int): Unit = {
    if (n > 0) {
      println(bsts(readInt))
      readInput(n - 1)
    }
  }

  def main(args: Array[String]) {
    populateAllBSTs(0)
    readInput(readInt)
  }
  
}
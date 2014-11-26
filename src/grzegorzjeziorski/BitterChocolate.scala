package grzegorzjeziorski

// Link to problem: https://www.hackerrank.com/challenges/bitter-chocolate
object BitterChocolate {

  var size = 26
  // true == win
  var chocolates = Array.ofDim[Boolean](size, size, size)

  def isBitter(rowNo: Int, colNo: Int): Boolean = {
    rowNo == 0 && colNo == 0
  }

  def isInside(row0: Int, row1: Int, row2: Int, rowNo: Int, colNo: Int): Boolean = {
    (rowNo == 0 && colNo < row0) || (rowNo == 1 && colNo < row1) || (rowNo == 2 && colNo < row2)
  }

  // row0 jest zawsze najdłuższy i jest na dole
  def canWin(row0: Int, row1: Int, row2: Int): Boolean = {
    for (rowNo <- 0 to 2) {
      for (colNo <- 0 to size) {
        if (isInside(row0, row1, row2, rowNo, colNo) && !isBitter(rowNo, colNo)) {
          // przelicz współrzędne
          if (rowNo == 2) {
            val newRow2 = math.min(row2, colNo)
            if (!chocolates(row0)(row1)(newRow2)) {
              return true
            }
          } else if (rowNo == 1) {
            val newRow2 = math.min(row2, colNo)
            val newRow1 = math.min(row1, colNo)
            if (!chocolates(row0)(newRow1)(newRow2)) {
              return true
            }
          } else {
            val newRow2 = math.min(row2, colNo)
            val newRow1 = math.min(row1, colNo)
            val newRow0 = math.min(row0, colNo)
            if (!chocolates(newRow0)(newRow1)(newRow2)) {
              return true
            }
          }
        }
      }
    }
    return false
  }

  def populateChocolate(): Unit = {
    var row0 = 0
    var row1 = 0
    var row2 = 0
    while (row0 < size && row1 < size && row2 < size) {
      chocolates(row0)(row1)(row2) = canWin(row0, row1, row2)
      if (row0 == row1 && row1 == row2) {
        row0 = row0 + 1
        row1 = 0
        row2 = 0
      } else if (row1 == row2) {
        row1 = row1 + 1
        row2 = 0
      } else {
        row2 = row2 + 1
      }
    }
  }

  def main(args: Array[String]) {
    populateChocolate()
    val n = readInt
    for (i <- 1 to n) {
      val list = readLine.trim.split(" ").toList.map(_.toInt)
      if (chocolates(list(0))(list(1))(list(2))) {
        println("WIN")
      } else {
        println("LOSE")
      }
    }
  }

}
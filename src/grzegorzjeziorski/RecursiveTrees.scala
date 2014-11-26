package grzegorzjeziorski

// Link to problem: https://www.hackerrank.com/challenges/fractal-trees
object RecursiveTrees {

  def belongsToSingleTree(rootRow: Int, rootCol: Int, height: Int, rowNo: Int, colNo: Int): Boolean = {
    (rootCol == colNo && rootRow >= rowNo && rootRow - height / 2 < rowNo) || (math.abs(rootCol - colNo) == rootRow - height / 2 + 1 - rowNo && rowNo > rootRow - height)
  }

  def belongsToTree(level: Int, rootRow: Int, rootCol: Int, height: Int, rowNo: Int, colNo: Int): Boolean = {
    if (level == 0) {
      false
    } else {
      belongsToSingleTree(rootRow, rootCol, height, rowNo, colNo) || belongsToTree(level - 1, rootRow - height, rootCol - height / 2, height / 2, rowNo, colNo) || belongsToTree(level - 1, rootRow - height, rootCol + height / 2, height / 2, rowNo, colNo)
    }
  }

  def drawLine(level: Int, rootRow: Int, rootCol: Int, initialHeight: Int, width: Int, rowNo: Int): Unit = {
    def loop(idx: Int): Unit = {
      if (idx < width) {
        if (belongsToTree(level, rootRow, rootCol, initialHeight, rowNo, idx)) {
          print(1)
          loop(idx + 1)
        } else {
          print("_")
          loop(idx + 1)
        }
      }
    }
    loop(0)
  }

  def drawTree(level: Int, rootRow: Int, rootCol: Int, height: Int, width: Int): Unit = {
    def loop(idx: Int): Unit = {
      if (idx < height) {
        drawLine(level, rootRow, rootCol, height / 2, width, idx)
        println("")
        loop(idx + 1)
      }
    }
    loop(1)
  }

  def main(args: Array[String]) {
    drawTree(readInt(), 63, 49, 64, 100)
  }

}
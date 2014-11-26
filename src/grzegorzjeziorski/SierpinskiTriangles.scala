package grzegorzjeziorski

// Link to problem: https://www.hackerrank.com/challenges/functions-and-fractals-sierpinski-triangles
object SierpinskiTriangles {

  def insideTriangle(width: Int, rowNo: Int, colNo: Int): Boolean = {
    colNo >= (width / 2 - rowNo) && colNo <= (width / 2 + rowNo)
  }

  def toRemoveSingleLevel(peakRow: Int, peakCol: Int, height: Int, rowNo: Int, colNo: Int): Boolean = {
    rowNo > (peakRow - height) && rowNo <= peakRow && colNo >= (peakCol - (peakRow - rowNo)) && colNo <= (peakCol + (peakRow - rowNo))
  }

  def toRemove(level: Int, peakRow: Int, peakCol: Int, height: Int, rowNo: Int, colNo: Int): Boolean = {
    if (level == 0) {
      false
    } else {
      toRemoveSingleLevel(peakRow, peakCol, height, rowNo, colNo) || toRemove(level - 1, peakRow, peakCol - height, height / 2, rowNo, colNo) || toRemove(level - 1, peakRow, peakCol + height, height / 2, rowNo, colNo) || toRemove(level - 1, peakRow - height, peakCol, height / 2, rowNo, colNo)
    }
  }

  def drawLine(level: Int, peakRow: Int, peakCol: Int, initialHeight: Int, width: Int, rowNo: Int): Unit = {
    def loop(idx: Int): Unit = {
      if (idx < width) {
        if (insideTriangle(width, rowNo, idx) && !toRemove(level, peakRow, peakCol, initialHeight, rowNo, idx)) {
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

  def drawSierpinski(level: Int, peakRow: Int, peakCol: Int, height: Int, width: Int): Unit = {
    def loop(idx: Int): Unit = {
      if (idx < height) {
        drawLine(level, peakRow, peakCol, height / 2, width, idx)
        println("")
        loop(idx + 1)
      }
    }
    loop(0)
  }

  def main(args: Array[String]) {
    drawSierpinski(readInt(), 31, 31, 32, 63)
  }

}
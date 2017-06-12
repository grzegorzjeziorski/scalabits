package grzegorzjeziorski

// Link to problem: https://www.hackerrank.com/challenges/sam-and-substrings
object SamAndSubstrings {

  val modulo = 1000000007L

  def sumsOfDigits(list: List[Long], currentValue: Long, counter: Long, acc: List[Long]): List[Long] = {
    list match {
      case Nil => acc
      case h :: t => {
        val newCurrentValue = (h * counter + currentValue) % modulo
        sumsOfDigits(t, newCurrentValue, counter + 1, newCurrentValue :: acc)
      }
    }
  }

  def combineWithDecimals(list: List[Long], multiplier: Long, result: Long): Long = {
    list match {
      case Nil => result
      case h :: t => {
        val toAdd = (multiplier * h) % modulo
        val newMultiplier = (multiplier * 10) % modulo
        val newResult = (result + toAdd) % modulo
        combineWithDecimals(t, newMultiplier, newResult)
      }
    }
  }

  def main(args: Array[String]) {
    val inputArray = readLine().trim.toList.map(character => Integer.parseInt(character.toString).toLong)
    val sums = sumsOfDigits(inputArray, 0L, 1L, Nil)
    println(combineWithDecimals(sums, 1L, 0L))
  }

}

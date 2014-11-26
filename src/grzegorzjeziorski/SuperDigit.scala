package grzegorzjeziorski

// Link to problem: https://www.hackerrank.com/challenges/super-digit
object SuperDigit {

  def sumOfDigits(str: List[Char]): Long = {
    str match {
      case Nil => 0
      case h :: t => (h.toLong - 48l) + sumOfDigits(t)
    }
  }

  def sumOfDigits(n: Long): Long = {
    if (n < 10) {
      n
    } else {
      (n % 10) + sumOfDigits(n / 10)
    }
  }

  def superDigitPreprocessing(n: String, l: Long): Long = {
    sumOfDigits(n.toList) * l
  }

  def superDigit(l: Long): Long = {
    if (l < 10) {
      return l
    } else {
      superDigit(sumOfDigits(l))
    }
  }

  def main(args: Array[String]) {
    val input = readLine().trim().split(" ")
    println(superDigit(superDigitPreprocessing(input(0), input(1).toLong)))
  }

}
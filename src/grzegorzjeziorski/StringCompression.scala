package grzegorzjeziorski

// Link to problem: https://www.hackerrank.com/challenges/string-compression
object StringCompression {

  def stringCompression(str: String): List[Char] = {
    def convertLastCharAndCounter(lastChar: Char, counter: Int): List[Char] = {
      if (counter > 1) {
        counter.toString.toList.reverse ::: List(lastChar)
      } else {
        List(lastChar)
      }
    }

    def loop(input: List[Char], lastChar: Char, counter: Int, acc: List[Char]): List[Char] = {
      input match {
        case Nil => convertLastCharAndCounter(lastChar, counter) ::: acc
        case h :: t =>
          if (h == lastChar) {
            loop(t, lastChar, counter + 1, acc)
          } else {
            loop(t, h, 1, convertLastCharAndCounter(lastChar, counter) ::: acc)
          }
      }
    }

    if (str.isEmpty()) {
      List()
    } else {
      str.toList match {
        case h :: t => loop(t, h, 1, List())
      }
    }
  }

  def main(args: Array[String]) {
    println(stringCompression(readLine()).mkString.reverse)
  }

}
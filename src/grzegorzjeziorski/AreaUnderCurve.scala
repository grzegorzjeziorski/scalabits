package grzegorzjeziorski

// Link to problem: https://www.hackerrank.com/challenges/area-under-curves-and-volume-of-revolving-a-curv
object AreaUnderCurve {

  def positivePow(param: Double, expotent: Int): Double = {
    if (expotent == 0) {
      1
    } else {
      param * pow(param, expotent - 1)
    }
  }

  def pow(param: Double, expotent: Int): Double = {
    if (expotent >= 0) {
      positivePow(param, expotent)
    } else {
      positivePow(1.0 / param, -expotent)
    }
  }

  def single(param: Double, coefficient: Int, power: Int): Double = {
    coefficient.toDouble * pow(param, power)
  }

  def g(param: Double, coefficients: List[Int], powers: List[Int], acc: Double): Double = {
    (coefficients, powers) match {
      case (Nil, Nil) => acc
      case (hc :: tc, hp :: tp) => g(param, tc, tp, acc + single(param, hc, hp))
    }
  }

  def f(coefficients: List[Int], powers: List[Int], x: Double): Double = {
    g(x, coefficients, powers, 0.0)
  }

  def area(coefficients: List[Int], powers: List[Int], x: Double): Double = {
    val fval = f(coefficients, powers, x)
    math.Pi * fval * fval
  }

  def summation(func: (List[Int], List[Int], Double) => Double, upperLimit: Int, lowerLimit: Int, coefficients: List[Int], powers: List[Int]): Double = {
    val step = 0.001
    def loop(currX: Double, acc: Double): Double = {
      if (currX >= upperLimit) {
        acc
      } else {
        loop(currX + step, func(coefficients, powers, currX) * step + acc)
      }
    }

    loop(lowerLimit.toDouble, 0)
  }

  def displayAnswers(coefficients: List[Int], powers: List[Int], limits: List[Int]) {
    println(summation(f, limits.reverse.head, limits.head, coefficients, powers))
    println(summation(area, limits.reverse.head, limits.head, coefficients, powers))
  }

  def main(args: Array[String]) {
    /** Purely IO Section **/
    displayAnswers(readLine().trim().split(" ").toList.map(_.toInt), readLine().trim().split(" ").toList.map(_.toInt), readLine().trim().split(" ").toList.map(_.toInt))
  }

}
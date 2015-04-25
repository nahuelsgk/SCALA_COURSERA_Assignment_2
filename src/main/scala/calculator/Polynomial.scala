package calculator

import math.sqrt

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
      println("Computando delta")
      Signal((b()*b())-(4*a()*b()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    println("Computando solucion")
    
    Signal(
        delta() match {
          case negative if negative <  0.0 => Set()
          case zero     if zero     == 0.0 => Set((b() * -1)/(2*a()))
          case positive if positive >  0.0 => Set(
              (((b() * -1) + sqrt(delta()))/(2*a()))
              ,
              (((b() * -1) - sqrt(delta()))/(2*a()))
          )
        }
    )
  }
}

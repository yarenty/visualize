package com.yarenty.bezier

/**
 * Created by yarenty on 02/09/2015.
 */
class Casteljau {

  def normalize(points: List[(Int, Double)]): List[(Int, Double)] = {

    val n = points.length
    val out: Array[(Int, Double)] = new Array[(Int, Double)](n)


    out(0) = (points(0)._1, (0.25 * points(0)._2 + 0.5 * points(0)._2 + 0.25 * points(1)._2))
    for (i <- 1 to n - 2) {
      out(i) = (points(i)._1, (0.25 * points(i - 1)._2 + 0.5 * points(i)._2 + 0.25 * points(i + 1)._2))
    }
    out(n - 1) = (points(n - 1)._1, (0.25 * points(n - 2)._2 + 0.5 * points(n - 1)._2 + 0.25 * points(n - 1)._2))
    out.toList
  }

  def interpolate(points: List[Double]): List[Double] = {
    val n = points.length
    val out: Array[Double] = new Array[Double](n)


    out(0) = 0.25 * points(0) + 0.5 * points(0) + 0.25 * points(1)
    for (i <- 1 to n - 2) {
      out(i) = 0.25 * points(i - 1) + 0.5 * points(i) + 0.25 * points(i + 1)
    }
    out(n - 1) = 0.25 * points(n - 2) + 0.5 * points(n - 1) + 0.25 * points(n - 1)
    out.toList
  }

  def extend(points: List[Double], times: Int): List[Double] = {
    val n = points.length
    val size = n * times
    val out: Array[Double] = new Array[Double](size + 1)

    var p0 = points(0)
    var p1 = points(0)
    var p2 = points(1)
    var p3 = points(2)

    var idx = 0
    var t = 0.0

    //TODO: this is wrong I need to go two times through each of points
    for (i <- 0 to n - 1) {
      for (j <- -times to times) {

        idx = i * times + j //this is wrong! idsx should be from -M to +M
        t = (times + j).toDouble / (2 * times).toDouble

        if (i < 1) p0 = points(0) else points(i - 1) //start
        p1 = points(i)
        if (i >= (n - 1)) p2 = points(n - 1) else p2 = points(i + 1) //end

        if (idx > 0)
          out(idx) += ((1 - t) * (1 - t) * p0 + 2 * (1 - t) * t * p1 + t * t * p2) / 3.0
      }
    }

    out.toList
    // interpolate(out.toList)

  }

  def extendWithIndex(points: List[Double], times: Int): List[(Int, Double)] = {
    val idx = extend(points, times).zipWithIndex
    for (i <- idx) yield (i._2, i._1)
  }

  def extendCubic(points: List[Double], times: Int): List[Double] = {
    val n = points.length
    val size = n * times
    val out: Array[Double] = new Array[Double](size)

    var p0 = points(0)
    var p1 = points(0)
    var p2 = points(1)
    var p3 = points(2)

    var x = 0
    var t = 0.0

    // TODO
    for (i <- 0 to size - 1) {
      x = i / times
      t = (i % times) / times.toDouble

      if (i < times) p0 = points(0) else points(x - 1) //start
      p1 = points(x)
      if (x >= (n - 1)) p2 = points(n - 1) else p2 = points(x + 1) //end
      if (x >= (n - 2)) p3 = points(n - 1) else p3 = points(x + 2) //end

      out(i) = (1 - t) * (1 - t) * (1 - t) * p0 + 3 * (1 - t) * (1 - t) * t * p1 + 3 * (1 - t) * t * t * p2 + t * t * t * p3
    }

    out.toList

  }

  def extendCubicWithIndex(points: List[Double], times: Int): List[(Int, Double)] = {
    val idx = extendCubic(points, times).zipWithIndex
    for (i <- idx) yield (i._2, i._1)
  }

}

object Casteljau {


  val initial = List((0, 0.5), (1, 0.66), (2, 0.0), (3, 0.0), (4, 0.0), (5, 0.83), (6, 0.0), (7, 0.0), (8, 0.34), (9, 0.0), (10, 0.22))

  def main(args: Array[String]) {

    val normalized = new Casteljau().normalize(initial)
    val fin = new Casteljau().extend(initial.map(y => y._2), 4)
    //normalized.foreach((y, x) => s" ($y,$x) ")

    println(normalized mkString (","))

    println(fin mkString (","))


  }
}
package yirg.dispersion

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import yirg.dispersion.Dispersion.close
import yirg.dispersion.DispersionSpec._

class DispersionSpec extends AnyFlatSpec with Matchers {

  "single value" must "work" in {
    Dispersion(List(5)) mustBe Aggregate(5, 5, 5, 0, 1)
  }

  "three values" must "work" in {
    val stuff = Dispersion[Int] + Seq(3, 4, 5)
    stuff mustBe Aggregate(5, 5, 4, 2, 3)
  }

  "inc and dec" must "work" in {
    val stuff = Dispersion[Int].inc.inc.dec.inc.inc.dec.dec.dec
    stuff mustBe Aggregate(0, 3, 1.5, 6, 8)
    stuff mustBe Dispersion(Seq(1, 2, 1, 2, 3, 2, 1, 0))
  }

  "some prop" must "pass" in {
    val vs = List(0.toShort)
    val result = Dispersion(vs).mean
    assert(close(result, mean(vs)))
  }

  "empty sums" must "work" in {
    Dispersion[Int] ++ Dispersion[Int] mustBe Dispersion[Int]
  }

  "one empty sum" must "work" in {
    Dispersion[Int] ++ Dispersion(List(5)) mustBe Dispersion(List(5))
  }

  "neither empty sum" must "work" in {
    Dispersion(List(5)) ++ Dispersion(List(5)) mustBe Dispersion(List(5, 5))
  }

  "reverse" must "work" in {
    val vs = List(-804571865, -2147483648)
    assert(Dispersion(vs) ~= Dispersion(vs.reverse))
  }

  "short" must "mean" in {
    val vs = List(-32768, 1, 0, 1, 32767)
    assert(close(mean(vs), Dispersion(vs).mean))
  }

}

/**
 * helpers to calculate dispersion metrics
 * using the standard all-at-once algorithms
 */
object DispersionSpec {

  def mean[T](item: Iterable[T])(implicit n : Numeric[T]): Double =
    item.map(n.toDouble).sum / item.size

  def variance[T](items: Iterable[T])(implicit n : Numeric[T]) : Double = {

    val itemMean = mean(items)
    val count = items.size

    val sumOfSquares = items.foldLeft(0.0d) {
      (total, item) =>
        val itemDbl = n.toDouble(item)
        val square = math.pow(itemDbl - itemMean, 2)
        total + square
    }

    sumOfSquares / count.toDouble
  }

  def deviation[T](items: Iterable[T])(implicit n : Numeric[T]) : Double = {
    math.sqrt(variance(items))
  }

}

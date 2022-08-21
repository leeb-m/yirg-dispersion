package yirg.dispersion

import yirg.dispersion.Dispersion.close

import scala.Double.NaN

/**
 * Welford's Online Algorithm
 * Supports algebraic composition
 * TODO: Time windowed streams
 */
trait Dispersion[A] {

  implicit val evidence: Numeric[A]

  def last: A
  def max: A
  def mean: Double

  def m2: Double
  def samples: Long

  def variance: Double = {
    if (samples < 2) NaN else m2 / samples
  }

  def deviation: Double = Math.sqrt(variance)

  def inc: Dispersion[A] = last match {
    case NaN => this + evidence.one
    case n => this + evidence.plus(n, evidence.one)
  }

  def dec: Dispersion[A] = last match {
    case NaN => this + evidence.negate(evidence.one)
    case n => this + evidence.minus(n, evidence.one)
  }

  def +(value: A): Aggregate[A]

  def +(values: Seq[A]): Dispersion[A] = {
    values.foldLeft(this)(_ + _)
  }

  def ++(that: Dispersion[A]): Dispersion[A]

  def --(that: Dispersion[A]): Dispersion[A]

  def ~=(that: Dispersion[A]): Boolean
}

/**
 * placeholder value is used to avoid options
 * it is returned initially from last and max but
 * does not affect the dispersion of future values
 * zero is generally a good value when counting
 * you may prefer NaN, or a previous mean
 */
case class Initial[A](placeholder: A)(implicit val evidence: Numeric[A]) extends Dispersion[A] {

  val last: A = placeholder
  val max: A = placeholder

  val mean: Double = NaN

  val m2: Double = 0 // no deviance
  val samples: Long = 0

  def +(value: A): Aggregate[A] = {
    Aggregate(value, value, evidence.toDouble(value), 0, 1)(evidence)
  }

  override def ++(that: Dispersion[A]): Dispersion[A] = that

  // TODO: I'm not sure what implementation should be here
  override def --(that: Dispersion[A]): Dispersion[A] = that

  override def ~=(that: Dispersion[A]) = that match {
    case Initial(_) => true
    case _ => false
  }
}

case class Aggregate[A](last: A,
                        max: A,
                        mean: Double,
                        m2: Double,
                        samples: Long)
                       (implicit val evidence: Numeric[A]) extends Dispersion[A] {

  def +(value: A): Aggregate[A] = {

    // Compute the new count, new mean, and the new M2.
    // mean accumulates the mean of the entire dataset
    // M2 aggregates the squared distance from the mean
    // count aggregates the number of samples seen so far

    val newSamples = samples + 1
    val delta = evidence.toDouble(value) - mean
    val newMean = mean + (delta / newSamples)
    val delta2 = evidence.toDouble(value) - newMean
    val newM2 = m2 + (delta * delta2)

    val newMax = if (evidence.gt(value, max)) value else max

    Aggregate(value, newMax, newMean, newM2, newSamples)(evidence)
  }

  override def ++(that: Dispersion[A]): Dispersion[A] = that match {
    case Initial(_) => this
    case Aggregate(last, max, mean, m2, samples) =>

      val newMax = evidence.max(this.max, max)

      // parallel algorithm – Chan et al
      val newSamples = this.samples + samples
      val sigma = mean - this.mean
      val newMean = this.mean + sigma * samples / newSamples
      val sampleProduct = this.samples * samples
      val newM2 = this.m2 + m2 + sigma * sigma * sampleProduct / newSamples

      Aggregate(last, newMax, newMean, newM2, newSamples)(evidence)
  }

  override def --(that: Dispersion[A]): Dispersion[A] = that match {
    case Initial(_) => this
    case Aggregate(last, max, mean, m2, samples) =>

      val newMax = evidence.max(this.max, max)

      // parallel algorithm – Chan et al
      val newSamples = this.samples - samples
      val sigma = mean - this.mean
      val newMean = this.mean + sigma * samples / newSamples
      val sampleProduct = this.samples * samples
      val newM2 = this.m2 + m2 + sigma * sigma * sampleProduct / newSamples

      Aggregate(last, newMax, newMean, newM2, newSamples)(evidence)
  }

  /**
   * order independence and tolerant maths on the doubles
   */
  override def ~=(that: Dispersion[A]) = that match {
    case Initial(_) => false
    case Aggregate(last, max, mean, m2, samples) =>
      // println(s"$this =~ $that")
      // println(s"last == this.last: ${last == this.last}")
      // println(s"max == this.max: ${max == this.max}")
      // println(s"close(mean, this.mean): ${close(mean, this.mean)}")
      // println(s"close(m2, this.m2): ${close(m2, this.m2)}")
      // println(s"samples == samples: ${samples == samples}")
      max == this.max &&
      close(mean, this.mean) &&
      close(m2, this.m2) &&
      samples == samples
  }
}

object Dispersion {

  def apply[A](implicit ev: Numeric[A]): Initial[A] = Initial(ev.zero)

  def apply[A](values: Seq[A])(implicit ev: Numeric[A]): Dispersion[A] = {
    Initial(ev.zero) + values
  }

  def apply[A](sentinel: A)(implicit ev: Numeric[A]): Dispersion[A] = {
    Initial(sentinel)
  }

  /**
   * the numeric error is large for large numbers
   * so scaling the error by the expected value
   * instead of using org.scalactic.Tolerance._
   */
  private[dispersion] def close(expect: Double, that: Double): Boolean = {
    val delta = if (expect == 0) expect - that
    else (expect - that) / expect
    // if (Math.abs(delta) > 0.000000001) println(s"$expect was not $that")
    Math.abs(delta) < 0.0000000001
  }

}
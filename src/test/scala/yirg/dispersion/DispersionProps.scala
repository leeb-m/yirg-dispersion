package yirg.dispersion

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Arbitrary._
import yirg.dispersion.Dispersion.close
import yirg.dispersion.DispersionSpec.{deviation, mean, variance}

object DispersionProps extends Properties("Dispersion") {

  // skipping longs and doubles to save the hassle
  // of having to filter the universal properties
  // to keep intermediate values from overflowing
  include(props[Int]("Int"))
  include(props[Float]("Float"))
  include(props[Char]("Char"))
  include(props[Byte]("Byte"))
  include(props[Short]("Short"))

  def props[A : Numeric : Arbitrary](name: String) = new Properties(name) {

    property("left identity") = forAll {
      (vs: Seq[A]) => Dispersion[A] ++ Dispersion(vs) == Dispersion(vs)
    }

    property("right identity") = forAll {
      (vs: Seq[A]) => Dispersion(vs) ++ Dispersion[A] == Dispersion(vs)
    }

    property("adding a sequence is the same as adding in sequence") = forAll { (vs: Seq[A]) =>
      Dispersion[A] + vs ~= vs.foldLeft(Dispersion[A]: Dispersion[A])(_ + _)
    }

    property("mean is mean") = forAll { (vs: Seq[A]) =>
      vs.nonEmpty ==> close(mean(vs), Dispersion[A](vs).mean)
    }

    property("deviation is deviation") = forAll { (vs: Seq[A]) =>
      vs.length > 2 ==> close(deviation(vs), Dispersion[A](vs).deviation)
    }

    property("variance is variance") = forAll { (vs: Seq[A]) =>
      vs.length > 2 ==> close(variance(vs), Dispersion[A](vs).variance)
    }

    property("reverse") = forAll { (vs: Seq[A]) =>
      Dispersion(vs) ~= Dispersion(vs.reverse)
    }

    property("sum of two dispersions is the dispersion of the sum") =
      forAll { (vs1: Seq[A], vs2: Seq[A]) =>
        Dispersion[A](vs1) ++ Dispersion[A](vs2) ~= Dispersion[A](vs1 ++ vs2)
      }

    property("swap order") =
      forAll { (vs1: Seq[A], vs2: Seq[A]) =>
        Dispersion(vs1) ++ Dispersion(vs2) ~= Dispersion(vs2 ++ vs1)
      }

  }

}

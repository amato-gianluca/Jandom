package it.unich.jandom.utils.breeze

import scala.language.implicitConversions
import scala.annotation.tailrec
import breeze.math.Field
import breeze.storage.Zero
import breeze.linalg._
import breeze.linalg.operators._
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import java.security.InvalidParameterException

class Rational(val numer: Int, val denom: Int) extends Ordered[Rational] {

  def unary_+ = this

  def unary_- = new Rational(-numer, denom)

  def +(that: Rational) = Rational(that.numer * denom + numer * that.denom, denom * that.denom)

  def -(that: Rational) = this + -that

  def *(that: Rational) = Rational(numer * that.numer, denom * that.denom)

  def /(that: Rational) = Rational(numer * that.denom, denom * that.numer)

  def %(that: Rational) = this - that * Rational((this / that).toInt)

  override def equals(that: Any): Boolean =
    that match {
      case that: Rational => this.numer == that.numer && this.denom == that.denom
      case _ => false
    }

  def compare(that: Rational): Int = numer * that.denom + denom * that.numer

  def toByte: Byte = (numer / denom).toByte

  def toChar: Char = (numer / denom).toChar

  def toInt: Int = numer / denom

  def toLong: Long = (numer / denom).toLong

  def toFloat: Float = numer.toFloat / denom

  def toDouble: Double = numer.toDouble / denom

  def abs = new Rational(numer.abs, denom)

  def pow(that: Int) = new Rational(Math.pow(numer, that).toInt, Math.pow(denom, that).toInt)

  def max(that: Rational) = if (this < that) that else this

  def min(that: Rational) = if (this < that) this else that

  def isWhole = (numer % denom) == 0

  def isInfinity = false

  def isPosInfinity = false

  def isNegInfinity = false

  override def toString = numer + "/" + denom

}

object Rational {
  outer =>

  val zero = Rational(0)

  val one = Rational(1)

  val minusone = Rational(-1)

  def NegativeInfinity: Rational = ???

  def PositiveInfinity: Rational = ???

  @tailrec
  private def gcd(x: Int, y: Int): Int =
    if (y == 0) Math.abs(x) else gcd(y, x % y)

  @tailrec
  private def gcd(x: Long, y: Long): Long =
    if (y == 0) Math.abs(x) else gcd(y, x % y)

  @tailrec
  private def gcd(x: BigInt, y: BigInt): BigInt =
    if (y == 0) x.abs else gcd(y, x % y)

  def apply(initialNumer: BigInt, initialDenom: BigInt): Rational = {
    val thisGcd = gcd(initialNumer, initialDenom)
    val numer = (if (initialDenom < 0) -initialNumer else initialNumer) / thisGcd
    val denom = if (numer == 0) BigInt(1) else initialDenom.abs / thisGcd
    if (numer.isValidInt && denom.isValidInt)
      new Rational(numer.toInt, denom.toInt)
    else {
      println(numer)
      println(denom)
      throw new IllegalArgumentException("Cannot represent this number")
    }

  }

  def apply(initialNumer: Long, initialDenom: Long): Rational = {
    val thisGcd = gcd(initialNumer, initialDenom)
    val numer = (if (initialDenom < 0) -initialNumer else initialNumer) / thisGcd
    val denom = if (numer == 0) 1 else Math.abs(initialDenom) / thisGcd
    if (numer.isValidInt && denom.isValidInt)
      new Rational(numer.toInt, denom.toInt)
    else
      throw new IllegalArgumentException("Cannot represent this number")
  }

  def apply(initialNumer: Int, initialDenom: Int): Rational = {
    require(initialDenom != 0, "denominator must be nonzero")
    val thisGcd = gcd(initialNumer, initialDenom)
    val numer = (if (initialDenom < 0) -initialNumer else initialNumer) / thisGcd
    val denom = if (numer == 0) 1 else Math.abs(initialDenom) / thisGcd
    new Rational(numer, denom)
  }

  def apply(x: Double): Rational = {
    if (x == 0)
      Rational.zero
    else if (x.isNaN)
      throw new InvalidParameterException("Cannot convert NaN to Rational")
    else if (x.isInfinity)
      throw new InvalidParameterException("Cannot convert infinite numbers to Rational")
    else {
      val bits = java.lang.Double.doubleToLongBits(x)
      val value = if ((bits >> 63) < 0) -(bits & 0x000FFFFFFFFFFFFFL | 0x0010000000000000L)
      else (bits & 0x000FFFFFFFFFFFFFL | 0x0010000000000000L)
      val exp = ((bits >> 52) & 0x7FF).toInt - 1075 // 1023 + 52
      if (exp > 10) {
        apply(BigInt(value) << exp, BigInt(1))
      } else if (exp >= 0) {
        apply(value << exp, 1L)
      } else if (exp >= -52 && (~((-1L) << (-exp)) & value) == 0L) {
        apply(value >> (-exp), 1L)
      } else {
        apply(BigInt(value), BigInt(1) << (-exp))
      }
    }
  }

  implicit def apply(x: Int): Rational = new Rational(x, 1)

  implicit object RationalIsFractional extends Fractional[Rational] {
    def compare(x: Rational, y: Rational) = x compare y
    def fromInt(x: Int) = Rational(x)
    def minus(x: Rational, y: Rational) = x - y
    def negate(x: Rational) = -x
    def plus(x: Rational, y: Rational) = x + y
    def times(x: Rational, y: Rational) = x * y
    def div(x: Rational, y: Rational) = x / y
    def toDouble(x: Rational) = x.toDouble
    def toFloat(x: Rational) = x.toFloat
    def toInt(x: Rational) = x.toInt
    def toLong(x: Rational) = x.toLong
  }

  implicit object RationalIsField extends Field[Rational] {
    val zero = outer.zero
    val one = outer.one
    def ==(a: Rational, b: Rational) = a == b
    def !=(a: Rational, b: Rational) = a != b
    def +(a: Rational, b: Rational) = a + b
    def -(a: Rational, b: Rational) = a - b
    def *(a: Rational, b: Rational) = a * b
    def /(a: Rational, b: Rational) = a / b
    def %(a: Rational, b: Rational) = a % b
    def max(a: Rational, b: Rational) = a max b
    def min(a: Rational, b: Rational) = a min b

    def >(a: Rational, b: Rational) = a > b
    def >=(a: Rational, b: Rational) = a >= b
    def <(a: Rational, b: Rational) = a < b
    def <=(a: Rational, b: Rational) = a <= b

    def pow(a: Rational, b: Rational) =
      if (b.isWhole)
        a pow b.toInt
      else
        throw new IllegalArgumentException("Cannot raise a rational to a non-integer power")

    val normImpl: norm.Impl[Rational, Double] = new norm.Impl[Rational, Double] {
      def apply(v: Rational): Double = v.toDouble.abs
    }
  }

  implicit object RationalZero extends Zero[Rational] {
    val zero = outer.zero
  }

  implicit def dv_s_Op_Rational_OpMulMatrix: OpMulMatrix.Impl2[DenseVector[Rational], Rational, DenseVector[Rational]] =
    new OpMulMatrix.Impl2[DenseVector[Rational], Rational, DenseVector[Rational]] {
      def apply(a: DenseVector[Rational], b: Rational): DenseVector[Rational] = {
        val ad = a.data
        var aoff = a.offset
        val result = DenseVector.zeros[Rational](a.length)
        val rd = result.data

        var i = 0
        while (i < a.length) {
          rd(i) = ad(aoff) * b
          aoff += a.stride
          i += 1
        }
        result
      }
      implicitly[BinaryRegistry[Vector[Rational], Rational, OpMulMatrix.type, Vector[Rational]]].register(this)
    }

  implicit object implOpSolveMatrixBy_DRR_DRR_eq_DRR
      extends OpSolveMatrixBy.Impl2[DenseMatrix[Rational], DenseMatrix[Rational], DenseMatrix[Rational]] {

    def LUSolve(X: DenseMatrix[Rational], A: DenseMatrix[Rational]) = {
      var perm = (0 until A.rows).toArray
      for (i <- 0 until A.rows) {
        val optPivot = (i until A.rows) find { p => A(perm(p), perm(i)) != Rational.zero }
        val pivotRow = optPivot.getOrElse(throw new MatrixSingularException())
        val tmp = perm(i)
        perm(i) = perm(pivotRow)
        perm(pivotRow) = tmp
        val pivot = A(perm(i), perm(i))
        for (j <- i + 1 until A.rows) {
          val coeff = A(perm(j), perm(i)) / pivot
          A(perm(j), ::) -= A(perm(i), ::) * coeff
          X(perm(j), ::) -= X(perm(i), ::) * coeff
        }
      }
      for (i <- A.rows - 1 to (0, -1)) {
        X(perm(i), ::) /= A(perm(i), perm(i))
        for (j <- i - 1 to (0, -1)) {
          X(perm(j), ::) -= X(perm(i), ::) * A(perm(j), perm(i))
        }
      }
    }

    override def apply(A: DenseMatrix[Rational], V: DenseMatrix[Rational]): DenseMatrix[Rational] = {
      require(A.rows == V.rows, "Non-conformant matrix sizes")

      if (A.size == 0) {
        DenseMatrix.zeros[Rational](0, 0)
      } else if (A.rows == A.cols) {
        val X = DenseMatrix.zeros[Rational](V.rows, V.cols)
        val Y = DenseMatrix.zeros[Rational](A.rows, A.cols)
        // square: LUSolve
        X := V
        Y := A
        LUSolve(X, Y)
        X
      } else
        ???
    }
  }

  implicit object implOpSolveMatrixBy_DMR_DVR_eq_DVR
      extends OpSolveMatrixBy.Impl2[DenseMatrix[Rational], DenseVector[Rational], DenseVector[Rational]] {
    override def apply(a: DenseMatrix[Rational], b: DenseVector[Rational]): DenseVector[Rational] = {
      val rv: DenseMatrix[Rational] = a \ new DenseMatrix[Rational](b.size, 1, b.data, b.offset, b.stride, true)
      new DenseVector[Rational](rv.data)
    }
  }

  implicit def countFromTraverseRational[T](implicit traverse: CanTraverseValues[T, Rational]): countNonZero.Impl[T, Int] = {
    new countNonZero.Impl[T, Int] {
      def apply(t: T): Int = {
        var count: Int = 0
        traverse.traverse(t, new ValuesVisitor[Rational] {
          def visit(a: Rational) = { if (a != Rational.zero) count += 1 }
          def zeros(count: Int, zeroValue: Rational) {}
        })
        count
      }
    }
  }
}

package zio.flow

sealed trait Numeric[A] {
  def schema: Schema[A]

  def fromLong(l: Long): Remote[A]
  def add(left: A, right: A): A
  def multiply(left: A, right: A): A
  def divide(left: A, right: A): A
  def pow(left: A, right: A): A
  def negate(left: A): A
  def log(left: A, right: A): A
  def root(left: A, right: A): A
}

object Numeric extends NumericImplicits0 {

  implicit object NumericlInt extends Numeric[Int] {
    override def fromLong(l: Long): Remote[Int]  = Remote(l.toInt)
    override def add(left: Int, right: Int): Int = left + right

    override def multiply(left: Int, right: Int): Int = left * right

    override def divide(left: Int, right: Int): Int = left / right

    override def pow(left: Int, right: Int): Int = Math.pow(left, right).toInt

    override def negate(left: Int): Int = -left

    override def log(left: Int, right: Int): Int = (Math.log(left) / Math.log(right)).toInt

    override def root(left: Int, right: Int): Int = Math.pow(left, 1.0 / right).toInt

    def schema: Schema[Int] = implicitly[Schema[Int]]
  }
}
sealed trait NumericImplicits0 {

  implicit case object NumericShort extends Numeric[Short] {
    override def fromLong(l: Long): Remote[Short]      = Remote(l.toShort)
    override def add(left: Short, right: Short): Short = (left + right).toShort

    override def multiply(left: Short, right: Short): Short = (left * right).toShort

    override def divide(left: Short, right: Short): Short = (left / right).toShort

    override def pow(left: Short, right: Short): Short = Math.pow(left, right).toShort

    override def negate(left: Short): Short = (-left).toShort

    override def log(left: Short, right: Short): Short = (Math.log(left) / Math.log(right)).toShort

    override def root(left: Short, right: Short): Short = Math.pow(left, 1.0 / right).toShort

    def schema: Schema[Short] = implicitly[Schema[Short]]
  }

  implicit case object NumericLong extends Numeric[Long] {
    override def fromLong(l: Long): Remote[Long]         = Remote(l)
    override def add(left: Long, right: Long): Long      = left + right
    override def multiply(left: Long, right: Long): Long = left * right

    override def divide(left: Long, right: Long): Long = left / right

    override def pow(left: Long, right: Long): Long = Math.pow(left, right).toLong

    override def negate(left: Long): Long = -left

    override def log(left: Long, right: Long): Long = (Math.log(left) / Math.log(right)).toLong

    override def root(left: Long, right: Long): Long = Math.pow(left, 1.0 / right).toLong

    def schema: Schema[Long] = implicitly[Schema[Long]]
  }

  implicit case object NumericBigInt extends Numeric[BigInt] {
    override def fromLong(l: Long): Remote[BigInt]        = Remote(BigInt(l))
    override def add(left: BigInt, right: BigInt): BigInt = left + right

    override def multiply(left: BigInt, right: BigInt): BigInt = left * right

    override def divide(left: BigInt, right: BigInt): BigInt = left / right

    override def pow(left: BigInt, right: BigInt): BigInt = Math.pow(left.doubleValue, right.doubleValue).toInt

    override def negate(left: BigInt): BigInt = -left

    override def log(left: BigInt, right: BigInt): BigInt =
      (Math.log(left.doubleValue) / Math.log(right.doubleValue)).toInt

    override def root(left: BigInt, right: BigInt): BigInt = Math.pow(left.doubleValue, 1.0 / right.doubleValue).toInt

    def schema: Schema[BigInt] = implicitly[Schema[BigInt]]
  }

  implicit case object NumericFloat extends Numeric[Float] {
    override def fromLong(l: Long): Remote[Float]      = Remote(l.toFloat)
    override def add(left: Float, right: Float): Float = left + right

    override def multiply(left: Float, right: Float): Float = left * right

    override def divide(left: Float, right: Float): Float = left / right

    override def pow(left: Float, right: Float): Float = Math.pow(left, right).toFloat

    override def negate(left: Float): Float = -left

    override def log(left: Float, right: Float): Float = (Math.log(left) / Math.log(right)).toFloat

    override def root(left: Float, right: Float): Float = Math.pow(left, 1.0 / right).toFloat

    def schema: Schema[Float] = implicitly[Schema[Float]]
  }

  implicit case object NumericDouble extends Numeric[Double] {
    override def fromLong(l: Long): Remote[Double]        = Remote(l.toDouble)
    override def add(left: Double, right: Double): Double = left + right

    override def multiply(left: Double, right: Double): Double = left * right

    override def divide(left: Double, right: Double): Double = left / right

    override def pow(left: Double, right: Double): Double = Math.pow(left, right)

    override def negate(left: Double): Double = -left

    override def log(left: Double, right: Double): Double = Math.log(left) / Math.log(right)

    override def root(left: Double, right: Double): Double = Math.pow(left, 1.0 / right)

    def schema: Schema[Double] = implicitly[Schema[Double]]
  }

  implicit case object NumericBigDecimal extends Numeric[BigDecimal] {
    override def fromLong(l: Long): Remote[BigDecimal]                = Remote(BigDecimal(l))
    override def add(left: BigDecimal, right: BigDecimal): BigDecimal = left + right

    override def multiply(left: BigDecimal, right: BigDecimal): BigDecimal = left * right

    override def divide(left: BigDecimal, right: BigDecimal): BigDecimal = left / right

    override def pow(left: BigDecimal, right: BigDecimal): BigDecimal = Math.pow(left.doubleValue, right.doubleValue)

    override def negate(left: BigDecimal): BigDecimal = -left

    override def log(left: BigDecimal, right: BigDecimal): BigDecimal = BigDecimal(
      Math.log(left.doubleValue) / Math.log(right.doubleValue)
    )

    override def root(left: BigDecimal, right: BigDecimal): BigDecimal =
      Math.pow(left.doubleValue, 1.0 / right.doubleValue)

    def schema: Schema[BigDecimal] = implicitly[Schema[BigDecimal]]
  }
}
sealed trait Fractional[A] extends Numeric[A] {
  def fromDouble(const: Double): A

  def schema: Schema[A]

  def sin(a: A): A = ???

  def inverseSin(a: A): A = ???
}

object Fractional {

  implicit case object FractionalFloat extends Fractional[Float] {
    def fromDouble(const: Double): Float = const.toFloat

    override def fromLong(l: Long): Remote[Float]      = Remote(l.toFloat)
    override def add(left: Float, right: Float): Float = left + right

    override def multiply(left: Float, right: Float): Float = left * right

    override def divide(left: Float, right: Float): Float = left / right

    override def pow(left: Float, right: Float): Float = Math.pow(left, right).toFloat

    override def negate(left: Float): Float = -left

    override def log(left: Float, right: Float): Float = (Math.log(left) / Math.log(right)).toFloat

    override def root(left: Float, right: Float): Float = Math.pow(left, 1.0 / right).toFloat

    def schema: Schema[Float] = implicitly[Schema[Float]]
  }

  implicit case object FractionalDouble extends Fractional[Double] {
    def fromDouble(const: Double): Double                 = const.toDouble
    override def fromLong(l: Long): Remote[Double]        = Remote(l.toDouble)
    override def add(left: Double, right: Double): Double = left + right

    override def multiply(left: Double, right: Double): Double = left * right

    override def divide(left: Double, right: Double): Double = left / right

    override def pow(left: Double, right: Double): Double = Math.pow(left, right)

    override def negate(left: Double): Double = -left

    override def log(left: Double, right: Double): Double = (Math.log(left) / Math.log(right)).toInt

    override def root(left: Double, right: Double): Double = Math.pow(left, 1.0 / right)

    def schema: Schema[Double] = implicitly[Schema[Double]]
  }

  implicit case object FractionalBigDecimal extends Fractional[BigDecimal] {
    def fromDouble(const: Double): BigDecimal                         = BigDecimal(const)
    override def fromLong(l: Long): Remote[BigDecimal]                = Remote(BigDecimal(l))
    override def add(left: BigDecimal, right: BigDecimal): BigDecimal = left + right

    override def multiply(left: BigDecimal, right: BigDecimal): BigDecimal = left * right

    override def divide(left: BigDecimal, right: BigDecimal): BigDecimal = left / right

    override def pow(left: BigDecimal, right: BigDecimal): BigDecimal = Math.pow(left.doubleValue, right.doubleValue)

    override def negate(left: BigDecimal): BigDecimal = -left

    override def log(left: BigDecimal, right: BigDecimal): BigDecimal =
      Math.log(left.doubleValue) / Math.log(right.doubleValue)

    override def root(left: BigDecimal, right: BigDecimal): BigDecimal =
      Math.pow(left.doubleValue, 1.0 / right.doubleValue)

    def schema: Schema[BigDecimal] = implicitly[Schema[BigDecimal]]
  }
}

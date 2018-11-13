package testgen

import scala.collection.mutable

class Emit {
  val sb = new java.lang.StringBuilder
  def apply(value: String): Unit = {
    sb.append(value)
    sb.append('\n')
  }
  override def toString = sb.toString
}

object TestGen {
  type Type = String
  type Value = Any
  type Op = String
  type UnaryOverload = (Seq[Type], (Any) => Any)
  type BinaryOverload = (Seq[Type], (Any, Any) => Any)
  type Variant = String

  val boolean = "Boolean"
  val byte = "Byte"
  val short = "Short"
  val char = "Char"
  val int = "Int"
  val long = "Long"
  val float = "Float"
  val double = "Double"

  val samples = Map[Type, Seq[Value]](
    boolean -> Seq(
      true,
      false
    ),
    char -> Seq(
      java.lang.Character.MIN_VALUE,
      java.lang.Character.MAX_VALUE,
      '0',
      ' ',
      '1',
      'ĳ',
      'a',
      'ᓹ',
      '妍'
    ),
    byte -> Seq(
      java.lang.Byte.MIN_VALUE,
      java.lang.Byte.MAX_VALUE,
      0.toByte,
      1.toByte,
      81.toByte,
      101.toByte,
      120.toByte,
      -1.toByte,
      -99.toByte,
      -114.toByte,
      -124.toByte
    ),
    short -> Seq(
      java.lang.Short.MIN_VALUE,
      java.lang.Short.MAX_VALUE,
      0.toShort,
      1.toShort,
      84.toShort,
      7410.toShort,
      16132.toShort,
      -1.toShort,
      -73.toShort,
      -1162.toShort,
      -18090.toShort
    ),
    int -> Seq(
      java.lang.Integer.MIN_VALUE,
      java.lang.Integer.MAX_VALUE,
      0,
      1,
      702460,
      275434658,
      1270029521,
      -1,
      -749990,
      -186748006,
      -1588499300
    ),
    long -> Seq(
      java.lang.Long.MIN_VALUE,
      java.lang.Long.MAX_VALUE,
      0L,
      1L,
      1412906027847L,
      70424924662051552L,
      2626308222543888459L,
      -1L,
      -8799231824L,
      -4746701162271676L,
      -2201690882079163160L
    ),
    float -> Seq(
      java.lang.Float.MIN_VALUE,
      java.lang.Float.MAX_VALUE,
      java.lang.Float.NaN,
      java.lang.Float.NEGATIVE_INFINITY,
      java.lang.Float.POSITIVE_INFINITY,
      0F,
      1F,
      0.50380343F,
      3.3229107F,
      718.87374F,
      67186.71F,
      -1F,
      -0.9160936F,
      -8.0150115F,
      -216.69602F,
      -38777.608F
    ),
    double -> Seq(
      java.lang.Double.MIN_VALUE,
      java.lang.Double.MAX_VALUE,
      java.lang.Double.NaN,
      java.lang.Double.NEGATIVE_INFINITY,
      java.lang.Double.POSITIVE_INFINITY,
      0D,
      1D,
      0.5936403795646567D,
      986.759726442125D,
      514475.6134971429D,
      -1D,
      -0.3222548099938489D,
      -3721.6239394538553D,
      -824639695.7818043D
    )
  )

  // val convOps = {
  //   val overloads =
  //     Seq(byte, short, char, int, long, double).foreach {
  //       Seq(byte, short, char, int, long, double).foreach {
  //       }
  //     }
  // }

  val opNames = Map[String, String](
    ("unary_!", "Not"),
    ("unary_~", "BitwiseNot") ,
    ("unary_-", "Neg"),
    ("unary_+", "UnaryAdd"),
    ("_+", "Unmodified"),
    ("*", "Multiply"),
    ("<=", "LessThanOrEqual"),
    ("%", "Modulo"),
    ("<", "LessThan"),
    ("&", "BitwiseAnd"),
    ("<<", "ShiftLeft"),
    ("||", "Or"),
    (">=", "GreaterThanOrEqual"),
    ("|", "BitwiseOr"),
    (">>", "ArithmeticShiftRight"),
    ("-", "Substract"),
    (">>>", "LogicalShiftRight"),
    ("==", "Equals"),
    ("+", "Add"),
    ("&&", "And"),
    ("^", "BitwiseXor"),
    ("/", "Divide"),
    (">", "Greater"),
    ("!=", "NotEquals")
  )

  val unaryOps = Map[Op, Seq[UnaryOverload]](
    "unary_!" -> Seq(
      (Seq(boolean, boolean), ((x: Boolean) => x.unary_!).asInstanceOf[Any => Any])
    ),
    "unary_~" -> Seq(
      (Seq(byte, int), ((x: Byte) => x.unary_~).asInstanceOf[Any => Any]),
      (Seq(short, int), ((x: Short) => x.unary_~).asInstanceOf[Any => Any]),
      (Seq(char, int), ((x: Char) => x.unary_~).asInstanceOf[Any => Any]),
      (Seq(int, int), ((x: Int) => x.unary_~).asInstanceOf[Any => Any]),
      (Seq(long, long), ((x: Long) => x.unary_~).asInstanceOf[Any => Any])
    ),
    "unary_+" -> Seq(
      (Seq(byte, int), ((x: Byte) => x.unary_+).asInstanceOf[Any => Any]),
      (Seq(short, int), ((x: Short) => x.unary_+).asInstanceOf[Any => Any]),
      (Seq(char, int), ((x: Char) => x.unary_+).asInstanceOf[Any => Any]),
      (Seq(int, int), ((x: Int) => x.unary_+).asInstanceOf[Any => Any]),
      (Seq(float, float), ((x: Float) => x.unary_+).asInstanceOf[Any => Any]),
      (Seq(double, double), ((x: Double) => x.unary_+).asInstanceOf[Any => Any])
    ),
    "unary_-" -> Seq(
      (Seq(byte, int), ((x: Byte) => x.unary_-).asInstanceOf[Any => Any]),
      (Seq(short, int), ((x: Short) => x.unary_-).asInstanceOf[Any => Any]),
      (Seq(char, int), ((x: Char) => x.unary_-).asInstanceOf[Any => Any]),
      (Seq(int, int), ((x: Int) => x.unary_-).asInstanceOf[Any => Any]),
      (Seq(float, float), ((x: Float) => x.unary_-).asInstanceOf[Any => Any]),
      (Seq(double, double), ((x: Double) => x.unary_-).asInstanceOf[Any => Any])
    )
  )

  val binaryOps = Map[Op, Seq[BinaryOverload]](
    "*" -> Seq(
      (Seq(byte, byte, int), ((x: Byte, y: Byte) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, short, int), ((x: Byte, y: Short) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, char, int), ((x: Byte, y: Char) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, int, int), ((x: Byte, y: Int) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, long, long), ((x: Byte, y: Long) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, float, float), ((x: Byte, y: Float) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, double, double), ((x: Byte, y: Double) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, byte, int), ((x: Short, y: Byte) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, short, int), ((x: Short, y: Short) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, char, int), ((x: Short, y: Char) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, int, int), ((x: Short, y: Int) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, long, long), ((x: Short, y: Long) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, float, float), ((x: Short, y: Float) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, double, double), ((x: Short, y: Double) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, byte, int), ((x: Char, y: Byte) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, short, int), ((x: Char, y: Short) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, char, int), ((x: Char, y: Char) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, int, int), ((x: Char, y: Int) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, long, long), ((x: Char, y: Long) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, float, float), ((x: Char, y: Float) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, double, double), ((x: Char, y: Double) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, byte, int), ((x: Int, y: Byte) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, short, int), ((x: Int, y: Short) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, char, int), ((x: Int, y: Char) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, int, int), ((x: Int, y: Int) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, long, long), ((x: Int, y: Long) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, float, float), ((x: Int, y: Float) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, double, double), ((x: Int, y: Double) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, byte, long), ((x: Long, y: Byte) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, short, long), ((x: Long, y: Short) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, char, long), ((x: Long, y: Char) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, int, long), ((x: Long, y: Int) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, long, long), ((x: Long, y: Long) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, float, float), ((x: Long, y: Float) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, double, double), ((x: Long, y: Double) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, byte, float), ((x: Float, y: Byte) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, short, float), ((x: Float, y: Short) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, char, float), ((x: Float, y: Char) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, int, float), ((x: Float, y: Int) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, long, float), ((x: Float, y: Long) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, float, float), ((x: Float, y: Float) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, double, double), ((x: Float, y: Double) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, byte, double), ((x: Double, y: Byte) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, short, double), ((x: Double, y: Short) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, char, double), ((x: Double, y: Char) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, int, double), ((x: Double, y: Int) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, long, double), ((x: Double, y: Long) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, float, double), ((x: Double, y: Float) => x * y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, double, double), ((x: Double, y: Double) => x * y).asInstanceOf[(Any, Any) => Any])
    ),
    "<=" -> Seq(
      (Seq(byte, byte, boolean), ((x: Byte, y: Byte) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, short, boolean), ((x: Byte, y: Short) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, char, boolean), ((x: Byte, y: Char) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, int, boolean), ((x: Byte, y: Int) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, long, boolean), ((x: Byte, y: Long) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, float, boolean), ((x: Byte, y: Float) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, double, boolean), ((x: Byte, y: Double) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, byte, boolean), ((x: Short, y: Byte) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, short, boolean), ((x: Short, y: Short) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, char, boolean), ((x: Short, y: Char) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, int, boolean), ((x: Short, y: Int) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, long, boolean), ((x: Short, y: Long) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, float, boolean), ((x: Short, y: Float) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, double, boolean), ((x: Short, y: Double) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, byte, boolean), ((x: Char, y: Byte) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, short, boolean), ((x: Char, y: Short) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, char, boolean), ((x: Char, y: Char) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, int, boolean), ((x: Char, y: Int) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, long, boolean), ((x: Char, y: Long) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, float, boolean), ((x: Char, y: Float) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, double, boolean), ((x: Char, y: Double) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, byte, boolean), ((x: Int, y: Byte) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, short, boolean), ((x: Int, y: Short) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, char, boolean), ((x: Int, y: Char) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, int, boolean), ((x: Int, y: Int) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, long, boolean), ((x: Int, y: Long) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, float, boolean), ((x: Int, y: Float) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, double, boolean), ((x: Int, y: Double) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, byte, boolean), ((x: Long, y: Byte) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, short, boolean), ((x: Long, y: Short) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, char, boolean), ((x: Long, y: Char) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, int, boolean), ((x: Long, y: Int) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, long, boolean), ((x: Long, y: Long) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, float, boolean), ((x: Long, y: Float) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, double, boolean), ((x: Long, y: Double) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, byte, boolean), ((x: Float, y: Byte) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, short, boolean), ((x: Float, y: Short) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, char, boolean), ((x: Float, y: Char) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, int, boolean), ((x: Float, y: Int) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, long, boolean), ((x: Float, y: Long) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, float, boolean), ((x: Float, y: Float) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, double, boolean), ((x: Float, y: Double) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, byte, boolean), ((x: Double, y: Byte) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, short, boolean), ((x: Double, y: Short) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, char, boolean), ((x: Double, y: Char) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, int, boolean), ((x: Double, y: Int) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, long, boolean), ((x: Double, y: Long) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, float, boolean), ((x: Double, y: Float) => x <= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, double, boolean), ((x: Double, y: Double) => x <= y).asInstanceOf[(Any, Any) => Any])
    ),
    "%" -> Seq(
      (Seq(byte, byte, int), ((x: Byte, y: Byte) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, short, int), ((x: Byte, y: Short) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, char, int), ((x: Byte, y: Char) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, int, int), ((x: Byte, y: Int) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, long, long), ((x: Byte, y: Long) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, float, float), ((x: Byte, y: Float) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, double, double), ((x: Byte, y: Double) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, byte, int), ((x: Short, y: Byte) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, short, int), ((x: Short, y: Short) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, char, int), ((x: Short, y: Char) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, int, int), ((x: Short, y: Int) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, long, long), ((x: Short, y: Long) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, float, float), ((x: Short, y: Float) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, double, double), ((x: Short, y: Double) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, byte, int), ((x: Char, y: Byte) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, short, int), ((x: Char, y: Short) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, char, int), ((x: Char, y: Char) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, int, int), ((x: Char, y: Int) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, long, long), ((x: Char, y: Long) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, float, float), ((x: Char, y: Float) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, double, double), ((x: Char, y: Double) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, byte, int), ((x: Int, y: Byte) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, short, int), ((x: Int, y: Short) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, char, int), ((x: Int, y: Char) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, int, int), ((x: Int, y: Int) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, long, long), ((x: Int, y: Long) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, float, float), ((x: Int, y: Float) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, double, double), ((x: Int, y: Double) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, byte, long), ((x: Long, y: Byte) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, short, long), ((x: Long, y: Short) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, char, long), ((x: Long, y: Char) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, int, long), ((x: Long, y: Int) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, long, long), ((x: Long, y: Long) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, float, float), ((x: Long, y: Float) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, double, double), ((x: Long, y: Double) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, byte, float), ((x: Float, y: Byte) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, short, float), ((x: Float, y: Short) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, char, float), ((x: Float, y: Char) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, int, float), ((x: Float, y: Int) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, long, float), ((x: Float, y: Long) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, float, float), ((x: Float, y: Float) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, double, double), ((x: Float, y: Double) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, byte, double), ((x: Double, y: Byte) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, short, double), ((x: Double, y: Short) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, char, double), ((x: Double, y: Char) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, int, double), ((x: Double, y: Int) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, long, double), ((x: Double, y: Long) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, float, double), ((x: Double, y: Float) => x % y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, double, double), ((x: Double, y: Double) => x % y).asInstanceOf[(Any, Any) => Any])
    ),
    "<" -> Seq(
      (Seq(byte, byte, boolean), ((x: Byte, y: Byte) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, short, boolean), ((x: Byte, y: Short) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, char, boolean), ((x: Byte, y: Char) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, int, boolean), ((x: Byte, y: Int) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, long, boolean), ((x: Byte, y: Long) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, float, boolean), ((x: Byte, y: Float) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, double, boolean), ((x: Byte, y: Double) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, byte, boolean), ((x: Short, y: Byte) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, short, boolean), ((x: Short, y: Short) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, char, boolean), ((x: Short, y: Char) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, int, boolean), ((x: Short, y: Int) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, long, boolean), ((x: Short, y: Long) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, float, boolean), ((x: Short, y: Float) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, double, boolean), ((x: Short, y: Double) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, byte, boolean), ((x: Char, y: Byte) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, short, boolean), ((x: Char, y: Short) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, char, boolean), ((x: Char, y: Char) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, int, boolean), ((x: Char, y: Int) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, long, boolean), ((x: Char, y: Long) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, float, boolean), ((x: Char, y: Float) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, double, boolean), ((x: Char, y: Double) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, byte, boolean), ((x: Int, y: Byte) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, short, boolean), ((x: Int, y: Short) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, char, boolean), ((x: Int, y: Char) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, int, boolean), ((x: Int, y: Int) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, long, boolean), ((x: Int, y: Long) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, float, boolean), ((x: Int, y: Float) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, double, boolean), ((x: Int, y: Double) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, byte, boolean), ((x: Long, y: Byte) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, short, boolean), ((x: Long, y: Short) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, char, boolean), ((x: Long, y: Char) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, int, boolean), ((x: Long, y: Int) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, long, boolean), ((x: Long, y: Long) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, float, boolean), ((x: Long, y: Float) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, double, boolean), ((x: Long, y: Double) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, byte, boolean), ((x: Float, y: Byte) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, short, boolean), ((x: Float, y: Short) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, char, boolean), ((x: Float, y: Char) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, int, boolean), ((x: Float, y: Int) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, long, boolean), ((x: Float, y: Long) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, float, boolean), ((x: Float, y: Float) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, double, boolean), ((x: Float, y: Double) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, byte, boolean), ((x: Double, y: Byte) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, short, boolean), ((x: Double, y: Short) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, char, boolean), ((x: Double, y: Char) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, int, boolean), ((x: Double, y: Int) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, long, boolean), ((x: Double, y: Long) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, float, boolean), ((x: Double, y: Float) => x < y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, double, boolean), ((x: Double, y: Double) => x < y).asInstanceOf[(Any, Any) => Any])
    ),
    "&" -> Seq(
      (Seq(boolean, boolean, boolean), ((x: Boolean, y: Boolean) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, byte, int), ((x: Byte, y: Byte) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, short, int), ((x: Byte, y: Short) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, char, int), ((x: Byte, y: Char) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, int, int), ((x: Byte, y: Int) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, long, long), ((x: Byte, y: Long) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, byte, int), ((x: Short, y: Byte) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, short, int), ((x: Short, y: Short) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, char, int), ((x: Short, y: Char) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, int, int), ((x: Short, y: Int) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, long, long), ((x: Short, y: Long) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, byte, int), ((x: Char, y: Byte) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, short, int), ((x: Char, y: Short) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, char, int), ((x: Char, y: Char) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, int, int), ((x: Char, y: Int) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, long, long), ((x: Char, y: Long) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, byte, int), ((x: Int, y: Byte) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, short, int), ((x: Int, y: Short) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, char, int), ((x: Int, y: Char) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, int, int), ((x: Int, y: Int) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, long, long), ((x: Int, y: Long) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, byte, long), ((x: Long, y: Byte) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, short, long), ((x: Long, y: Short) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, char, long), ((x: Long, y: Char) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, int, long), ((x: Long, y: Int) => x & y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, long, long), ((x: Long, y: Long) => x & y).asInstanceOf[(Any, Any) => Any])
    ),
    "<<" -> Seq(
      (Seq(byte, int, int), ((x: Byte, y: Int) => x << y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, long, int), ((x: Byte, y: Long) => x << y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, int, int), ((x: Short, y: Int) => x << y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, long, int), ((x: Short, y: Long) => x << y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, int, int), ((x: Char, y: Int) => x << y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, long, int), ((x: Char, y: Long) => x << y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, int, int), ((x: Int, y: Int) => x << y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, long, int), ((x: Int, y: Long) => x << y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, int, long), ((x: Long, y: Int) => x << y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, long, long), ((x: Long, y: Long) => x << y).asInstanceOf[(Any, Any) => Any])
    ),
    "||" -> Seq(
      (Seq(boolean, boolean, boolean), ((x: Boolean, y: Boolean) => x || y).asInstanceOf[(Any, Any) => Any])
    ),
    ">=" -> Seq(
      (Seq(byte, byte, boolean), ((x: Byte, y: Byte) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, short, boolean), ((x: Byte, y: Short) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, char, boolean), ((x: Byte, y: Char) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, int, boolean), ((x: Byte, y: Int) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, long, boolean), ((x: Byte, y: Long) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, float, boolean), ((x: Byte, y: Float) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, double, boolean), ((x: Byte, y: Double) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, byte, boolean), ((x: Short, y: Byte) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, short, boolean), ((x: Short, y: Short) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, char, boolean), ((x: Short, y: Char) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, int, boolean), ((x: Short, y: Int) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, long, boolean), ((x: Short, y: Long) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, float, boolean), ((x: Short, y: Float) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, double, boolean), ((x: Short, y: Double) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, byte, boolean), ((x: Char, y: Byte) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, short, boolean), ((x: Char, y: Short) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, char, boolean), ((x: Char, y: Char) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, int, boolean), ((x: Char, y: Int) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, long, boolean), ((x: Char, y: Long) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, float, boolean), ((x: Char, y: Float) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, double, boolean), ((x: Char, y: Double) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, byte, boolean), ((x: Int, y: Byte) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, short, boolean), ((x: Int, y: Short) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, char, boolean), ((x: Int, y: Char) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, int, boolean), ((x: Int, y: Int) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, long, boolean), ((x: Int, y: Long) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, float, boolean), ((x: Int, y: Float) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, double, boolean), ((x: Int, y: Double) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, byte, boolean), ((x: Long, y: Byte) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, short, boolean), ((x: Long, y: Short) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, char, boolean), ((x: Long, y: Char) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, int, boolean), ((x: Long, y: Int) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, long, boolean), ((x: Long, y: Long) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, float, boolean), ((x: Long, y: Float) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, double, boolean), ((x: Long, y: Double) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, byte, boolean), ((x: Float, y: Byte) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, short, boolean), ((x: Float, y: Short) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, char, boolean), ((x: Float, y: Char) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, int, boolean), ((x: Float, y: Int) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, long, boolean), ((x: Float, y: Long) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, float, boolean), ((x: Float, y: Float) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, double, boolean), ((x: Float, y: Double) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, byte, boolean), ((x: Double, y: Byte) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, short, boolean), ((x: Double, y: Short) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, char, boolean), ((x: Double, y: Char) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, int, boolean), ((x: Double, y: Int) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, long, boolean), ((x: Double, y: Long) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, float, boolean), ((x: Double, y: Float) => x >= y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, double, boolean), ((x: Double, y: Double) => x >= y).asInstanceOf[(Any, Any) => Any])
    ),
    "|" -> Seq(
      (Seq(boolean, boolean, boolean), ((x: Boolean, y: Boolean) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, byte, int), ((x: Byte, y: Byte) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, short, int), ((x: Byte, y: Short) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, char, int), ((x: Byte, y: Char) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, int, int), ((x: Byte, y: Int) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, long, long), ((x: Byte, y: Long) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, byte, int), ((x: Short, y: Byte) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, short, int), ((x: Short, y: Short) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, char, int), ((x: Short, y: Char) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, int, int), ((x: Short, y: Int) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, long, long), ((x: Short, y: Long) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, byte, int), ((x: Char, y: Byte) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, short, int), ((x: Char, y: Short) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, char, int), ((x: Char, y: Char) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, int, int), ((x: Char, y: Int) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, long, long), ((x: Char, y: Long) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, byte, int), ((x: Int, y: Byte) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, short, int), ((x: Int, y: Short) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, char, int), ((x: Int, y: Char) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, int, int), ((x: Int, y: Int) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, long, long), ((x: Int, y: Long) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, byte, long), ((x: Long, y: Byte) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, short, long), ((x: Long, y: Short) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, char, long), ((x: Long, y: Char) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, int, long), ((x: Long, y: Int) => x | y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, long, long), ((x: Long, y: Long) => x | y).asInstanceOf[(Any, Any) => Any])
    ),
    ">>" -> Seq(
      (Seq(byte, int, int), ((x: Byte, y: Int) => x >> y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, long, int), ((x: Byte, y: Long) => x >> y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, int, int), ((x: Short, y: Int) => x >> y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, long, int), ((x: Short, y: Long) => x >> y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, int, int), ((x: Char, y: Int) => x >> y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, long, int), ((x: Char, y: Long) => x >> y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, int, int), ((x: Int, y: Int) => x >> y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, long, int), ((x: Int, y: Long) => x >> y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, int, long), ((x: Long, y: Int) => x >> y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, long, long), ((x: Long, y: Long) => x >> y).asInstanceOf[(Any, Any) => Any])
    ),
    "-" -> Seq(
      (Seq(byte, byte, int), ((x: Byte, y: Byte) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, short, int), ((x: Byte, y: Short) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, char, int), ((x: Byte, y: Char) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, int, int), ((x: Byte, y: Int) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, long, long), ((x: Byte, y: Long) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, float, float), ((x: Byte, y: Float) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, double, double), ((x: Byte, y: Double) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, byte, int), ((x: Short, y: Byte) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, short, int), ((x: Short, y: Short) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, char, int), ((x: Short, y: Char) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, int, int), ((x: Short, y: Int) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, long, long), ((x: Short, y: Long) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, float, float), ((x: Short, y: Float) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, double, double), ((x: Short, y: Double) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, byte, int), ((x: Char, y: Byte) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, short, int), ((x: Char, y: Short) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, char, int), ((x: Char, y: Char) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, int, int), ((x: Char, y: Int) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, long, long), ((x: Char, y: Long) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, float, float), ((x: Char, y: Float) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, double, double), ((x: Char, y: Double) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, byte, int), ((x: Int, y: Byte) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, short, int), ((x: Int, y: Short) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, char, int), ((x: Int, y: Char) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, int, int), ((x: Int, y: Int) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, long, long), ((x: Int, y: Long) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, float, float), ((x: Int, y: Float) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, double, double), ((x: Int, y: Double) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, byte, long), ((x: Long, y: Byte) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, short, long), ((x: Long, y: Short) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, char, long), ((x: Long, y: Char) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, int, long), ((x: Long, y: Int) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, long, long), ((x: Long, y: Long) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, float, float), ((x: Long, y: Float) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, double, double), ((x: Long, y: Double) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, byte, float), ((x: Float, y: Byte) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, short, float), ((x: Float, y: Short) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, char, float), ((x: Float, y: Char) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, int, float), ((x: Float, y: Int) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, long, float), ((x: Float, y: Long) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, float, float), ((x: Float, y: Float) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, double, double), ((x: Float, y: Double) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, byte, double), ((x: Double, y: Byte) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, short, double), ((x: Double, y: Short) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, char, double), ((x: Double, y: Char) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, int, double), ((x: Double, y: Int) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, long, double), ((x: Double, y: Long) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, float, double), ((x: Double, y: Float) => x - y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, double, double), ((x: Double, y: Double) => x - y).asInstanceOf[(Any, Any) => Any])
    ),
    ">>>" -> Seq(
      (Seq(byte, int, int), ((x: Byte, y: Int) => x >>> y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, long, int), ((x: Byte, y: Long) => x >>> y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, int, int), ((x: Short, y: Int) => x >>> y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, long, int), ((x: Short, y: Long) => x >>> y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, int, int), ((x: Char, y: Int) => x >>> y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, long, int), ((x: Char, y: Long) => x >>> y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, int, int), ((x: Int, y: Int) => x >>> y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, long, int), ((x: Int, y: Long) => x >>> y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, int, long), ((x: Long, y: Int) => x >>> y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, long, long), ((x: Long, y: Long) => x >>> y).asInstanceOf[(Any, Any) => Any])
    ),
    "==" -> Seq(
      (Seq(boolean, boolean, boolean), ((x: Boolean, y: Boolean) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, byte, boolean), ((x: Byte, y: Byte) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, short, boolean), ((x: Byte, y: Short) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, char, boolean), ((x: Byte, y: Char) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, int, boolean), ((x: Byte, y: Int) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, long, boolean), ((x: Byte, y: Long) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, float, boolean), ((x: Byte, y: Float) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, double, boolean), ((x: Byte, y: Double) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, byte, boolean), ((x: Short, y: Byte) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, short, boolean), ((x: Short, y: Short) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, char, boolean), ((x: Short, y: Char) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, int, boolean), ((x: Short, y: Int) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, long, boolean), ((x: Short, y: Long) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, float, boolean), ((x: Short, y: Float) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, double, boolean), ((x: Short, y: Double) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, byte, boolean), ((x: Char, y: Byte) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, short, boolean), ((x: Char, y: Short) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, char, boolean), ((x: Char, y: Char) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, int, boolean), ((x: Char, y: Int) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, long, boolean), ((x: Char, y: Long) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, float, boolean), ((x: Char, y: Float) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, double, boolean), ((x: Char, y: Double) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, byte, boolean), ((x: Int, y: Byte) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, short, boolean), ((x: Int, y: Short) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, char, boolean), ((x: Int, y: Char) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, int, boolean), ((x: Int, y: Int) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, long, boolean), ((x: Int, y: Long) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, float, boolean), ((x: Int, y: Float) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, double, boolean), ((x: Int, y: Double) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, byte, boolean), ((x: Long, y: Byte) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, short, boolean), ((x: Long, y: Short) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, char, boolean), ((x: Long, y: Char) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, int, boolean), ((x: Long, y: Int) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, long, boolean), ((x: Long, y: Long) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, float, boolean), ((x: Long, y: Float) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, double, boolean), ((x: Long, y: Double) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, byte, boolean), ((x: Float, y: Byte) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, short, boolean), ((x: Float, y: Short) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, char, boolean), ((x: Float, y: Char) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, int, boolean), ((x: Float, y: Int) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, long, boolean), ((x: Float, y: Long) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, float, boolean), ((x: Float, y: Float) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, double, boolean), ((x: Float, y: Double) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, byte, boolean), ((x: Double, y: Byte) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, short, boolean), ((x: Double, y: Short) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, char, boolean), ((x: Double, y: Char) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, int, boolean), ((x: Double, y: Int) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, long, boolean), ((x: Double, y: Long) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, float, boolean), ((x: Double, y: Float) => x == y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, double, boolean), ((x: Double, y: Double) => x == y).asInstanceOf[(Any, Any) => Any])
    ),
    "+" -> Seq(
      (Seq(byte, byte, int), ((x: Byte, y: Byte) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, short, int), ((x: Byte, y: Short) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, char, int), ((x: Byte, y: Char) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, int, int), ((x: Byte, y: Int) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, long, long), ((x: Byte, y: Long) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, float, float), ((x: Byte, y: Float) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, double, double), ((x: Byte, y: Double) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, byte, int), ((x: Short, y: Byte) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, short, int), ((x: Short, y: Short) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, char, int), ((x: Short, y: Char) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, int, int), ((x: Short, y: Int) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, long, long), ((x: Short, y: Long) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, float, float), ((x: Short, y: Float) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, double, double), ((x: Short, y: Double) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, byte, int), ((x: Char, y: Byte) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, short, int), ((x: Char, y: Short) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, char, int), ((x: Char, y: Char) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, int, int), ((x: Char, y: Int) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, long, long), ((x: Char, y: Long) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, float, float), ((x: Char, y: Float) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, double, double), ((x: Char, y: Double) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, byte, int), ((x: Int, y: Byte) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, short, int), ((x: Int, y: Short) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, char, int), ((x: Int, y: Char) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, int, int), ((x: Int, y: Int) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, long, long), ((x: Int, y: Long) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, float, float), ((x: Int, y: Float) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, double, double), ((x: Int, y: Double) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, byte, long), ((x: Long, y: Byte) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, short, long), ((x: Long, y: Short) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, char, long), ((x: Long, y: Char) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, int, long), ((x: Long, y: Int) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, long, long), ((x: Long, y: Long) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, float, float), ((x: Long, y: Float) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, double, double), ((x: Long, y: Double) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, byte, float), ((x: Float, y: Byte) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, short, float), ((x: Float, y: Short) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, char, float), ((x: Float, y: Char) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, int, float), ((x: Float, y: Int) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, long, float), ((x: Float, y: Long) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, float, float), ((x: Float, y: Float) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, double, double), ((x: Float, y: Double) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, byte, double), ((x: Double, y: Byte) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, short, double), ((x: Double, y: Short) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, char, double), ((x: Double, y: Char) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, int, double), ((x: Double, y: Int) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, long, double), ((x: Double, y: Long) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, float, double), ((x: Double, y: Float) => x + y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, double, double), ((x: Double, y: Double) => x + y).asInstanceOf[(Any, Any) => Any])
    ),
    "!=" -> Seq(
      (Seq(boolean, boolean, boolean), ((x: Boolean, y: Boolean) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, byte, boolean), ((x: Byte, y: Byte) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, short, boolean), ((x: Byte, y: Short) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, char, boolean), ((x: Byte, y: Char) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, int, boolean), ((x: Byte, y: Int) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, long, boolean), ((x: Byte, y: Long) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, float, boolean), ((x: Byte, y: Float) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, double, boolean), ((x: Byte, y: Double) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, byte, boolean), ((x: Short, y: Byte) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, short, boolean), ((x: Short, y: Short) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, char, boolean), ((x: Short, y: Char) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, int, boolean), ((x: Short, y: Int) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, long, boolean), ((x: Short, y: Long) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, float, boolean), ((x: Short, y: Float) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, double, boolean), ((x: Short, y: Double) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, byte, boolean), ((x: Char, y: Byte) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, short, boolean), ((x: Char, y: Short) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, char, boolean), ((x: Char, y: Char) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, int, boolean), ((x: Char, y: Int) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, long, boolean), ((x: Char, y: Long) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, float, boolean), ((x: Char, y: Float) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, double, boolean), ((x: Char, y: Double) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, byte, boolean), ((x: Int, y: Byte) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, short, boolean), ((x: Int, y: Short) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, char, boolean), ((x: Int, y: Char) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, int, boolean), ((x: Int, y: Int) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, long, boolean), ((x: Int, y: Long) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, float, boolean), ((x: Int, y: Float) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, double, boolean), ((x: Int, y: Double) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, byte, boolean), ((x: Long, y: Byte) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, short, boolean), ((x: Long, y: Short) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, char, boolean), ((x: Long, y: Char) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, int, boolean), ((x: Long, y: Int) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, long, boolean), ((x: Long, y: Long) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, float, boolean), ((x: Long, y: Float) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, double, boolean), ((x: Long, y: Double) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, byte, boolean), ((x: Float, y: Byte) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, short, boolean), ((x: Float, y: Short) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, char, boolean), ((x: Float, y: Char) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, int, boolean), ((x: Float, y: Int) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, long, boolean), ((x: Float, y: Long) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, float, boolean), ((x: Float, y: Float) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, double, boolean), ((x: Float, y: Double) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, byte, boolean), ((x: Double, y: Byte) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, short, boolean), ((x: Double, y: Short) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, char, boolean), ((x: Double, y: Char) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, int, boolean), ((x: Double, y: Int) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, long, boolean), ((x: Double, y: Long) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, float, boolean), ((x: Double, y: Float) => x != y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, double, boolean), ((x: Double, y: Double) => x != y).asInstanceOf[(Any, Any) => Any])
    ),
    "&&" -> Seq(
      (Seq(boolean, boolean, boolean), ((x: Boolean, y: Boolean) => x && y).asInstanceOf[(Any, Any) => Any])
    ),
    "^" -> Seq(
      (Seq(boolean, boolean, boolean), ((x: Boolean, y: Boolean) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, byte, int), ((x: Byte, y: Byte) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, short, int), ((x: Byte, y: Short) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, char, int), ((x: Byte, y: Char) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, int, int), ((x: Byte, y: Int) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, long, long), ((x: Byte, y: Long) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, byte, int), ((x: Short, y: Byte) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, short, int), ((x: Short, y: Short) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, char, int), ((x: Short, y: Char) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, int, int), ((x: Short, y: Int) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, long, long), ((x: Short, y: Long) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, byte, int), ((x: Char, y: Byte) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, short, int), ((x: Char, y: Short) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, char, int), ((x: Char, y: Char) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, int, int), ((x: Char, y: Int) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, long, long), ((x: Char, y: Long) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, byte, int), ((x: Int, y: Byte) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, short, int), ((x: Int, y: Short) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, char, int), ((x: Int, y: Char) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, int, int), ((x: Int, y: Int) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, long, long), ((x: Int, y: Long) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, byte, long), ((x: Long, y: Byte) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, short, long), ((x: Long, y: Short) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, char, long), ((x: Long, y: Char) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, int, long), ((x: Long, y: Int) => x ^ y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, long, long), ((x: Long, y: Long) => x ^ y).asInstanceOf[(Any, Any) => Any])
    ),
    "/" -> Seq(
      (Seq(byte, byte, int), ((x: Byte, y: Byte) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, short, int), ((x: Byte, y: Short) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, char, int), ((x: Byte, y: Char) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, int, int), ((x: Byte, y: Int) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, long, long), ((x: Byte, y: Long) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, float, float), ((x: Byte, y: Float) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, double, double), ((x: Byte, y: Double) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, byte, int), ((x: Short, y: Byte) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, short, int), ((x: Short, y: Short) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, char, int), ((x: Short, y: Char) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, int, int), ((x: Short, y: Int) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, long, long), ((x: Short, y: Long) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, float, float), ((x: Short, y: Float) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, double, double), ((x: Short, y: Double) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, byte, int), ((x: Char, y: Byte) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, short, int), ((x: Char, y: Short) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, char, int), ((x: Char, y: Char) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, int, int), ((x: Char, y: Int) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, long, long), ((x: Char, y: Long) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, float, float), ((x: Char, y: Float) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, double, double), ((x: Char, y: Double) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, byte, int), ((x: Int, y: Byte) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, short, int), ((x: Int, y: Short) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, char, int), ((x: Int, y: Char) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, int, int), ((x: Int, y: Int) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, long, long), ((x: Int, y: Long) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, float, float), ((x: Int, y: Float) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, double, double), ((x: Int, y: Double) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, byte, long), ((x: Long, y: Byte) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, short, long), ((x: Long, y: Short) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, char, long), ((x: Long, y: Char) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, int, long), ((x: Long, y: Int) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, long, long), ((x: Long, y: Long) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, float, float), ((x: Long, y: Float) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, double, double), ((x: Long, y: Double) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, byte, float), ((x: Float, y: Byte) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, short, float), ((x: Float, y: Short) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, char, float), ((x: Float, y: Char) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, int, float), ((x: Float, y: Int) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, long, float), ((x: Float, y: Long) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, float, float), ((x: Float, y: Float) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, double, double), ((x: Float, y: Double) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, byte, double), ((x: Double, y: Byte) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, short, double), ((x: Double, y: Short) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, char, double), ((x: Double, y: Char) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, int, double), ((x: Double, y: Int) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, long, double), ((x: Double, y: Long) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, float, double), ((x: Double, y: Float) => x / y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, double, double), ((x: Double, y: Double) => x / y).asInstanceOf[(Any, Any) => Any])
    ),
    ">" -> Seq(
      (Seq(byte, byte, boolean), ((x: Byte, y: Byte) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, short, boolean), ((x: Byte, y: Short) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, char, boolean), ((x: Byte, y: Char) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, int, boolean), ((x: Byte, y: Int) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, long, boolean), ((x: Byte, y: Long) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, float, boolean), ((x: Byte, y: Float) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(byte, double, boolean), ((x: Byte, y: Double) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, byte, boolean), ((x: Short, y: Byte) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, short, boolean), ((x: Short, y: Short) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, char, boolean), ((x: Short, y: Char) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, int, boolean), ((x: Short, y: Int) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, long, boolean), ((x: Short, y: Long) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, float, boolean), ((x: Short, y: Float) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(short, double, boolean), ((x: Short, y: Double) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, byte, boolean), ((x: Char, y: Byte) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, short, boolean), ((x: Char, y: Short) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, char, boolean), ((x: Char, y: Char) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, int, boolean), ((x: Char, y: Int) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, long, boolean), ((x: Char, y: Long) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, float, boolean), ((x: Char, y: Float) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(char, double, boolean), ((x: Char, y: Double) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, byte, boolean), ((x: Int, y: Byte) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, short, boolean), ((x: Int, y: Short) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, char, boolean), ((x: Int, y: Char) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, int, boolean), ((x: Int, y: Int) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, long, boolean), ((x: Int, y: Long) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, float, boolean), ((x: Int, y: Float) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(int, double, boolean), ((x: Int, y: Double) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, byte, boolean), ((x: Long, y: Byte) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, short, boolean), ((x: Long, y: Short) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, char, boolean), ((x: Long, y: Char) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, int, boolean), ((x: Long, y: Int) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, long, boolean), ((x: Long, y: Long) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, float, boolean), ((x: Long, y: Float) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(long, double, boolean), ((x: Long, y: Double) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, byte, boolean), ((x: Float, y: Byte) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, short, boolean), ((x: Float, y: Short) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, char, boolean), ((x: Float, y: Char) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, int, boolean), ((x: Float, y: Int) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, long, boolean), ((x: Float, y: Long) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, float, boolean), ((x: Float, y: Float) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(float, double, boolean), ((x: Float, y: Double) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, byte, boolean), ((x: Double, y: Byte) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, short, boolean), ((x: Double, y: Short) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, char, boolean), ((x: Double, y: Char) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, int, boolean), ((x: Double, y: Int) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, long, boolean), ((x: Double, y: Long) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, float, boolean), ((x: Double, y: Float) => x > y).asInstanceOf[(Any, Any) => Any]),
      (Seq(double, double, boolean), ((x: Double, y: Double) => x > y).asInstanceOf[(Any, Any) => Any])
    )
  )

  val allOps = (unaryOps.keys.toSeq ++ binaryOps.keys.toSeq).sorted

  val opIds = allOps.zipWithIndex.toMap

  def genUtil(): String = """
    |trait Util {
    |  var succs  = 0
    |  var checks = 0
    |  def check(succ: Boolean): Unit = {
    |    checks += 1
    |    if (succ) {
    |      succs += 1
    |    }
    |  }
    |  def throws[T](cond: => T): Unit = {
    |    checks += 1
    |    val succ =
    |      try {
    |        cond
    |        false
    |      } catch {
    |        case _: ArithmeticException =>
    |          true
    |      }
    |    if (succ) {
    |      succs += 1
    |    }
    |  }
    |}""".stripMargin

  def genSamples(): String = {
    val emit = new Emit

    emit("object Samples {")

    samples.foreach {
      case (ty, values) =>
        values.zipWithIndex.foreach {
          case (value, i) =>
            emit(s"  @noinline def noinline$ty$i : $ty = ${genValue(ty, value)}")
            emit(s"  @inline def inline$ty$i : $ty = ${genValue(ty, value)}")
        }
    }

    emit("}")
    emit.toString
  }

  def genValue(ty: Type, value: Any): String = ty match {
    case "Boolean" => value.toString
    case "Byte" => s"$value.toByte"
    case "Short" => s"$value.toShort"
    case "Char" => s"${value.asInstanceOf[Char].toInt}.toChar"
    case "Int"  => value.toString
    case "Long" => s"${value}L"
    case "Float" =>
      val v = value.asInstanceOf[Float]
      if (java.lang.Float.isNaN(v)) {
        s"java.lang.Float.NaN"
      } else {
        s"java.lang.Float.intBitsToFloat(${java.lang.Float.floatToRawIntBits(v)})"
      }
    case "Double" =>
      val v = value.asInstanceOf[Double]
      if (java.lang.Double.isNaN(v)) {
        s"java.lang.Double.NaN"
      } else {
        s"java.lang.Double.longBitsToDouble(${java.lang.Double.doubleToRawLongBits(v)}L)"
      }
  }

  def genCheck(expr: String, ty: Type, f: () => Any): String = {
    try {
      f() match {
        case result: Float if java.lang.Float.isNaN(result) =>
          s"check(java.lang.Float.isNaN($expr))"
        case result: Double if java.lang.Double.isNaN(result) =>
          s"check(java.lang.Double.isNaN($expr))"
        case result =>
          s"check(($expr) == ${genValue(ty, result)})"
      }
    } catch {
      case _: ArithmeticException =>
        s"throws($expr)"
    }
  }

  def genUnaryOpOverload(emit: Emit, op: Op, overloadId: Int, argty: Type, retty: Type, f: Any => Any): Seq[String] = {
    def genTest(lsample: Any, i: Int, prefix: String): String = {
      val name = s"test_${opIds(op)}_${overloadId}_${i}_$prefix"
      val check = genCheck(s"$prefix$argty$i.$op", retty, () => f(lsample))
      emit(s"  def $name = $check")
      name
    }

    samples(argty).zipWithIndex.flatMap {
      case (lsample, i) =>
        Seq(genTest(lsample, i, "inline"),
            genTest(lsample, i, "noinline"))
    }
  }

  def genBinaryOpOverload(emit: Emit, op: String, overloadId: Int, arg1ty: Type, arg2ty: Type, retty: Type, f: (Any, Any) => Any): Seq[String] = {
    def genTest(lsample: Any, i: Int, rsample: Any, j: Int, prefix: String): String = {
      val name = s"test_${opIds(op)}_${overloadId}_${i}_${j}_$prefix"
      val check = genCheck(s"$prefix$arg1ty$i $op $prefix$arg2ty$j", retty, () => f(lsample, rsample))
      emit(s"  def $name = $check")
      name
    }

    val tests = mutable.UnrolledBuffer.empty[String]

    samples(arg1ty).zipWithIndex.flatMap {
      case (lsample, i) =>
        samples(arg2ty).zipWithIndex.flatMap {
          case (rsample, j) =>
            tests += genTest(lsample, i, rsample, j, "inline")
            tests += genTest(lsample, i, rsample, j, "noinline")
        }
    }

    if (tests.nonEmpty) {
      val name = s"test_${opIds(op)}_${overloadId}"
      emit(s"  def $name = {")
      tests.foreach { test =>
        emit("    " + test)
      }
      emit("  }")
      Seq(name)
    } else {
      Seq()
    }

  }

  def genOpOverloads(emit: Emit, ownerty: Type, op: Op): Seq[String] = {
    println(s"genOpOverloads($ownerty, $op)")
    val tests = mutable.UnrolledBuffer.empty[String]

    if (unaryOps.contains(op)) {
      unaryOps(op).zipWithIndex.foreach {
        case ((Seq(argty, retty), f), overloadId) if argty == ownerty =>
          println(s"genOpOverload($ownerty, $op) @ ($argty, $retty)")
          tests ++= genUnaryOpOverload(emit, op, overloadId, argty, retty, f)
        case _ =>
          ()
      }
    } else if (binaryOps.contains(op)) {
      binaryOps(op).zipWithIndex.foreach {
        case ((Seq(arg1ty, arg2ty, retty), f), overloadId) if arg1ty == ownerty =>
          println(s"genOpOverload($ownerty, $op) @ ($arg1ty, $arg2ty, $retty))")
          tests ++= genBinaryOpOverload(emit, op, overloadId, arg1ty, arg2ty, retty, f)
        case _ =>
          ()
      }
    }

    if (tests.nonEmpty) {
      val name = s"test_${opIds(op)}"
      emit(s"  def $name = {")
      tests.foreach { test =>
        emit("    " + test)
      }
      emit("  }")
      Seq(name)
    } else {
      Seq()
    }
  }

  def genTypeOp(ownerty: Type, op: Op): (String, String) = {
    println(s"genTypeOp($ownerty, $op)")

    val name = s"Test${ownerty}${opNames(op)}"

    val emit = new Emit

    emit("import Samples._")
    emit(s"object $name extends Util {")

    val tests = genOpOverloads(emit, ownerty, op)

    emit("  def run : Unit = {")
    tests.foreach { test =>
      emit("    " + test)
    }
    emit(s"""    println(s"Score for $name: $$succs out of $$checks (ok? $${succs == checks})")""")
    emit("  }")
    emit("}")

    (name + ".scala", emit.toString)
  }

  val typeOps: Seq[(Type, Op)] = {
    val unaryTypeOps = unaryOps.toSeq.flatMap {
      case (op, overloads) =>
        overloads.map {
          case (overload, f) =>
            (overload.head, op)
        }
    }.distinct
    val binaryTypeOps = binaryOps.toSeq.flatMap {
      case (op, overloads) =>
        overloads.map {
          case (overload, f) =>
            (overload.head, op)
        }
    }.distinct
    unaryTypeOps ++ binaryTypeOps
  }

  def genMain(): String = {
    val emit = new Emit

    emit("object Main extends App {")

    typeOps.foreach {
      case (ownerty, op) =>
        emit(s"  Test${ownerty}${opNames(op)}.run")
    }

    emit("}")
    emit.toString
  }

  def gen(): Seq[(String, String)] = {
    val out = mutable.UnrolledBuffer.empty[(String, String)]

    out += (("Util.scala", genUtil()))
    out += (("Samples.scala", genSamples()))

    typeOps.foreach {
      case (ownerty, op) =>
        val (name, contents) = genTypeOp(ownerty, op)
        out += ((name, contents))
    }

    out += (("Main.scala", genMain()))

    out
  }
}

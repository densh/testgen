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
  type Value = String
  type Op = String
  type Overload = Seq[Type]
  type Variant = String

  val bool = "Boolean"
  val byte = "Byte"
  val short = "Short"
  val char = "Char"
  val int = "Int"
  val long = "Long"
  val float = "Float"
  val double = "Double"

  val samples = Map[Type, Seq[Value]](
    bool -> Seq(
      "true",
      "false"
    ),
    char -> Seq(
      "java.lang.Character.MIN_VALUE",
      "java.lang.Character.MAX_VALUE",
      "'\\0'",
      "'0'",
      "' '",
      "'1'",
      "'ĳ'",
      "'a'",
      "'ᓹ'",
      "'妍'"
    ),
    byte -> Seq(
      "java.lang.Byte.MIN_VALUE",
      "java.lang.Byte.MAX_VALUE",
      "0",
      "1",
      "81",
      "101",
      "120",
      "-1",
      "-99",
      "-114",
      "-124"
    ),
    short -> Seq(
      "java.lang.Short.MIN_VALUE",
      "java.lang.Short.MAX_VALUE",
      "0",
      "1",
      "84",
      "7410",
      "16132",
      "-1",
      "-73",
      "-1162",
      "-18090"
    ),
    int -> Seq(
      "java.lang.Integer.MIN_VALUE",
      "java.lang.Integer.MAX_VALUE",
      "0",
      "1",
      "702460",
      "275434658",
      "1270029521",
      "-1",
      "-749990",
      "-186748006",
      "-1588499300"
    ),
    long -> Seq(
      "java.lang.Long.MIN_VALUE",
      "java.lang.Long.MAX_VALUE",
      "0L",
      "1L",
      "1412906027847L",
      "70424924662051552L",
      "2626308222543888459L",
      "-1L",
      "-8799231824L",
      "-4746701162271676L",
      "-2201690882079163160L"
    ),
    float -> Seq(
      "java.lang.Float.MIN_VALUE",
      "java.lang.Float.MAX_VALUE",
      "0F",
      "1F",
      "0.50380343F",
      "3.3229107F",
      "718.87374F",
      "67186.71F",
      "-1F",
      "-0.9160936F",
      "-8.0150115F",
      "-216.69602F",
      "-38777.608F"
    ),
    double -> Seq(
      "java.lang.Double.MIN_VALUE",
      "java.lang.Double.MAX_VALUE",
      "0D",
      "1D",
      "0.5936403795646567D",
      "986.759726442125D",
      "514475.6134971429D",
      "-1D",
      "-0.3222548099938489D",
      "-3721.6239394538553D",
      "-824639695.7818043D"
    )
  )

  val zeros = Set(
    "java.lang.Character.MIN_VALUE",
    "'\\0'",
    "0",
    "0L",
    "0F",
    "0D"
  )

  def isDiv(op: Op): Boolean =
    op == "%" || op == "/"

  def isZero(value: Value): Boolean =
    zeros.contains(value)

  val ops = Map[Op, Seq[Overload]](
    "unary_!" -> Seq(
      Seq(bool, bool)
    ),
    "unary_~" -> Seq(
      Seq(byte, int),
      Seq(short, int),
      Seq(char, int),
      Seq(int, int)
    ),
    "unary_+" -> Seq(
      Seq(byte, int),
      Seq(short, int),
      Seq(char, int),
      Seq(int, int),
      Seq(float, float),
      Seq(double, double)
    ),
    "unary_-" -> Seq(
      Seq(byte, int),
      Seq(short, int),
      Seq(char, int),
      Seq(int, int),
      Seq(float, float),
      Seq(double, double)
    ),
    "==" -> Seq(
      Seq(bool, bool, bool),
      Seq(byte, byte, bool),
      Seq(byte, short, bool),
      Seq(byte, char, bool),
      Seq(byte, int, bool),
      Seq(byte, long, bool),
      Seq(byte, float, bool),
      Seq(byte, double, bool),
      Seq(short, byte, bool),
      Seq(short, short, bool),
      Seq(short, char, bool),
      Seq(short, int, bool),
      Seq(short, long, bool),
      Seq(short, float, bool),
      Seq(short, double, bool),
      Seq(char, byte, bool),
      Seq(char, short, bool),
      Seq(char, char, bool),
      Seq(char, int, bool),
      Seq(char, long, bool),
      Seq(char, float, bool),
      Seq(char, double, bool),
      Seq(int, byte, bool),
      Seq(int, short, bool),
      Seq(int, char, bool),
      Seq(int, int, bool),
      Seq(int, long, bool),
      Seq(int, float, bool),
      Seq(int, double, bool),
      Seq(long, byte, bool),
      Seq(long, short, bool),
      Seq(long, char, bool),
      Seq(long, int, bool),
      Seq(long, long, bool),
      Seq(long, float, bool),
      Seq(long, double, bool),
      Seq(float, byte, bool),
      Seq(float, short, bool),
      Seq(float, char, bool),
      Seq(float, int, bool),
      Seq(float, long, bool),
      Seq(float, float, bool),
      Seq(float, double, bool),
      Seq(double, byte, bool),
      Seq(double, short, bool),
      Seq(double, char, bool),
      Seq(double, int, bool),
      Seq(double, long, bool),
      Seq(double, float, bool),
      Seq(double, double, bool)
    ),
    "!=" -> Seq(
      Seq(bool, bool, bool),
      Seq(byte, byte, bool),
      Seq(byte, short, bool),
      Seq(byte, char, bool),
      Seq(byte, int, bool),
      Seq(byte, long, bool),
      Seq(byte, float, bool),
      Seq(byte, double, bool),
      Seq(short, byte, bool),
      Seq(short, short, bool),
      Seq(short, char, bool),
      Seq(short, int, bool),
      Seq(short, long, bool),
      Seq(short, float, bool),
      Seq(short, double, bool),
      Seq(char, byte, bool),
      Seq(char, short, bool),
      Seq(char, char, bool),
      Seq(char, int, bool),
      Seq(char, long, bool),
      Seq(char, float, bool),
      Seq(char, double, bool),
      Seq(int, byte, bool),
      Seq(int, short, bool),
      Seq(int, char, bool),
      Seq(int, int, bool),
      Seq(int, long, bool),
      Seq(int, float, bool),
      Seq(int, double, bool),
      Seq(long, byte, bool),
      Seq(long, short, bool),
      Seq(long, char, bool),
      Seq(long, int, bool),
      Seq(long, long, bool),
      Seq(long, float, bool),
      Seq(long, double, bool),
      Seq(float, byte, bool),
      Seq(float, short, bool),
      Seq(float, char, bool),
      Seq(float, int, bool),
      Seq(float, long, bool),
      Seq(float, float, bool),
      Seq(float, double, bool),
      Seq(double, byte, bool),
      Seq(double, short, bool),
      Seq(double, char, bool),
      Seq(double, int, bool),
      Seq(double, long, bool),
      Seq(double, float, bool),
      Seq(double, double, bool)
    ),
    "<" -> Seq(
      Seq(byte, byte, bool),
      Seq(byte, short, bool),
      Seq(byte, char, bool),
      Seq(byte, int, bool),
      Seq(byte, long, bool),
      Seq(byte, float, bool),
      Seq(byte, double, bool),
      Seq(short, byte, bool),
      Seq(short, short, bool),
      Seq(short, char, bool),
      Seq(short, int, bool),
      Seq(short, long, bool),
      Seq(short, float, bool),
      Seq(short, double, bool),
      Seq(char, byte, bool),
      Seq(char, short, bool),
      Seq(char, char, bool),
      Seq(char, int, bool),
      Seq(char, long, bool),
      Seq(char, float, bool),
      Seq(char, double, bool),
      Seq(int, byte, bool),
      Seq(int, short, bool),
      Seq(int, char, bool),
      Seq(int, int, bool),
      Seq(int, long, bool),
      Seq(int, float, bool),
      Seq(int, double, bool),
      Seq(long, byte, bool),
      Seq(long, short, bool),
      Seq(long, char, bool),
      Seq(long, int, bool),
      Seq(long, long, bool),
      Seq(long, float, bool),
      Seq(long, double, bool),
      Seq(float, byte, bool),
      Seq(float, short, bool),
      Seq(float, char, bool),
      Seq(float, int, bool),
      Seq(float, long, bool),
      Seq(float, float, bool),
      Seq(float, double, bool),
      Seq(double, byte, bool),
      Seq(double, short, bool),
      Seq(double, char, bool),
      Seq(double, int, bool),
      Seq(double, long, bool),
      Seq(double, float, bool),
      Seq(double, double, bool)
    ),
    "<=" -> Seq(
      Seq(byte, byte, bool),
      Seq(byte, short, bool),
      Seq(byte, char, bool),
      Seq(byte, int, bool),
      Seq(byte, long, bool),
      Seq(byte, float, bool),
      Seq(byte, double, bool),
      Seq(short, byte, bool),
      Seq(short, short, bool),
      Seq(short, char, bool),
      Seq(short, int, bool),
      Seq(short, long, bool),
      Seq(short, float, bool),
      Seq(short, double, bool),
      Seq(char, byte, bool),
      Seq(char, short, bool),
      Seq(char, char, bool),
      Seq(char, int, bool),
      Seq(char, long, bool),
      Seq(char, float, bool),
      Seq(char, double, bool),
      Seq(int, byte, bool),
      Seq(int, short, bool),
      Seq(int, char, bool),
      Seq(int, int, bool),
      Seq(int, long, bool),
      Seq(int, float, bool),
      Seq(int, double, bool),
      Seq(long, byte, bool),
      Seq(long, short, bool),
      Seq(long, char, bool),
      Seq(long, int, bool),
      Seq(long, long, bool),
      Seq(long, float, bool),
      Seq(long, double, bool),
      Seq(float, byte, bool),
      Seq(float, short, bool),
      Seq(float, char, bool),
      Seq(float, int, bool),
      Seq(float, long, bool),
      Seq(float, float, bool),
      Seq(float, double, bool),
      Seq(double, byte, bool),
      Seq(double, short, bool),
      Seq(double, char, bool),
      Seq(double, int, bool),
      Seq(double, long, bool),
      Seq(double, float, bool),
      Seq(double, double, bool)
    ),
    ">" -> Seq(
      Seq(byte, byte, bool),
      Seq(byte, short, bool),
      Seq(byte, char, bool),
      Seq(byte, int, bool),
      Seq(byte, long, bool),
      Seq(byte, float, bool),
      Seq(byte, double, bool),
      Seq(short, byte, bool),
      Seq(short, short, bool),
      Seq(short, char, bool),
      Seq(short, int, bool),
      Seq(short, long, bool),
      Seq(short, float, bool),
      Seq(short, double, bool),
      Seq(char, byte, bool),
      Seq(char, short, bool),
      Seq(char, char, bool),
      Seq(char, int, bool),
      Seq(char, long, bool),
      Seq(char, float, bool),
      Seq(char, double, bool),
      Seq(int, byte, bool),
      Seq(int, short, bool),
      Seq(int, char, bool),
      Seq(int, int, bool),
      Seq(int, long, bool),
      Seq(int, float, bool),
      Seq(int, double, bool),
      Seq(long, byte, bool),
      Seq(long, short, bool),
      Seq(long, char, bool),
      Seq(long, int, bool),
      Seq(long, long, bool),
      Seq(long, float, bool),
      Seq(long, double, bool),
      Seq(float, byte, bool),
      Seq(float, short, bool),
      Seq(float, char, bool),
      Seq(float, int, bool),
      Seq(float, long, bool),
      Seq(float, float, bool),
      Seq(float, double, bool),
      Seq(double, byte, bool),
      Seq(double, short, bool),
      Seq(double, char, bool),
      Seq(double, int, bool),
      Seq(double, long, bool),
      Seq(double, float, bool),
      Seq(double, double, bool)
    ),
    ">=" -> Seq(
      Seq(byte, byte, bool),
      Seq(byte, short, bool),
      Seq(byte, char, bool),
      Seq(byte, int, bool),
      Seq(byte, long, bool),
      Seq(byte, float, bool),
      Seq(byte, double, bool),
      Seq(short, byte, bool),
      Seq(short, short, bool),
      Seq(short, char, bool),
      Seq(short, int, bool),
      Seq(short, long, bool),
      Seq(short, float, bool),
      Seq(short, double, bool),
      Seq(char, byte, bool),
      Seq(char, short, bool),
      Seq(char, char, bool),
      Seq(char, int, bool),
      Seq(char, long, bool),
      Seq(char, float, bool),
      Seq(char, double, bool),
      Seq(int, byte, bool),
      Seq(int, short, bool),
      Seq(int, char, bool),
      Seq(int, int, bool),
      Seq(int, long, bool),
      Seq(int, float, bool),
      Seq(int, double, bool),
      Seq(long, byte, bool),
      Seq(long, short, bool),
      Seq(long, char, bool),
      Seq(long, int, bool),
      Seq(long, long, bool),
      Seq(long, float, bool),
      Seq(long, double, bool),
      Seq(float, byte, bool),
      Seq(float, short, bool),
      Seq(float, char, bool),
      Seq(float, int, bool),
      Seq(float, long, bool),
      Seq(float, float, bool),
      Seq(float, double, bool),
      Seq(double, byte, bool),
      Seq(double, short, bool),
      Seq(double, char, bool),
      Seq(double, int, bool),
      Seq(double, long, bool),
      Seq(double, float, bool),
      Seq(double, double, bool)
    ),
    "||" -> Seq(
      Seq(bool, bool, bool)
    ),
    "&&" -> Seq(
      Seq(bool, bool, bool)
    ),
    "|" -> Seq(
      Seq(bool, bool, bool),
      Seq(byte, byte, int),
      Seq(byte, short, int),
      Seq(byte, char, int),
      Seq(byte, int, int),
      Seq(byte, long, long),
      Seq(short, byte, int),
      Seq(short, short, int),
      Seq(short, char, int),
      Seq(short, int, int),
      Seq(short, long, long),
      Seq(char, byte, int),
      Seq(char, short, int),
      Seq(char, char, int),
      Seq(char, int, int),
      Seq(char, long, long),
      Seq(int, byte, int),
      Seq(int, short, int),
      Seq(int, char, int),
      Seq(int, int, int),
      Seq(int, long, long),
      Seq(long, byte, long),
      Seq(long, short, long),
      Seq(long, char, long),
      Seq(long, int, long),
      Seq(long, long, long)
    ),
    "&" -> Seq(
      Seq(bool, bool, bool),
      Seq(byte, byte, int),
      Seq(byte, short, int),
      Seq(byte, char, int),
      Seq(byte, int, int),
      Seq(byte, long, long),
      Seq(short, byte, int),
      Seq(short, short, int),
      Seq(short, char, int),
      Seq(short, int, int),
      Seq(short, long, long),
      Seq(char, byte, int),
      Seq(char, short, int),
      Seq(char, char, int),
      Seq(char, int, int),
      Seq(char, long, long),
      Seq(int, byte, int),
      Seq(int, short, int),
      Seq(int, char, int),
      Seq(int, int, int),
      Seq(int, long, long),
      Seq(long, byte, long),
      Seq(long, short, long),
      Seq(long, char, long),
      Seq(long, int, long),
      Seq(long, long, long)
    ),
    "^" -> Seq(
      Seq(bool, bool, bool),
      Seq(byte, byte, int),
      Seq(byte, short, int),
      Seq(byte, char, int),
      Seq(byte, int, int),
      Seq(byte, long, long),
      Seq(short, byte, int),
      Seq(short, short, int),
      Seq(short, char, int),
      Seq(short, int, int),
      Seq(short, long, long),
      Seq(char, byte, int),
      Seq(char, short, int),
      Seq(char, char, int),
      Seq(char, int, int),
      Seq(char, long, long),
      Seq(int, byte, int),
      Seq(int, short, int),
      Seq(int, char, int),
      Seq(int, int, int),
      Seq(int, long, long),
      Seq(long, byte, long),
      Seq(long, short, long),
      Seq(long, char, long),
      Seq(long, int, long),
      Seq(long, long, long)
    ),
    "<<" -> Seq(
      Seq(byte, int, int),
      Seq(byte, long, int),
      Seq(short, int, int),
      Seq(short, long, int),
      Seq(char, int, int),
      Seq(char, long, int),
      Seq(int, int, int),
      Seq(int, long, int),
      Seq(long, int, long),
      Seq(long, long, long)
    ),
    ">>>" -> Seq(
      Seq(byte, int, int),
      Seq(byte, long, int),
      Seq(short, int, int),
      Seq(short, long, int),
      Seq(char, int, int),
      Seq(char, long, int),
      Seq(int, int, int),
      Seq(int, long, int),
      Seq(long, int, long),
      Seq(long, long, long)
    ),
    ">>" -> Seq(
      Seq(byte, int, int),
      Seq(byte, long, int),
      Seq(short, int, int),
      Seq(short, long, int),
      Seq(char, int, int),
      Seq(char, long, int),
      Seq(int, int, int),
      Seq(int, long, int),
      Seq(long, int, long),
      Seq(long, long, long)
    ),
    "+" -> Seq(
      Seq(byte, byte, int),
      Seq(byte, short, int),
      Seq(byte, char, int),
      Seq(byte, int, int),
      Seq(byte, long, long),
      Seq(byte, float, float),
      Seq(byte, double, double),
      Seq(short, byte, int),
      Seq(short, short, int),
      Seq(short, char, int),
      Seq(short, int, int),
      Seq(short, long, long),
      Seq(short, float, float),
      Seq(short, double, double),
      Seq(char, byte, int),
      Seq(char, short, int),
      Seq(char, char, int),
      Seq(char, int, int),
      Seq(char, long, long),
      Seq(char, float, float),
      Seq(char, double, double),
      Seq(int, byte, int),
      Seq(int, short, int),
      Seq(int, char, int),
      Seq(int, int, int),
      Seq(int, long, long),
      Seq(int, float, float),
      Seq(int, double, double),
      Seq(long, byte, long),
      Seq(long, short, long),
      Seq(long, char, long),
      Seq(long, int, long),
      Seq(long, long, long),
      Seq(long, float, float),
      Seq(long, double, double),
      Seq(float, byte, float),
      Seq(float, short, float),
      Seq(float, char, float),
      Seq(float, int, float),
      Seq(float, long, float),
      Seq(float, float, float),
      Seq(float, double, double),
      Seq(double, byte, double),
      Seq(double, short, double),
      Seq(double, char, double),
      Seq(double, int, double),
      Seq(double, long, double),
      Seq(double, float, double),
      Seq(double, double, double)
    ),
    "-" -> Seq(
      Seq(byte, byte, int),
      Seq(byte, short, int),
      Seq(byte, char, int),
      Seq(byte, int, int),
      Seq(byte, long, long),
      Seq(byte, float, float),
      Seq(byte, double, double),
      Seq(short, byte, int),
      Seq(short, short, int),
      Seq(short, char, int),
      Seq(short, int, int),
      Seq(short, long, long),
      Seq(short, float, float),
      Seq(short, double, double),
      Seq(char, byte, int),
      Seq(char, short, int),
      Seq(char, char, int),
      Seq(char, int, int),
      Seq(char, long, long),
      Seq(char, float, float),
      Seq(char, double, double),
      Seq(int, byte, int),
      Seq(int, short, int),
      Seq(int, char, int),
      Seq(int, int, int),
      Seq(int, long, long),
      Seq(int, float, float),
      Seq(int, double, double),
      Seq(long, byte, long),
      Seq(long, short, long),
      Seq(long, char, long),
      Seq(long, int, long),
      Seq(long, long, long),
      Seq(long, float, float),
      Seq(long, double, double),
      Seq(float, byte, float),
      Seq(float, short, float),
      Seq(float, char, float),
      Seq(float, int, float),
      Seq(float, long, float),
      Seq(float, float, float),
      Seq(float, double, double),
      Seq(double, byte, double),
      Seq(double, short, double),
      Seq(double, char, double),
      Seq(double, int, double),
      Seq(double, long, double),
      Seq(double, float, double),
      Seq(double, double, double)
    ),
    "*" -> Seq(
      Seq(byte, byte, int),
      Seq(byte, short, int),
      Seq(byte, char, int),
      Seq(byte, int, int),
      Seq(byte, long, long),
      Seq(byte, float, float),
      Seq(byte, double, double),
      Seq(short, byte, int),
      Seq(short, short, int),
      Seq(short, char, int),
      Seq(short, int, int),
      Seq(short, long, long),
      Seq(short, float, float),
      Seq(short, double, double),
      Seq(char, byte, int),
      Seq(char, short, int),
      Seq(char, char, int),
      Seq(char, int, int),
      Seq(char, long, long),
      Seq(char, float, float),
      Seq(char, double, double),
      Seq(int, byte, int),
      Seq(int, short, int),
      Seq(int, char, int),
      Seq(int, int, int),
      Seq(int, long, long),
      Seq(int, float, float),
      Seq(int, double, double),
      Seq(long, byte, long),
      Seq(long, short, long),
      Seq(long, char, long),
      Seq(long, int, long),
      Seq(long, long, long),
      Seq(long, float, float),
      Seq(long, double, double),
      Seq(float, byte, float),
      Seq(float, short, float),
      Seq(float, char, float),
      Seq(float, int, float),
      Seq(float, long, float),
      Seq(float, float, float),
      Seq(float, double, double),
      Seq(double, byte, double),
      Seq(double, short, double),
      Seq(double, char, double),
      Seq(double, int, double),
      Seq(double, long, double),
      Seq(double, float, double),
      Seq(double, double, double)
    ),
    "/" -> Seq(
      Seq(byte, byte, int),
      Seq(byte, short, int),
      Seq(byte, char, int),
      Seq(byte, int, int),
      Seq(byte, long, long),
      Seq(byte, float, float),
      Seq(byte, double, double),
      Seq(short, byte, int),
      Seq(short, short, int),
      Seq(short, char, int),
      Seq(short, int, int),
      Seq(short, long, long),
      Seq(short, float, float),
      Seq(short, double, double),
      Seq(char, byte, int),
      Seq(char, short, int),
      Seq(char, char, int),
      Seq(char, int, int),
      Seq(char, long, long),
      Seq(char, float, float),
      Seq(char, double, double),
      Seq(int, byte, int),
      Seq(int, short, int),
      Seq(int, char, int),
      Seq(int, int, int),
      Seq(int, long, long),
      Seq(int, float, float),
      Seq(int, double, double),
      Seq(long, byte, long),
      Seq(long, short, long),
      Seq(long, char, long),
      Seq(long, int, long),
      Seq(long, long, long),
      Seq(long, float, float),
      Seq(long, double, double),
      Seq(float, byte, float),
      Seq(float, short, float),
      Seq(float, char, float),
      Seq(float, int, float),
      Seq(float, long, float),
      Seq(float, float, float),
      Seq(float, double, double),
      Seq(double, byte, double),
      Seq(double, short, double),
      Seq(double, char, double),
      Seq(double, int, double),
      Seq(double, long, double),
      Seq(double, float, double),
      Seq(double, double, double)
    ),
    "%" -> Seq(
      Seq(byte, byte, int),
      Seq(byte, short, int),
      Seq(byte, char, int),
      Seq(byte, int, int),
      Seq(byte, long, long),
      Seq(byte, float, float),
      Seq(byte, double, double),
      Seq(short, byte, int),
      Seq(short, short, int),
      Seq(short, char, int),
      Seq(short, int, int),
      Seq(short, long, long),
      Seq(short, float, float),
      Seq(short, double, double),
      Seq(char, byte, int),
      Seq(char, short, int),
      Seq(char, char, int),
      Seq(char, int, int),
      Seq(char, long, long),
      Seq(char, float, float),
      Seq(char, double, double),
      Seq(int, byte, int),
      Seq(int, short, int),
      Seq(int, char, int),
      Seq(int, int, int),
      Seq(int, long, long),
      Seq(int, float, float),
      Seq(int, double, double),
      Seq(long, byte, long),
      Seq(long, short, long),
      Seq(long, char, long),
      Seq(long, int, long),
      Seq(long, long, long),
      Seq(long, float, float),
      Seq(long, double, double),
      Seq(float, byte, float),
      Seq(float, short, float),
      Seq(float, char, float),
      Seq(float, int, float),
      Seq(float, long, float),
      Seq(float, float, float),
      Seq(float, double, double),
      Seq(double, byte, double),
      Seq(double, short, double),
      Seq(double, char, double),
      Seq(double, int, double),
      Seq(double, long, double),
      Seq(double, float, double),
      Seq(double, double, double)
    )
  )

  def genUtil(): String = """
    |trait Util {
    |  var succs  = 0
    |  var checks = 0
    |  def check(cond: => Boolean): Unit = {
    |    checks += 1
    |    val succ =
    |      try {
    |        cond
    |      } catch {
    |        case _: ArithmeticException =>
    |          false
    |      }
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
    |  def test(s: String)(f: => Unit): Unit = f
    |}""".stripMargin

  def genSamples(): String = {
    val emit = new Emit

    emit("object Samples {")

    samples.foreach {
      case (ty, values) =>
        values.zipWithIndex.foreach {
          case (value, i) =>
            emit(s"  final val val$ty$i = $value")
            emit(s"  @noinline def noinline$ty$i : $ty = $value")
            emit(s"  @inline def inline$ty$i : $ty = $value")
        }
    }

    emit("}")
    emit.toString
  }

  def genUnaryOpOverload(emit: Emit, op: Op, n: Int, argty: Type, retty: Type): Seq[String] = {
    def genTest(i: Int, variant1: Variant, variant2: Variant) = {
      val name = s"test_${n}_${i}_${variant1}_${variant2}"
      emit(s"  def $name : Unit = {")
      emit(s"    val _$variant1 = $variant1$argty$i.$op")
      emit(s"    val _$variant2 = $variant2$argty$i.$op")
      emit(s"    check(_$variant1 == _$variant2)")
      emit("  }")
      name
    }

    samples(argty).zipWithIndex.flatMap {
      case (_, i) =>
        Seq(genTest(i, "val", "inline"),
            genTest(i, "val", "noinline"))
    }
  }

  def genBinaryOpOverload(emit: Emit, op: Op, n: Int, arg1ty: Type, arg2ty: Type, retty: Type): Seq[String] = {
    def genCheckTest(i: Int, j: Int, variant1: Variant, variant2: Variant) = {
      val name = s"test_${n}_${i}_${j}_${variant1}_${variant2}"
      emit(s"  def $name : Unit = {")
      emit(s"    val _$variant1 = $variant1$arg1ty$i $op $variant1$arg2ty$j")
      emit(s"    val _$variant2 = $variant2$arg1ty$i $op $variant2$arg2ty$j")
      emit(s"    check(_$variant1 == _$variant2)")
      emit("  }")
      name
    }

    def genThrowsTest(i: Int, j: Int, variant: Variant) = {
      val name = s"test_${n}_${i}_${j}_${variant}"
      emit(s"  def $name : Unit = {")
      emit(s"    throws($variant$arg1ty$i $op $variant$arg2ty$j)")
      emit("  }")
      name
    }

    samples(arg1ty).zipWithIndex.flatMap {
      case (_, i) =>
        samples(arg2ty).zipWithIndex.flatMap {
          case (rsample, j) =>
            if (isDiv(op) && isZero(rsample)) {
              Seq(genThrowsTest(i, j, "val"),
                  genThrowsTest(i, j, "inline"),
                  genThrowsTest(i, j, "noinline"))
            } else {
              Seq(genCheckTest(i, j, "val", "inline"),
                  genCheckTest(i, j, "val", "noinline"))
            }
        }
    }
  }

  def genOpOverload(emit: Emit, op: Op, n: Int, overload: Overload): Seq[String] = overload match {
    case Seq(argty, retty) =>
      genUnaryOpOverload(emit, op, n, argty, retty)
    case Seq(arg1ty, arg2ty, retty) =>
      genBinaryOpOverload(emit, op, n, arg1ty, arg2ty, retty)
  }

  def genTypeOp(ownerty: Type, op: Op): (String, String) = {
    val name = s"Test_${ownerty}_$op"

    val emit = new Emit
    val tests = mutable.UnrolledBuffer.empty[String]

    emit("import Samples._")
    emit(s"object $name extends Util {")

    ops(op).zipWithIndex.foreach {
      case (overload, n) =>
        if (overload.head == ownerty) {
          tests ++= genOpOverload(emit, op, n, overload)
        }
    }

    ops(op).zipWithIndex.foreach {
      case (_, n) =>
        emit(s"  def test_$n : Unit = {")
        tests.foreach { test =>
          if (test.startsWith(s"test_${n}_")) {
            emit(s"    $test")
          }
        }
        emit("  }")
    }

    emit("  def run : Unit = {")
    ops(op).zipWithIndex.foreach {
      case (_, n) =>
        emit(s"    test_$n")
    }
    emit(s"""    println(s"Score for $name: $$succs out of $$checks")""")
    emit("  }")
    emit("}")

    (name + ".scala", emit.toString)
  }

  def genMain(): String = {
    val emit = new Emit

    emit("object Main extends App {")
    typeOps.foreach {
      case (ownerty, op) =>
        emit(s"  Test_${ownerty}_$op.run")
    }
    emit("}")
    emit.toString
  }

  val typeOps: Seq[(Type, Op)] = ops.toSeq.flatMap {
    case (op, overloads) =>
      overloads.map { overload =>
        (overload.head, op)
      }
  }.distinct

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

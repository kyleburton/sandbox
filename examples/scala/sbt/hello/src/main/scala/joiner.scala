/*

  object StringUtil {
  def joiner( strings: List[String], sep: String = ", " ) : String =
    strings.mkString(sep)
}



import StringUtil._

println(joiner(List("Foo","Bar","Qux")))


def factorial(i: Int): Int = {
  var acc : Int = 1
  def fact(i:Int) : Int = {
    if (i<=1)
      acc
    else {
      acc = acc * i
      fact(i-1)
    }
  }
  fact(i)
  acc
}

println(factorial(0))
println(factorial(1))
println(factorial(2))
println(factorial(3))
println(factorial(4))
println(factorial(5))
println(factorial(100))

*/

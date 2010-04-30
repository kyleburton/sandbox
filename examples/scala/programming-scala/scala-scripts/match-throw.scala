// NB: you can't do this in the REPL
import java.util.Calendar

val then = null
val now = Calendar.getInstance()

try {
 now.compareTo(then)
}
catch {
  case e: NullPointerException =>
    println("someting was null")
  case unknown =>
    println("Ok, something else happened: " + unknown)
}
finally {
  println("in the finally")
}


object App2 {
  def _main (args: Array[String]) {
    printf("world yo\n")

    val movies =
    <movies>
    |     <movie genre="action">Pirates of the Caribbean</movie>
    |     <movie genre="fairytale">Edward Scissorhands</movie>
    | </movies>

    printf("movies=%s\n", movies)
    printf("movies=%s\n", movies.text)

    // xpath baby
    val nodes = movies \ "movie"
    printf("nodes=%s\n",nodes)
    printf("nodes=%s\n",nodes.getClass)
    printf("nodes(0).text=%s\n",nodes(0).text)
    printf("nodes(0)'s genre=%s\n",nodes(0) \ "@genre")

    printf("doChore(cook dinner): %s\n", doChore("cook dinner"))
    printf("doChore(take out trash): %s\n", doChore("take out trash"))

    printf("factorial(5): %s\n", factorial(5))
    // Throws a scala.MatchError
    // printf("factorial(-1): %s\n", factorial(-1))

    val reg = """^(F|f)\w*""".r
    println(reg.findFirstIn("Fantastic Four"))
    println(reg.findFirstIn("not so Fantastic Four"))
  }

  def doChore(chore: String) : String = chore match {
    case "clean dishes" => "scrub, dry"
    case "cook dinner" => "chop, sizzle"
    case _ => "whine, complain"
  }

  def factorial(n: Int): Int = n match { 
    case 0 => 1
    case x if x > 0 => factorial(n - 1) * n
  }
}

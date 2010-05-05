// vim: set ts=2 sw=2 et:

import scala.io.Source
import scala.xml._

object SbtDepsFromPom {
  def main ( args: Seq[String] ) {
    //println("main, args=%s".format(args))
    val pomContents = Source.fromFile(args(0)).mkString
    //println("xml is: %d".format(pomContents.length))
    var xml = XML.loadString(pomContents)
    var deps = (xml \\ "dependency") map { (dep) =>
      //val groupid = 
      List( (dep \\ "groupId")(0).text,
            (dep \\ "artifactId")(0).text,
            (dep \\ "version")(0).text,
            (dep \\ "scope")(0).text)
    }
    //println("deps are: %s".format(deps))
    deps.foreach {(dep) =>
      dep.map { "\"%s\"".format(_) } match {
        case List(group, artifact, version, scope) => {
          println("%-28s %% %-18s %% %-12s %% %s".format(group,artifact,version,scope))
        }
      }
    }
  }
}

SbtDepsFromPom.main(argv)

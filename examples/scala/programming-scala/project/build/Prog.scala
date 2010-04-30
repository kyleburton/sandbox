import sbt._

class ProgProject(info: ProjectInfo) extends DefaultProject(info) {
  override def dependencyPath = "lib"
  override def disableCrossPaths = true

  override def repositories = Set(
    "Scala Testing Tools" at "http://scala-tools.org/repo-releases/",
    "Scala Tools" at "http://scala-tools.org/repo-releases/",
    "Simmetrics" at "http://maven.mse.jhu.edu/m2repository/",
    "Configgy" at "http://www.lag.net/repo",
    "ibiblio" at "http://mirrors.ibiblio.org/pub/mirrors/maven2/"
  )

  override def libraryDependencies = Set(
    // Libraries
    "net.lag" % "configgy" % "1.5.2" % "compile",
    // "uk.ac.shef.wit" % "simmetrics" % "1.6.2" % "compile",
    // Libraries for testing
    "org.scala-tools.testing" % "specs" % "1.6.2" % "test",
    "org.mockito" % "mockito-core" % "1.8.1" % "test",
    "org.objenesis" % "objenesis" % "1.1" % "test",
    "org.hamcrest" % "hamcrest-all" % "1.1" % "test"
  )

}

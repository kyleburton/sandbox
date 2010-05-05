// vim:set ts=2 sw=2 et ai:
import sbt._

class KestrelExampleProject(info: ProjectInfo) extends DefaultProject(info) {
  override def dependencyPath = "lib"
  override def disableCrossPaths = true

  override def mainClass = Some("com.github.kyleburton.examples.kestrel.Main")

  override def repositories = Set(
    "Scala Testing Tools" at "http://scala-tools.org/repo-releases/",
    "Scala Tools"         at "http://scala-tools.org/repo-releases/",
    "Configgy"            at "http://www.lag.net/repo",
    "ibiblio"             at "http://mirrors.ibiblio.org/pub/mirrors/maven2/"
  )

  override def libraryDependencies = Set(
    // Libraries
    "net.lag"                 % "configgy"     % "1.5.2"   % "compile",
    // Libraries for testing
    "org.scala-tools.testing" % "specs"        % "1.6.2.1" % "test",
    "org.mockito"             % "mockito-core" % "1.8.1"   % "test",
    "org.objenesis"           % "objenesis"    % "1.1"     % "test",
    "org.hamcrest"            % "hamcrest-all" % "1.1"     % "test",

    // kestrel dependencies
    //"org.scala-lang"             % "scala-compiler"   % "2.7.7"      % "runtime",
    //"org.scala-lang"             % "scala-library"    % "2.7.7"      % "runtime",
    //"org.scala-tools.testing"    % "specs"            % "1.6.1"      % "compile",
    "com.twitter"                % "xrayspecs"        % "1.0.6"      % "compile",
    //"net.lag"                    % "configgy"         % "1.4.3"      % "runtime",
    "net.lag"                    % "naggati"          % "0.7.2"      % "runtime",
    "org.apache.mina"            % "mina-core"        % "2.0.0-M6"   % "runtime",
    "org.slf4j"                  % "slf4j-api"        % "1.5.2"      % "runtime",
    "org.slf4j"                  % "slf4j-jdk14"      % "1.5.2"      % "runtime",

    //"org.scala-lang"             % "scala-compiler"   % "2.7.7"      % "compile",
    //"org.scala-lang"             % "scala-library"    % "2.7.7"      % "compile",
    //"org.scala-tools.testing"    % "specs"            % "1.6.1"      % "provided",
    "org.jmock"                  % "jmock"            % "2.4.0"      % "provided",
    //"org.hamcrest"               % "hamcrest-all"     % "1.1"        % "provided",
    "cglib"                      % "cglib"            % "2.1_3"      % "provided",
    "asm"                        % "asm"              % "1.5.3"      % "provided",
    "org.objenesis"              % "objenesis"        % "1.1"        % "provided",
    "org.scala-tools"            % "vscaladoc"        % "1.1"        % "provided",
    //"org.apache.mina"            % "mina-core"        % "2.0.0-M6"   % "compile",
    "org.slf4j"                  % "slf4j-jdk14"      % "1.5.2"      % "compile",
    "org.slf4j"                  % "slf4j-api"        % "1.5.2"      % "compile",
    "com.twitter"                % "xrayspecs"        % "1.0.5"      % "compile"
    //"net.lag"                    % "configgy"         % "1.4"        % "compile",
    //"net.lag"                    % "naggati"          % "0.7.2"      % "compile"


  )
}

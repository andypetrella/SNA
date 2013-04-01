import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {
  val SNAPSHOT = "-SNAPSHOT"

  val appName         = "SNA"
  val appVersion      = "1.0-SNAPSHOT"

  val neo4jPlungin = "be.nextlab" %% "neo4j-rest-play-plugin" % ("0.0.5" + SNAPSHOT)

  val appDependencies = Seq(
    // Add your project dependencies here,
    neo4jPlungin
  )


  val main = play.Project(appName, appVersion, appDependencies)

}

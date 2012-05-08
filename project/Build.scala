import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "openiedemo"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      "edu.washington.cs.knowitall" % "openiedemo-backend" % "0.0.1-SNAPSHOT"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here      
      resolvers += (
        "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"
      )
    )

}

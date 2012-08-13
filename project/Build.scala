import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "openiedemo"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      "edu.washington.cs.knowitall" % "openiedemo-backend" % "1.0.2-SNAPSHOT",
      "net.debasishg" % "sjson_2.9.1" % "0.17" 
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here      
        resolvers ++= Seq(
          "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository",
          "Internal Maven Repository" at "http://knowitall.cs.washington.edu/maven2",
          "Internal Snapshot Maven Repository" at "http://knowitall.cs.washington.edu/maven2-snapshot",
          "OpenNlp" at "http://opennlp.sourceforge.net/maven2")
    )
}

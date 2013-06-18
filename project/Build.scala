import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "openiedemo"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      "edu.washington.cs.knowitall.openie" %% "openie-backend" % "1.0",
      "edu.washington.cs.knowitall.openie" %% "openie-models" % "1.0",
      "edu.washington.cs.knowitall.nlptools" %% "nlptools-stem-morpha" % "2.4.2",
      "net.debasishg" %% "sjson" % "0.19",
      "org.apache.solr" % "solr-solrj" % "4.3.0"
    )

    val main = play.Project(appName, appVersion, appDependencies).settings(
      // Add your own project settings here
        resolvers ++= Seq(
          "internal snapshot" at "http://knowitall.cs.washington.edu/maven2-snapshot/",
          "internal release" at "http://knowitall.cs.washington.edu/maven2/"
        )
    )
}

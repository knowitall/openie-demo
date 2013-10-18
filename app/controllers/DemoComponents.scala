package controllers

import edu.knowitall.apps.Components

/**
 * An extension of edu.knowitall.apps.Components to contain
 * parsers for the demo.
 */
object DemoComponents {

  val paraphrasers = Components.paraphrasers
  val parsers = Components.parsers + ("demo-triple" -> DemoTripleParser())
}
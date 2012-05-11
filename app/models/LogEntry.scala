package models

import scala.actors.Futures.future

import java.io.{PrintWriter, FileOutputStream, File}
import java.util.{Calendar, Date}

import LogEntry.{LOG_DIRECTORY_FILE}
import edu.washington.cs.knowitall.common.Resource.using

case class LogEntry (
  query: Query,
  groupCount: Int,
  resultCount: Int,
  date: Date = new Date
) {
  import LogEntry._

  def toRow = {
    Iterable(
        query.arg1String,
        query.relString,
        query.arg2String,
        groupCount.toString,
        resultCount.toString,
        date.getTime.toString).mkString("\t")
  }

  def log() {
    if (!LOG_DIRECTORY_FILE.exists()) {
      LOG_DIRECTORY_FILE.mkdirs()
    }

    val date = new java.util.Date
    val file = new File(LOG_DIRECTORY_FILE, LogEntry.logFileName())

    LogEntry.synchronized {
      using(new PrintWriter(new FileOutputStream(file, true))) { writer =>
        writer.println(this.toRow)
      }
    }
  }
}

object LogEntry {
  final val LOG_DIRECTORY_FILE = new File(System.getProperty("user.home") + "/openiedemo/logs")

  def logFileName(): String = {
    val cal = Calendar.getInstance
    logFileName(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DAY_OF_MONTH))
  }

  def logFileName(year: Int, month: Int, day: Int): String = {
    "%0.4d.%0.2d.%0.2d.log".format(year, month, day)
  }
}

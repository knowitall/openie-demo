package models

import play.api.Logger

import scala.actors.Futures.future

import java.io.{PrintWriter, FileOutputStream, File}
import java.util.{Calendar, Date}

import LogEntry.{LOG_DIRECTORY_FILE}
import edu.washington.cs.knowitall.common.Resource.using

case class LogEntry (
  query: Query,
  answerCount: Int,
  sentenceCount: Int,
  date: Date = new Date
) {
  import LogEntry._

  def toRow = {
    Iterable(
        query.arg1String,
        query.relString,
        query.arg2String,
        answerCount.toString,
        sentenceCount.toString,
        date.getTime.toString).mkString("\t")
  }

  def log() {
    if (!LOG_DIRECTORY_FILE.exists()) {
      LOG_DIRECTORY_FILE.mkdirs()
    }

    val file = LogEntry.logFile()

    LogEntry.synchronized {
      using(new PrintWriter(new FileOutputStream(file, true))) { writer =>
        writer.println(this.toRow)
      }
    }
  }
}

object LogEntry {
  final val LOG_DIRECTORY_FILE = new File(System.getProperty("user.home") + "/openiedemo/logs")

  def logFile(): File = {
    val cal = Calendar.getInstance
    logFile(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH)+1, cal.get(Calendar.DAY_OF_MONTH))
  }

  def logFile(year: Int, month: Int, day: Int): File = {
    new File(LOG_DIRECTORY_FILE, "%04d.%02d.%02d.log".format(year, month, day))
  }

  def logs(year: Int, month: Int, day: Int): Seq[LogEntry] = {
    val file = logFile(year, month, day)
    val exists = file.exists()
    Logger.info("Retrieving logs in " + file + (if (!exists) "(not found)" else ""));
    if (!exists) Seq()
    else {
      using (io.Source.fromFile(logFile(year, month, day))) { source =>
        (for (line <- source.getLines) yield LogEntry.fromRow(line)).toList
      }
    }
  }

  def fromRow(row: String) = {
    def noneIfEmpty(string: String) = if (string.isEmpty) None else Some(string)
    val Array(arg1, rel, arg2, groupCount, resultCount, date) = row.split("\t")
    LogEntry(Query.fromStrings(arg1, rel, arg2), groupCount.toInt, resultCount.toInt, new Date(date.toLong))
  }
}

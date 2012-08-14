package models

import java.io.{PrintWriter, FileOutputStream, File}
import java.util.{Date, Calendar}
import java.net.UnknownHostException
import java.net.InetAddress
import java.text.SimpleDateFormat

import edu.washington.cs.knowitall.common.Resource.using

import LogEntry.LOG_DIRECTORY_FILE
import scala.util.control.Exception._
import play.api.Logger
import play.api.mvc.RequestHeader

case class LogEntry (
  query: Query,
  filter: String,
  answerCount: Int,
  sentenceCount: Int,
  address: String = "0.0.0.0",
  date: Date = new Date) {
  import LogEntry._

  def dateString = LogEntry.dateFormatter.format(date)

  def toRow = {
    Iterable(
        query.arg1String,
        query.relString,
        query.arg2String,
        filter,
        answerCount.toString,
        sentenceCount.toString,
        date.getTime.toString,
        address).mkString("\t")
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

  def fromUW = {
    address.startsWith("128.208.") || address.startsWith("128.95.")
  }
}

object LogEntry {
  private final val LOG_DIRECTORY_FILE = new File(System.getProperty("user.home") + "/openiedemo/logs")
  private final val dateFormatter = new SimpleDateFormat("yyyy.MM.dd 'at' HH:mm:ss z")

  def fromRequest(query: Query, filter: String, answerCount: Int, sentenceCount: Int, request: RequestHeader) = {
    val remoteIp = request.remoteAddress
    val remoteHost = catching(classOf[UnknownHostException]) opt (InetAddress.getByName(remoteIp).getHostName)

    val address = remoteHost match {
      case Some(host) if host != remoteIp => remoteIp + "/" + host
      case _ => remoteIp
    }

    LogEntry(query, filter, answerCount, sentenceCount, address)
  }

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
    Logger.info("Retrieving logs in " + file + (if (!exists) " (not found)" else ""));
    if (!exists) Seq()
    else {
      using (io.Source.fromFile(logFile(year, month, day))) { source =>
        (for (line <- source.getLines) yield LogEntry.fromRow(line)).toList
      }
    }
  }

  def fromRow(row: String) = {
    def noneIfEmpty(string: String) = if (string.isEmpty) None else Some(string)
    val Array(arg1, rel, arg2, filter, groupCount, resultCount, date, ip) = row.split("\t")
    LogEntry(Query.fromStrings(arg1, rel, arg2, "unknownCorpora"), filter, groupCount.toInt, resultCount.toInt, ip, new Date(date.toLong))
  }
}
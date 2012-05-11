import models.Query

import java.io.{ File, FileOutputStream, FileInputStream, ObjectOutputStream, ObjectInputStream }

import edu.washington.cs.knowitall.common.Resource.using

object PickleQuery extends App {
  def so(string: String) = if (string == "" || string == "none") None else Some(string)

  run(new File(args(0)), so(args(1)), so(args(2)), so(args(3)))

  def run(file: File, arg1: Option[String], rel: Option[String], arg2: Option[String]) = {
    val regs = Query.fetcher.getGroups(arg1, rel, arg2)

    println("writing...")
    using(new FileOutputStream(file)) { fos =>
      using(new ObjectOutputStream(fos)) { oos =>
        oos.writeObject(regs)
      }
    }

    // test read
    println("reading...")
    using(new FileInputStream(file)) { fis =>
      using(new ObjectInputStream(fis) {
        override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
          try { Class.forName(desc.getName, false, getClass.getClassLoader) }
          catch { case ex: ClassNotFoundException => super.resolveClass(desc) }
        }
      }) { ois =>
        ois.readObject().toString
      }
    }

    println("done.")
  }
}

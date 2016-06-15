
/**
 *
 * remove old backups from directory to save space
 * leave at least 2 backups and delete the last
 *
 */

import java.util.Date
import java.io.File
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributeView;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;

import scala.io._

case class Directory(path:String, mdate: Long)

object remover extends App {

	def listDirContentByAge(target: String): Seq[Directory] = {
		val f1 = new File(target)
		if (!f1.exists()) {
			throw new Exception("Directory not found! " + target)
		}
		val t = listDirMTimes(f1).toList.sortWith( _.mdate > _.mdate )
		t.foreach(println(_))
		t
	}

	def listDirMTimes(dir: File) : List[Directory] = {
		var list:List[Directory] = Nil
		for ( file <- dir.listFiles()) {
			println("\t>> " + file.getCanonicalPath())
			list = list :+ new Directory(file.getCanonicalPath(), getFileTime(file));
		}
		list
	}

	def runDirectory(t:String) {
		val l = listDirContentByAge(t)
		if (l.length <= 2 ) {
			return 
		}
		
		val target = l.last
		val f1 = new File(target.path)
		if (f1.exists()) {
			println("Deleting " + f1.getCanonicalPath() + " for target " + target.path)
			removeDir(f1)	
		}
	}

	def runTargets() {
		println("starting to read dirs.txt")
		if (!new File("dirs.txt").exists()) {
			throw new Exception("No file dirs.txt")
		}
		for(line <- Source.fromFile("dirs.txt").getLines()) {
			println("> " + line )
			if (!line.isEmpty() && !line.trim().equals("/") ) {
				runDirectory(line)
			}
		}
	}

	def getFileTime(f:File) :Long = {
		val p = Paths.get(f.getAbsolutePath());
	    val view = Files.getFileAttributeView(p, classOf[BasicFileAttributeView])
			.readAttributes()
		val fileTime = view.creationTime();
		//  also available view.lastAccessTine and view.lastModifiedTime
	    return fileTime.toMillis();
	}

	def removeDir(f1: File) {
		if (f1.isDirectory()) {
			for ( f <- f1.listFiles()) {
				if (f.isDirectory()) {
					removeDir(f)
				} else {
					f.delete()
				}
			}
		}  
		f1.delete()
	}

	runTargets()
}


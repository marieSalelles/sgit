package sgit.io

import better.files._
import scala.util.matching.Regex

object RepoSearching {

  /**
   * Search all the files and folders in the current directory.
   * @return : the sequence of all file/folder
   */
  def searchAllDirectoryFile ():Seq[File] = {
    val currentFolder: File = ".sgit/".toFile.parent
    val files :Seq[File] = currentFolder.listRecursively.toSeq
    val filesWithoutSgit = files.filterNot(f => f.path.toString.contains(".sgit"))
    val filesName = filesWithoutSgit.map((f :File) => f)
    filesName
  }

  /**
   * Search all files which match with the regex
   * @param regex : regular expression
   * @return the sequence of all file/folder
   */
  def searchDirectoryFile (regex :Regex):Seq[File] = {
    val currentFolder: File = ".sgit/".toFile.parent
    val files :Seq[File] = currentFolder.listRecursively.filter(f => regex.matches(f.name)).toSeq
    val filesWithoutSgit = files.filterNot(f => f.path.toString.contains(".sgit"))
    val filesName = filesWithoutSgit.map((f :File) => f)
    filesName
  }

  /**
   * Search files corresponding to the name given in the arguments
   * @param files : file name list
   * @return : file list
   */
  def searchDirectoryFile(files : Seq[String]) :Seq[File]= {
    val list = files.iterator.map((f: String) => if (f.toFile.exists) f.toFile else null).toSeq
    list.filterNot(f => f == null)
  }

}

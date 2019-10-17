package sgit.io

import better.files._
import sgit.objects.{Blob, Commit, StagedLine}

import scala.annotation.tailrec

object ReadFile {

  /**
   * Retrieve the name of a file
   * @param f : file sha key
   * @return the name of the file
   */
  def readName(f :String) :String= {
    val currentFolder = (".sgit/objects/"+ f)
    if (currentFolder.toFile.exists && f != "") {
      val content :String = currentFolder.toFile.contentAsString
      content.substring(0, content.indexOf('\n'))
    } else ""
  }

  /**
   * Retrieve the content of a file
   * @param f : file sha key
   * @return the name of the file
   */
  def readBlobContent(f :String) :String= {
    val currentFolder = (".sgit/objects/"+ f)
    if (currentFolder.toFile.exists && f != "") {
      val content :String = currentFolder.toFile.contentAsString
      content.substring(content.indexOf("\n"))
    } else ""
  }

  /**
   * Read the content of the staged file.
   * @return the list of the added files.
   */
  def readStaged() :Option[List[StagedLine]]  = {
    val line :List[String] = ".sgit/staged".toFile.contentAsString
      .replace("\r", "")
      .split("\n").toList
    //create a stagedLine object for each line
    if (line != List("")) {
      Some(line.map(l => {
        val ids = l.split(" ")
       StagedLine(ids(0), ids(1))
      }))
    } else None
  }

  /**
   * Read the content of a commit file.
   * @param commit : the sha key of a commit
   * @return the list of the committed files
   */
  def readCommit(commit :String) :List[StagedLine] = {
    val line :List[String] = (".sgit/objects/"+commit).toFile.contentAsString
      .replace("\r","")
      .split("\n").toList

    val contentWithoutParents = line.tail
    contentWithoutParents.tail.map(l => {
      val indices = l.split(" ")
      StagedLine(indices(0),indices(1))
    })
  }

  /**
   * Read the current branch in the HEAD file
   * @return the current branch (something like refs/heads/branchName)
   */
  def readHEAD() :String = {
    (".sgit/HEAD").toFile.contentAsString
  }

  /**
   * Read the last commit of a branch
   * @param branch the branch name
   * @return the name of the last commit (sha key)
   */
  def readHeads(branch: String) :Option[String] = {
    if ((".sgit/"+branch).toFile.exists) Some((".sgit/"+ branch).toFile.contentAsString)
    else None
  }

  /**
   * Read the last commit of a branch file
   * @param branch : branch name
   * @return the commit sha key
   */
  def readBranchCommit(branch: String): String = {
    (".sgit/"+branch).toFile.contentAsString
  }

  /**
   * Read the content of a tag file
   * @param tag : tag file path
   * @return the commit sha key
   */
  def readTagCommit(tag: String): String = {
    (".sgit/"+tag).toFile.contentAsString
  }

  /**
   * Retrieve all the commit properties and create a commit object.
   * @param commit: commit sha key
   * @return a commit object
   */
  def readCommitProperties(commit: Option[String]) :Option[Commit]= {
    if(commit.isEmpty) return None
    val commitFile = (".sgit/objects/" + commit.get).toFile
    if(commitFile.exists) {
      val content: Seq[String] = commitFile.contentAsString.replace("\r","").split("\n").toList
      val parents: Seq[String] = content.head.split(" ").toList
      val contentWithoutParents: Seq[String] = content.tail
      val message: String = contentWithoutParents.head
      val allFileElement: Seq[StagedLine] = contentWithoutParents.tail.map(f => {
        val line = f.split(" ")
        StagedLine(line(0), line(1))
      })
      Some(Commit(commit.get, commitFile.lastModifiedTime, parents, allFileElement, message))
    } else None
  }

  /**
   * Build blob object with the reading of the blob file (store in the object folder))
   * @param files : file list
   * @param blobs : sequence of built blobs
   * @return the list of blobs
   */
  @tailrec
  def readBlobFile(files: Seq[StagedLine],blobs: Seq[Blob]): Seq[Blob]= {
    if (files.isEmpty) blobs
    else {
      val file: StagedLine = files.head
      //retrieve all the file content (the file path in the working directory and the file content)
      val fileContent: String = (".sgit/objects/" + file.sha).toFile
        .contentAsString
        .replace("\r", "")
      readBlobFile(files.tail, blobs:+Blob(file.sha,fileContent.substring(fileContent.indexOf("\n")), file.path ))
    }
  }
}

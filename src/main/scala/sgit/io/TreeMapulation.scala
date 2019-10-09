package sgit.io

import sgit.objects.{Blob, Tree, TreeElement}
import better.files._

import scala.annotation.tailrec

object TreeMapulation {
/*
  def buildRepoTree(origin :TreeElement) :TreeElement = {
    val root :File = ".sgit/".toFile.parent
    val childrenList :List[File] = origin.sha.toFile.children.toList
    val folders :List[File] = childrenList.filter(c => c.isDirectory)
    val l1 :List[Tree] = folders
      .map( (c :File) =>
        Tree(c.sha1, buildRepoTree(TreeElement(root.relativize(c).toString, c.sha1)),root.relativize(c).toString ))

    val files = childrenList.filterNot(c => c.isDirectory)
    val l2 = files.map(c => if((".sgit/objects:"+c.sha1).toFile.exists) c.sha1)
    val contentChildren = l1.concat(l2)
    Tree(origin.sha,origin.name, contentChildren)
  }
  */
  /**
   * Adds children recursively to the tree.
   * @param files the files to add
   * @param res the result
   * @return the sequence of children
   */
  @tailrec
  def addChildren(files: Seq[File], res: Seq[TreeElement]): Seq[TreeElement] = {
    if(files.isEmpty) res
    else {
      val firstFile: File = files.head
      if(firstFile.isDirectory) addChildren(files.tail, res:+buildTree(firstFile).orNull)
      else {
        val root :File = ".sgit/".toFile.parent
        val blob = Blob(firstFile.sha1, firstFile.contentAsString,root.relativize(firstFile).toString)
        addChildren(files.tail, res:+blob)
      }
    }
  }

  /**
   * Builds the main tree from an origin.
   * @param origin the origin folder
   * @return the tree.
   */
  def buildTree(origin: File): Option[TreeElement] = {
    if(!origin.isDirectory) None
    val root :File = ".sgit/".toFile.parent
    val children: Seq[File]= origin.children.toIndexedSeq
    Some(Tree(origin.sha1, addChildren(children, Seq()).filterNot(elt => elt ==null), root.relativize(origin).toString))
  }
}

package io.github.tsnee.hpack.huffman

import scala.annotation.tailrec
import zio.Chunk

/** A Huffman code represented by a binary tree.
  *
  * Every `Leaf` node of the tree represents one character of the alphabet
  * that the tree can encode. The weight of a `Leaf` is the frequency of
  * appearance of the character.
  *
  * The branches of the huffman tree, the `Fork` nodes, represent a set
  * containing all the characters present in the leaves below it. The
  * weight of a `Fork` node is the sum of the weights of these leaves.
  */
object HuffmanTree extends HuffmanCodec {
  private sealed trait CodeTree
  private final case class Fork(
    left: CodeTree,
    right: CodeTree,
    bytes: List[Byte],
    weight: Int
  ) extends CodeTree
  private final case class Leaf(byte: Byte, weight: Int) extends CodeTree

  private def weight(tree: CodeTree): Int = tree match {
    case Leaf(_, w) => w
    case Fork(_, _, _, w) => w
  }

  private def bytes(tree: CodeTree): List[Byte] = tree match {
    case Leaf(byte, _) => List(byte)
    case Fork(_, _, byteList, _) => byteList
  }

  private def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(
      left,
      right,
      bytes(left) ::: bytes(right),
      weight(left) + weight(right)
    )

  private val tree: CodeTree = ???

  /** This function computes for each unique character in the list `bytes`
    *  the number of times it occurs. For example, the invocation
    *
    *   times(List('a', 'b', 'a'))
    *
    * returns the following (the order of the resulting list is not
    * important):
    *
    *   List(('a', 2), ('b', 1))
    */
  private def times(bytes: List[Byte]): List[(Byte, Int)] =
    if (bytes.isEmpty) return Nil
    else upsert(bytes.head, times(bytes.tail))

  private def upsert(byte: Byte, resultList: List[(Byte, Int)]): List[(Byte, Int)] =
    if (resultList.isEmpty) List((byte, 1))
    else if (resultList.head._1 == byte)
      (byte, resultList.head._2 + 1) :: resultList.tail
    else resultList.head :: upsert(byte, resultList.tail)

  /** Returns a list of `Leaf` nodes for a given frequency table `freqs`.
    *
    * The returned list should be ordered by ascending weights (i.e. the
    * head of the list should have the smallest weight), where the weight
    * of a leaf is the frequency of the character.
    */
  private def makeOrderedLeafList(freqs: List[(Byte, Int)]): List[Leaf] =
    if (freqs.isEmpty) Nil
    else
      insertLeaf(
        Leaf(freqs.head._1, freqs.head._2),
        makeOrderedLeafList(freqs.tail)
      )

  private def insertLeaf(leaf: Leaf, sortedLeaves: List[Leaf]): List[Leaf] =
    if (sortedLeaves.isEmpty) List(leaf)
    else if (leaf.weight < sortedLeaves.head.weight) leaf :: sortedLeaves
    else sortedLeaves.head :: insertLeaf(leaf, sortedLeaves.tail)

  /** Checks whether the list `trees` contains only one single code tree. */
  private def singleton(trees: List[CodeTree]): Boolean =
    !trees.isEmpty && trees.tail.isEmpty

  /** The parameter `trees` of this function is a list of code trees
    *  ordered by ascending weights.
    *
    * This function takes the first two elements of the list `trees` and
    * combines them into a single `Fork` node. This node is then added back
    * into the remaining elements of `trees` at a position such that the
    * ordering by weights is preserved.
    *
    * If `trees` is a list of less than two elements, that list should be
    * returned unchanged.
    */
  private def combine(trees: List[CodeTree]): List[CodeTree] =
    if (trees.isEmpty || trees.tail.isEmpty) trees
    else
      insertFork(makeCodeTree(trees.head, trees.tail.head), trees.tail.tail)

  private def insertFork(fork: Fork, sortedTrees: List[CodeTree]): List[CodeTree] =
    if (sortedTrees.isEmpty) List(fork)
    else if (weight(fork) < weight(sortedTrees.head)) fork :: sortedTrees
    else sortedTrees.head :: insertFork(fork, sortedTrees.tail)

  /** This function will be called in the following way:
    *
    *   until(singleton, combine)(trees)
    *
    * where `trees` is of type `List[CodeTree]`, `singleton` and `combine`
    * refer to the two functions defined above.
    */
  @tailrec
  private def until(
    singletonFunction: List[CodeTree] => Boolean,
    combineFunction: List[CodeTree] => List[CodeTree]
  )(
    trees: List[CodeTree]
  ): CodeTree =
    if (singletonFunction(trees)) trees.head
    else until(singletonFunction, combineFunction)(combineFunction(trees))

  /** This function creates a code tree which is optimal to encode the text
    *  `bytes`.
    *
    * The parameter `bytes` is an arbitrary text. This function extracts
    * the character frequencies from that text and creates a code tree
    * based on them.
    */
  private def createCodeTree(bytes: List[Byte]): CodeTree =
    until(singleton, combine)(makeOrderedLeafList(times(bytes)))

  override def decode(input: IndexedSeq[Byte]): Chunk[Byte] = {
    val output = decode(tree, expand(input))
    Chunk.fromIterable(output)
  }

  private def expand(input: IndexedSeq[Byte]): List[Bit] =
    input.toList.flatMap { byte =>
      List(byte & 0x80, byte & 0x40, byte & 0x20, byte & 0x10, byte & 0x08, byte & 0x04, byte & 0x02, byte & 0x01)
    }

  type Bit = Int

  /** Decodes the bit sequence `bits` using the code tree
    * `tree` and returns the resulting list of characters.
    */
  private def decode(tree: CodeTree, bits: List[Bit]): List[Byte] = {
    def decodeNextByte(subTree: CodeTree, bitsRemaining: List[Bit]): List[Byte] =
      subTree match {
        case Leaf(byte: Byte, _) => byte :: decodeNextByte(tree, bitsRemaining)
        case Fork(left: CodeTree, right: CodeTree, _, _) =>
          if (bitsRemaining.isEmpty) List()
          else if (bitsRemaining.head == 0) decodeNextByte(left, bitsRemaining.tail)
          else decodeNextByte(right, bitsRemaining.tail)
      }
    decodeNextByte(tree, bits)
  }

  override def encode(input: IndexedSeq[Byte]): Chunk[Byte] =
    compact(encode(input.toList))

  private def compact(xs: List[Bit]): Chunk[Byte] = ???

  /** Encodes `text` using the code tree `tree`
    * into a sequence of bits.
    */
  private def encode(text: List[Byte]): List[Bit] = {
    def traverseTree(subTree: CodeTree, bytesRemaining: List[Byte]): List[Bit] =
      subTree match {
        case Leaf(byteFound: Byte, _) => {
          assert(bytesRemaining.head == byteFound)
          traverseTree(tree, bytesRemaining.tail)
        }
        case Fork(left: CodeTree, right: CodeTree, _, _) =>
          if (bytesRemaining.isEmpty) List()
          else if (bytes(left).contains(bytesRemaining.head))
            0 :: traverseTree(left, bytesRemaining)
          else 1 :: traverseTree(right, bytesRemaining)
      }
    traverseTree(tree, text)
  }

  type CodeTable = List[(Byte, List[Bit])]

  /** Given a code tree, create a code table which contains, for every
    * character in the code tree, the sequence of bits representing that
    * character.
    */
  private def convert(tree: CodeTree): CodeTable = {
    def traverseTree(subTree: CodeTree, bitsSoFar: List[Bit]): CodeTable =
      subTree match {
        case Fork(left: CodeTree, right: CodeTree, _, _) =>
          traverseTree(left, bitsSoFar :+ 0) :::
            traverseTree(right, bitsSoFar :+ 1)
        case Leaf(byteFound: Byte, _) => List((byteFound, bitsSoFar))
      }
    traverseTree(tree, List())
  }

  /** This function encodes `text` according to the code tree `tree`.
    *
    * To speed up the encoding process, it first converts the code tree to
    * a code table and then uses it to perform the actual encoding.
    */
  private def quickEncode(tree: CodeTree)(text: List[Byte]): List[Bit] = {
    val codeTable = convert(tree)
    @tailrec
    def findInTable(partialCodeTable: CodeTable, byte: Byte): List[Bit] = {
      require(!partialCodeTable.isEmpty)
      if (partialCodeTable.head._1 == byte) partialCodeTable.head._2
      else findInTable(partialCodeTable.tail, byte)
    }
    def processText(unprocessedText: List[Byte]): List[Bit] =
      if (unprocessedText.isEmpty) List()
      else
        findInTable(codeTable, unprocessedText.head) :::
          processText(unprocessedText.tail)
    processText(text)
  }
}

package streams

import common._

/**
 * This component implements the solver for the Bloxorz game
 */
trait Solver extends GameDef {

  /**
   * Returns `true` if the block `b` is at the final position
   */
  def done(b: Block): Boolean = {
    b.isStanding &&
    goal.x == b.b1.x &&
    goal.y == b.b1.y
  }

  /**
   * This function takes two arguments: the current block `b` and
   * a list of moves `history` that was required to reach the
   * position of `b`.
   * 
   * The `head` element of the `history` list is the latest move
   * that was executed, i.e. the last move that was performed for
   * the block to end up at position `b`.
   * 
   * The function returns a stream of pairs: the first element of
   * the each pair is a neighboring block, and the second element
   * is the augmented history of moves required to reach this block.
   * 
   * It should only return valid neighbors, i.e. block positions
   * that are inside the terrain.
   */
  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] = {
    b.legalNeighbors.map {
      case (neighbor, move) => (neighbor, move :: history)
    }.toStream
  }

  /**
   * This function returns the list of neighbors without the block
   * positions that have already been explored. We will use it to
   * make sure that we don't explore circular paths.
   */
  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]): Stream[(Block, List[Move])] = {
    neighbors.filter{case (block, _) => !explored.contains(block)}
  }

  /**
   * The function `from` returns the stream of all possible paths
   * that can be followed, starting at the `head` of the `initial`
   * stream.
   * 
   * The blocks in the stream `initial` are """sorted by ascending""" path
   * length: the block positions with the shortest paths (length of
   * move list) are at the head of the stream.
   * 
   * The parameter `explored` is a set of block positions that have
   * been visited before, on the path to any of the blocks in the
   * stream `initial`. When search reaches a block that has already
   * been explored before, that position should not be included a
   * second time to avoid cycles.
   * 
   * The resulting stream should be """sorted by ascending""" path length,
   * i.e. the block positions that can be reached with the fewest
   * amount of moves should appear first in the stream.
   * 
   * Note: the solution should not look at or compare the lengths
   * of different paths - the implementation should """naturally"""
   * construct the correctly sorted stream.
   */
  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = {
    //note that this is nothing but implementing recursive BFS
    if (initial.isEmpty) Stream()
    else {
      val next_blocks: Stream[(Block, List[Move])] = for{
        (cur_block,history) <- initial
        next_block <- newNeighborsOnly(neighborsWithHistory(cur_block,history),explored)
      } yield next_block
      // this is because the case like
      // initial(block only, (0,0) never explored) : [(1,0),(0,1)...]
      // then, (0,0) is duplicated in next_blocks
      val new_next_blocks = next_blocks.toSet.toStream
      // all traversed block in initial list and make return value """naturally"""
      initial #::: from(new_next_blocks, explored ++ new_next_blocks.map(x=>x._1))
    }

  }

  /**
   * The stream of all paths that begin at the starting block.
   */
  lazy val pathsFromStart: Stream[(Block, List[Move])] = {
    val initial = Stream((startBlock,Nil))
    val explored:Set[Block] = Set()
    from(initial,explored)

  }

  /**
   * Returns a stream of all possible pairs of the goal block along
   * with the history how it was reached.
   */
  lazy val pathsToGoal: Stream[(Block, List[Move])] = {
    pathsFromStart.filter{case (block,_) => done(block)}
  }

  /**
   * The (or one of the) shortest sequence(s) of moves to reach the
   * goal. If the goal cannot be reached, the empty list is returned.
   *
   * Note: the `head` element of the returned list should represent
   * the first move that the player should perform from the starting
   * position.
   */
  lazy val solution: List[Move] = {
    val min_path = pathsToGoal minBy {_._2.length}
    min_path._2.reverse
  }
}

package forcomp

import common._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *  
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *  
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurence list.
   *  
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = {

    val w_preprocessed = w.toLowerCase().toList.sortWith((x:Char,y:Char)=> x<y)
    def pack(xs:List[Char]):List[Tuple2[Char,Int]] = xs match {
      case Nil => Nil
      case x::_ => {
        val (result, rest) = xs span (_==x)
        (x,result.length) :: pack(rest)
      }
    }
    pack(w_preprocessed)

  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    val concatnated_s = s.mkString
    wordOccurrences(concatnated_s)
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *  
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    // check assignment doc for usage of groupBy
    dictionary.groupBy((x:Word)=> wordOccurrences(x))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   * 
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      // _ is pivot
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      // ('b', 1) is pivot
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      // ('b', 2) is pivot
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    //note that Occurrences is nothing but "List[(Char, Int)]"
    occurrences match {
      case Nil => List(Nil)
      case (char, count) :: tl =>
        (for (rest <- combinations(tl); i <- 0 to count) yield if (i == 0) rest else (char, i) :: rest).toList
    }
  }
  /** Subtracts occurrence list `y` from occurrence list `x`.
   * 
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences ={
    //note that Occurrences is nothing but "List[(Char, Int)]" and it is analogous to Map
    val xMap = x.toMap
    // precondition: y is subset of x
    val update_info = for (y_occur <- y) yield (y_occur._1, xMap(y_occur._1) - y_occur._2)
    val updateMap = update_info.toMap
    val ret = for (x_info<-x) yield if (update_info.exists(a=> a._1 == x_info._1)) (x_info._1,updateMap(x_info._1)) else x_info
    ret.filter(x=>x._2!=0)
  }
  /** Returns a list of all anagram sentences of the given sentence.
   *  
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *  
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *  
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    val sentence_occurrence = sentenceOccurrences(sentence)

    def sentenceAnagramsInner(occurrences: Occurrences):List[Sentence] = {
      if (occurrences.isEmpty) List(Nil)
      else {
        for {
          occurence_one <- combinations(occurrences) // List[Occ]
          word <- dictionaryByOccurrences.getOrElse(occurence_one,List()) // List[Word] / List()이면 알아서 돌지 않음
          rest <- sentenceAnagramsInner(subtract(occurrences,occurence_one)) // List[ Sent == List[Word] ]
        } yield word::rest

      }
    }
    sentenceAnagramsInner(sentence_occurrence)
  }

}

//    sentenceAnagrams 함수 과정
//    val combin_of_occurrence = combinations(sentence_occurrence)
//    // 사전에 있는 되는 단어. Some((Occurrence,Some(단어))
//    val approved = combin_of_occurrence.map(x=> {
//      val word = dictionaryByOccurrences.get(x)
//      if(word!= None) Some((x,word)) else None
//      }
//    ).filter(x=> x!=None)
//    val combin_of_occurrence_new = approved.map{case(Some((o,Some(w)))) => combinations(subtract(sentence_occurrence,o))}
//    val approved_new = ... 하다보니... 트리처럼 커진다. 재귀를 찾았다.
package forcomp


object Anagrams {
  type Word = String

  type Sentence = List[Word]

  type Occurrences = List[(Char, Int)]


  val dictionary: List[Word] = loadDictionary


  def wordOccurrences(w: Word): Occurrences = w.toLowerCase.toList
    .groupBy(element=>element)
    .map((f:(Char,List[Char]))=>(f._1,f._2.size)).toList.sortBy(f=>f._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = s.toList.map((w:Word)=>w.toLowerCase.toList).flatten
    .groupBy(c=>c)
    .map((f:(Char,List[Char]))=>(f._1,f._2.size)).toList.sortBy(f=>f._1)


  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.map(f => (wordOccurrences(f),f))
    .groupBy(f=>f._1)
    .map(f=>(f._1,f._2.map(tmp=>tmp._2)))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] =
    dictionaryByOccurrences.filter(p=>p._2.contains(word.toLowerCase)).toList.head._2


  def combinations(occurrences: Occurrences): List[Occurrences] =  occurrences match {
      case List() => List(List())
      case (ch,occ)::rest => {
        val restOfElem = combinations(rest)
        restOfElem ::: (for{
          e <- restOfElem
          o <- 1 to occ
        }yield (ch,o) :: e)
        }
  }

  def subtract(x: Occurrences, y: Occurrences): Occurrences = y match {
    case List() => x //y is a subset of x
    case e::rest=>{
      subtract(x.map(xx=>{
        if(xx._1==e._1){
          (xx._1,xx._2-e._2)
        }
        else
          xx
      }).filter(e=>e._2!=0),rest)
    }
  }

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def getPossibleWord(occurrences : Occurrences): List[Sentence]={
      if(occurrences.isEmpty) List(Nil)
      else for{
        comb <- combinations(occurrences)
        w <- dictionaryByOccurrences getOrElse (comb,Nil)
        sentence <- getPossibleWord(subtract(occurrences,wordOccurrences(w)))
        if !comb.isEmpty
      } yield w::sentence
    }
    getPossibleWord(sentenceOccurrences(sentence))
  }
}

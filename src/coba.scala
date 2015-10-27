/**
 * Created by calvin-pc on 9/23/2015.
 */

class coba {
  import java.io.File
  import scala.io.Source

  /**
   *  Index for term weight
   *  Usage: inverted_document_file(term)(document_name)
   */
  var inverted_document_file: Map[String,Map[String,Float]] = Map()
  /**
   *  Map a term to IDF value
   *  Usage: IDFterm(term)
   */
  var IDFterm: Map[String,Float] = Map()
  // sequence of stop word
  var stop_word: Seq[String] = Seq()
  // map from no document to title document
  var no2title: Map[String,String] = Map()

  def stringToWordVec (s:String, stop_word: Seq[String], stem: Boolean): Seq[String] = {
    val word_vec = s split "\\W+" filterNot( stop_word.contains(_))
    if (stem) {
      val stemmer = new Porter()
      word_vec filterNot (_.length == 0) map ( stemmer.stripAffixes(_))
    }
    else word_vec
  }

  def calculateTF (term: String, word_map: Map[String,Int], tfKind: coba.TF) : Double = {
    val rTF = word_map(term)
    val TF = tfKind match {
      case x:coba.noTF => 1
      case x:coba.rawTF => rTF
      case x:coba.binaryTF => Math.min(rTF,1)
      case x:coba.logisticTF => if (rTF > 1) 1 + Math.log(rTF)
                                  else rTF
      case x:coba.augmentedTF => 0.5 + 0.5 * rTF / word_map.maxBy(t => {val (word,freq) = t; freq})._2
      case _ => 1
    }
    TF
  }

  /**
   * Update stop_word, inverse_document, and IDFterm and no2title
   */
  def create_index (tf: coba.TF, idf: Boolean, normalization: Boolean, stemmer : Boolean,
                    stop_word_location: String, document_collection_location: String) {
    // get stop_word
    if (!stop_word_location.isEmpty)
    {
      val source = Source.fromFile(new File(stop_word_location))
      stop_word = source.mkString split "\\W+" map (_.toLowerCase)
    }
    val dir = new File(document_collection_location)

    val listFile = for {
      file <- dir.listFiles()
      if(file.isFile)
    } yield file

    val listContent = listFile flatMap (file => {
      val source = Source.fromFile(file)
      try {
        val title = source.getLines().next()
        no2title = no2title + (file.getName -> title)
        Seq((file.getName,source.mkString.toLowerCase))
      } catch {
        case _ => Seq();
      } finally {
        source.close()
      }
    })

    val listWordMap = listContent map ( t => {
      val (name,content) = t
      val word_vec = stringToWordVec(content,stop_word,stemmer)
      val word_map = word_vec.groupBy( t => t).mapValues(_.length).withDefaultValue(0)
      (name,word_map)
    })

    val listTerm = (listWordMap flatMap ( t => {
      val (name,word_map) = t
      word_map.keys.toSeq
    })).distinct.sorted

    val listName = (listWordMap flatMap ( t => {
      val (name,word_map) = t
      name
    }))

    IDFterm = listTerm.map (term => {
      val idf = Math.log(listWordMap.length.toDouble / listWordMap.count(t => {
        val (_, word_map) = t
        word_map(term) > 0
      })
      )
      term -> idf.toFloat
    }).toMap.withDefaultValue(0)

    inverted_document_file = (listTerm map ( term => {
      val document_weight: Map[String,Float] = (listWordMap flatMap ( t => {
        val (name,word_map) = t
        if (word_map.contains(term))
          Seq(name -> TF_IDF(term,word_map,tf,idf).toFloat)
        else Seq()
      })).toMap.withDefaultValue(0)

      term -> document_weight
    }) toMap) withDefaultValue(Map().withDefaultValue(0))

    if (normalization) {
      val name_to_weights = inverted_document_file.values.flatMap(_.toSeq).groupBy(_._1)
      val name_to_length = name_to_weights.mapValues({ weights =>
          val length_square = weights.map( t => t._2).map(t => t * t).foldLeft(0.0f)(_ + _)
          Math.sqrt(length_square)
      })
      inverted_document_file = inverted_document_file.mapValues({doc2termweight =>
        doc2termweight.map( {t =>
          val (name,weight) = t
          (name,weight/name_to_length(name).toFloat)
        }) withDefaultValue(0)
      })

      inverted_document_file = inverted_document_file.withDefaultValue(Map().withDefaultValue(0))
    }
  }

  def test(): Unit = {
    create_index(coba.rawTF(),true,true,false,"","D:\\tugas\\STBI\\doc_col")
  }

  def search(tf: coba.TF, idf: Boolean, normalization: Boolean, stemmer : Boolean,
              query: String) : Seq[(String,Float)] = {
    val word_vec = stringToWordVec(query,stop_word,stemmer)
    val word_map = word_vec.groupBy( t => t).mapValues(_.length).withDefaultValue(0)
    var listWordWeight = word_vec.map(word => (word,TF_IDF(word,word_map,tf, idf)))
    if (normalization) {
      val length = Math.sqrt(listWordWeight.map(_._2).map(t => t * t).sum)
      listWordWeight = listWordWeight.map(t => (t._1,t._2/length))
    }
    val all_weight = listWordWeight.foldLeft(Seq():Seq[(String,Float)])((seq,t) => {
      val (word,weight) = t
      val seq_weight = inverted_document_file(word).mapValues(_*weight).mapValues(_.toFloat).toSeq
      seq ++ seq_weight
    })
    val document_similarity = all_weight.groupBy(_._1).mapValues(_.foldLeft(0.0f)(_ + _._2))
    document_similarity.toSeq.sortBy(_._2).reverse
  }

  def experiment_query(tf: coba.TF, idf: Boolean, normalization: Boolean, stemmer : Boolean,
                       query: String, relevance: Seq[String]) = {
    def computeRecall (judgement : Seq[String], result: Seq[String]) : Float = {
      val relevant_result = result.filter(judgement.contains(_))
      relevant_result.length.toFloat / judgement.length
    }

    def computePrecision (judgement : Seq[String], result: Seq[String]) : Float = {
      val relevant_result = result.filter(judgement.contains(_))
      if (result.length == 0) 0
      else relevant_result.length.toFloat / result.length
    }

    def computeInterpolatedPrecision (judgement : Seq[String], result: Seq[String]) : Float = {
      def recurse(cur_index : Int, matched: Int, res: Seq[String], acc:Double): Double = {
        if (res.isEmpty || matched == judgement.length) return acc
        val head = res.head
        val tail = res.tail
        if (judgement.contains(head))
          recurse(cur_index + 1, matched + 1, tail, acc + matched.toFloat/(cur_index + 1))
        else
          recurse(cur_index + 1, matched , tail, acc)
      }
      (recurse(0,0,result,0) / judgement.length).toFloat
    }

    def computeNonInterpolatedPrecision (judgement : Seq[String], result: Seq[String]) : Float = {
      def recurse(cur_index : Int, matched: Int, res: Seq[String], acc:Double): Double = {
        if (res.isEmpty || matched == judgement.length) return acc
        val head = res.head
        val tail = res.tail
        if (judgement.contains(head))
          recurse(cur_index + 1, matched + 1, tail, acc + matched.toFloat/(cur_index + 1))
        else
          recurse(cur_index + 1, matched , tail, acc)
      }
      (recurse(0,0,result,0) / judgement.length).toFloat
    }

    val result = search(tf,idf,normalization,stemmer,query)
    val resultString = result.map(_._1)
    experimentResult(
      query,
      result,
      computePrecision(relevance,resultString),
      computeRecall(relevance,resultString),
      computeNonInterpolatedPrecision(relevance,resultString))
  }

  def experiment(tf: coba.TF, idf: Boolean, normalization: Boolean, stemmer : Boolean,
                       query_location: String, relevance_location: String): Seq[(String,experimentResult)] = {
    val query_dir = new File(query_location)
    val relevance_dir = new File(relevance_location)

    def getNameContent (dir: File): Seq[(String,String)] = {
      val listFile = for {
        file <- dir.listFiles()
        if(file.isFile)
      } yield file

      val listContent = listFile flatMap (file => {
        val source = Source.fromFile(file)
        try {
          Seq((file.getName,source.mkString.toLowerCase))
        } catch {
          case _ => Seq();
        } finally {
          source.close()
        }
      })

      listContent
    }

    val query_content = getNameContent(query_dir)
    val relevance_content = getNameContent(relevance_dir)

    val experiment_data = for {
      (query_name,query_text) <- query_content
      (relevance_name,relevance_text) <- relevance_content
      if (query_name == relevance_name)
    } yield (query_name,query_text,relevance_text.lines.filterNot(_.isEmpty))

    experiment_data.map( t=> {
      val (name,text,relevances) = t
      (name,experiment_query(tf,idf,normalization,stemmer,text,relevances.toSeq))
    })
  }

  def experimentText(tf: coba.TF, idf: Boolean, normalization: Boolean, stemmer : Boolean,
                 query_location: String, relevance_location: String): String = {
    val list = experiment(tf,idf,normalization,stemmer,query_location,relevance_location)
    list.foldLeft(new String())((s, t) => {
      val (name,result) = t
      var ret = s + "Query name: " + name + System.lineSeparator()
      ret = ret + "Query text: " + result.query + System.lineSeparator();
      ret = ret + "Precission: " + result.precission + System.lineSeparator();
      ret = ret + "Recall: " + result.recall + System.lineSeparator();
      ret = ret + "Non Interpolated recall precission: " + result.interpolated_precission + System.lineSeparator()
      ret = ret + "-------------" + System.lineSeparator()
      ret
    })
  }

  def searchToText(tf: coba.TF, idf: Boolean, normalization: Boolean, stemmer : Boolean,
             query: String) : String = {
    search(tf,idf,normalization,stemmer,query).foldLeft("")((z,t) => {
      val (name,similarity) = t
      z + name + " title:" + no2title(name) + " similarity:" + similarity + System.lineSeparator()
    })
  }

  def TF_IDF(term:String,word_map:Map[String,Int], tfKind:coba.TF, idf:Boolean) = {
    val rTF = word_map(term)
    val TF = calculateTF(term,word_map,tfKind)
    val IDF =
      if (!idf) 1
      else IDFterm(term)
    TF * IDF
  }
}

case class experimentResult (query:String, list_doc : Seq[(String,Float)], precission : Float
                             , recall: Float, interpolated_precission : Float)

object coba {
  sealed trait TF;
  case class noTF() extends TF;
  case class logisticTF() extends TF;
  case class augmentedTF() extends TF;
  case class rawTF() extends TF;
  case class binaryTF() extends TF;
  def main(args: Array[String]) = {
    import java.io.PrintWriter

    val doc_location = "D:\\tugas\\STBI\\CISI\\doc"
    val query_location = "D:\\tugas\\STBI\\CISI\\query"
    val relevance_location = "D:\\tugas\\STBI\\CISI\\relevance"
    val stop_word_location = "D:\\tugas\\STBI\\stop_word.txt"

    val list_TF = Seq(new binaryTF(), new augmentedTF(), new logisticTF(), new rawTF())
    val name_TF = Seq("Binary TF","Augmented TF","Logistic TF","Raw TF")
    val all_TF = list_TF.zip(name_TF)

    val list_IDF = Seq(true,false)
    val name_IDF = Seq("IDF","Tidak IDF")
    val all_IDF = list_IDF.zip(name_IDF)

    val list_Stem = Seq(true,false)
    val name_Stem = Seq("Dengan stemmer","Tidak pakai stemmer")
    val all_Stem = list_Stem.zip(name_Stem)

    val list_normal = Seq(true,false)
    val name_normal = Seq("Dengan normalization","Tidak pakai normalization")
    val all_normal = list_Stem.zip(name_normal)

    val all_configuration = for {
      t <- all_TF;
      i <- all_IDF;
      s <- all_Stem;
      n <- all_normal
    } yield (t,i,s,n)

    println("Done listing configuration")

    val all_result = all_configuration.flatMap( conf1 => {
      println("Index = " + conf1.toString())
      val search_engine = new coba;
      search_engine.create_index(conf1._1._1,conf1._2._1,conf1._4._1,conf1._3._1,stop_word_location,doc_location)
      all_configuration.map(conf2 => {
        println("Query = " + conf2.toString())
        val res = search_engine.experiment(conf2._1._1,conf2._2._1,conf2._4._1,conf2._3._1,query_location,relevance_location)
        val stat_precission = new Statistic(res.map(t => t._2.precission).map(_.toDouble).toArray)
        val stat_recall = new Statistic(res.map(t => t._2.recall).map(_.toDouble).toArray)
        val stat_non_interpolated = new Statistic(res.map(t => t._2.interpolated_precission).map(_.toDouble).toArray)
        (conf1,conf2,stat_precission,stat_recall,stat_non_interpolated)
      })
    })

    println("Done listing all result")
    val string = new StringBuilder
    all_result.map( t => {
      val (conf_index,conf_query,stat_precission,stat_recall,stat_non_interpolated) = t
      string.append("Konfigurasi index" + System.lineSeparator())
      string.append("TF = " + conf_index._1._2 + System.lineSeparator())
      string.append("IDF = " + conf_index._2._2 + System.lineSeparator())
      string.append("Stemmer = " + conf_index._3._2 + System.lineSeparator())
      string.append("Normalization = " + conf_index._4._2 + System.lineSeparator())
      string.append("------------" + System.lineSeparator())
      string.append("Konfigurasi query" + System.lineSeparator())
      string.append("TF = " + conf_query._1._2 + System.lineSeparator())
      string.append("IDF = " + conf_query._2._2 + System.lineSeparator())
      string.append("Stemmer = " + conf_query._3._2 + System.lineSeparator())
      string.append("Normalization = " + conf_query._4._2 + System.lineSeparator())
      string.append("------------" + System.lineSeparator())
      string.append("Precission" + System.lineSeparator())
      string.append("Mean = " + stat_precission.getMean + System.lineSeparator())
      string.append("Std Dev = " + stat_precission.getStdDev + System.lineSeparator())
      string.append("Median = " + stat_precission.median() + System.lineSeparator())
      string.append("------------" + System.lineSeparator())
      string.append("Recall" + System.lineSeparator())
      string.append("Mean = " + stat_recall.getMean + System.lineSeparator())
      string.append("Std Dev = " + stat_recall.getStdDev + System.lineSeparator())
      string.append("Median = " + stat_recall.median() + System.lineSeparator())
      string.append("------------" + System.lineSeparator())
      string.append("Non Interpolated Precission" + System.lineSeparator())
      string.append("Mean = " + stat_non_interpolated.getMean + System.lineSeparator())
      string.append("Std Dev = " + stat_non_interpolated.getStdDev + System.lineSeparator())
      string.append("Median = " + stat_non_interpolated.median() + System.lineSeparator())
      string.append("------------END-----------------" + System.lineSeparator())
    })
    val best_prec = all_result.sortBy(_._3.getMean).reverse.head
    string.append("Best Mean Precission" + System.lineSeparator())
    string.append("Mean = " + best_prec._3.getMean + System.lineSeparator())
    string.append("Konfigurasi Index" + best_prec._1.toString() + System.lineSeparator())
    string.append("Konfigurasi Query" + best_prec._2.toString() + System.lineSeparator())
    string.append("------------END-----------------" + System.lineSeparator())
    val best_recall = all_result.sortBy(_._4.getMean).reverse.head
    string.append("Best Mean Recall" + System.lineSeparator())
    string.append("Mean = " + best_recall._4.getMean + System.lineSeparator())
    string.append("Konfigurasi Index" + best_recall._1.toString() + System.lineSeparator())
    string.append("Konfigurasi Query" + best_recall._2.toString() + System.lineSeparator())
    string.append("------------END-----------------" + System.lineSeparator())
    val best_non_interpolated = all_result.sortBy(_._5.getMean).reverse.head
    string.append("Best Mean Non interpolated average precission" + System.lineSeparator())
    string.append("Mean = " + best_non_interpolated._5.getMean + System.lineSeparator())
    string.append("Konfigurasi Index" + best_non_interpolated._1.toString() + System.lineSeparator())
    string.append("Konfigurasi Query" + best_non_interpolated._2.toString() + System.lineSeparator())
    string.append("------------END-----------------" + System.lineSeparator())

    println(string)
    val writer = new PrintWriter("result.txt", "UTF-8");
    writer.println(string.toString());
    writer.close();
  }
}
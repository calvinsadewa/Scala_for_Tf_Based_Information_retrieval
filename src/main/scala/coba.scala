import java.io.PrintWriter

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
  var stop_word: Set[String] = Set()
  // map from no document to title document
  var no2title: Map[String,String] = Map()
  //set of all document title
  var setDocument: Set[String] = Set()
  //old query weight, in case we should display it
  var oldQuery2Weights: Map[String,Double] = Map()
  //new query weight, in case we should display it
  var newQuery2Weights: Map[String,Double] = Map()

  //tokenize string to word vector
  def stringToWordVec (s:String, stop_word: Set[String], stem: Boolean): Seq[String] = {
    val word_vec = s split "\\W+" filterNot( stop_word.contains(_))
    if (stem) {
      val stemmer = new Porter()
      word_vec filterNot (_.length == 0) map ( stemmer.stripAffixes(_))
    }
    else word_vec
  }

  //calculate TF of a term in a document (word_map)
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
      stop_word = source.mkString split "\\W+" map (_.toLowerCase) toSet
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

    val listWordVec = listContent map ( t => {
      val (name, content) = t
      (name,stringToWordVec(content, stop_word, stemmer))
    })

    val listWordMap = listWordVec map ( t => {
      val (name,word_vec) = t
      val word_map = word_vec.groupBy( t => t).mapValues(_.length).withDefaultValue(0)
      (name,word_map)
    })

    val listTerm = (listWordMap flatMap ( t => {
      val (name,word_map) = t
      word_map.keys.toSeq
    })).distinct.sorted

    setDocument = listWordMap map ( t => {
      val (name,word_map) = t
      name
    }) toSet

    {
      val temp_occurence : collection.mutable.Map[String,Int] = collection.mutable.Map()
      listTerm.map (term => {
        temp_occurence.update(term,0)
      })

      for {
        (_, word_map) <- listWordMap
        term <- word_map.keys
      } yield temp_occurence.update(term,temp_occurence(term) + 1)

      val n = listWordMap.length

      IDFterm = temp_occurence.mapValues( value => Math.log(n/value).toFloat).toMap.withDefaultValue(0.0f);
    }

    {
      val temp_inverted_document_file: collection.mutable.Map[String, scala.collection.mutable.Map[String, Float]] = scala.collection.mutable.Map()
      var doc2length: Map[String,Double] = Map()

      listTerm map (term => {
        temp_inverted_document_file.update(term,scala.collection.mutable.Map().withDefaultValue(0))
      })

      if (!normalization) {
        listWordMap map (t => {
          val (name,word_map) = t
          word_map.toSeq map (t2 => {
            val (term, _) = t2
            temp_inverted_document_file(term).update(name,TF_IDF(term,word_map,tf,idf).toFloat)
          })
        })
        doc2length = doc2length.withDefaultValue(1)
      }
      else {
        val mutable_doc2length: collection.mutable.Map[String,Double] = collection.mutable.Map().withDefaultValue(0)
        listWordMap map (t => {
          val (name,word_map) = t
          word_map.toSeq map (t2 => {
            val (term, _) = t2
            val weight = TF_IDF(term,word_map,tf,idf)
            temp_inverted_document_file(term).update(name,weight.toFloat)
            mutable_doc2length.update(name,mutable_doc2length(name) + weight * weight)
          })
        })
        doc2length = mutable_doc2length.toMap.mapValues(Math.sqrt(_))
      }

      inverted_document_file = temp_inverted_document_file.mapValues(
        _.toMap.map(t => {
          val (doc,weight) = t
          (doc,weight/doc2length(doc).toFloat)
        }).withDefaultValue(0.0f)
      ).toMap.withDefaultValue(Map().withDefaultValue(0))

    }

    val writer = new PrintWriter("inverted_file.csv", "UTF-8");
    writer.println("Term , Document , Weight");
    for {
      (term,map) <- inverted_document_file
      (document,weight) <- map
    } yield writer.println(term + " , " + document + " , " + weight)
    writer.close();
  }

  //return List of (doc name,similarity) by search the inverted file using the
  //TF-IDF-Normalization weighting method for query
  def search(tf: coba.TF, idf: Boolean, normalization: Boolean, stemmer : Boolean,
              query: String) : Seq[(String,Float)] = {
    val word2weight = map_query2weight(tf,idf,normalization,stemmer,query)
    newQuery2Weights = word2weight
    searchWithWeight(word2weight)
  }

  //get map (query_term -> weight) from query using TF-IDF-Normalization weighting method
  def map_query2weight(tf: coba.TF, idf: Boolean, normalization: Boolean, stemmer : Boolean,
                   query: String) : Map[String, Double] = {
    val word_vec = stringToWordVec(query,stop_word,stemmer)
    val word_map = word_vec.groupBy( t => t).mapValues(_.length).withDefaultValue(0)
    val word2weight = word_vec.map(word => (word,TF_IDF(word,word_map,tf, idf))).toMap
    if (normalization) {
      val length = Math.sqrt(word2weight.map(_._2).map(t => t * t).sum)
      word2weight.mapValues(_/length)
    }
    else word2weight
  }

  //Relevance Feedback using old weighted query(query2weight)
  def relevanceFeedbackSearch(query2weight: Map[String,Double], top_n: Int, feedback_type:coba.FeedbackType,
                       relevance_set: Set[String], search_seen:Boolean, prev_result:Seq[String]) : Seq[(String,Float)] = {
    val seen_doc = prev_result.take(top_n).toSet
    val relevant2docs = seen_doc.toSeq.groupBy(relevance_set.contains(_)).withDefaultValue(Seq())
    val relevants = relevant2docs(true)
    val not_relevants = relevant2docs(false)

    val new_query2weight = relevanceFeedbackQueryWeight (relevants,not_relevants, query2weight,feedback_type)
    oldQuery2Weights = query2weight
    newQuery2Weights = new_query2weight

    val ret = searchWithWeight(new_query2weight)
    if (search_seen) ret
    else {
      ret.filterNot{case (doc,sim) => seen_doc.contains(doc)}
    }
  }

  //Pseudo feedback variant of search
  def pseudoFeedbackSearch(tf: coba.TF, idf: Boolean, normalization: Boolean, stemmer : Boolean,
             query: String, top_n: Int, feedback_type:coba.FeedbackType) : Seq[(String,Float)] = {
    val word2weight = map_query2weight(tf,idf,normalization,stemmer,query)
    val first_result = searchWithWeight(word2weight)
    val (relevants, rest) = first_result.map(_._1).splitAt(top_n)
    val not_relevants = rest ++ ((setDocument -- relevants -- rest).toSeq)
    val new_word2weight = relevanceFeedbackQueryWeight (relevants,not_relevants, word2weight,feedback_type)
    oldQuery2Weights = word2weight
    newQuery2Weights = new_word2weight
    searchWithWeight(new_word2weight)
  }

  //return the modified query weight using the relevance feedback type
  // not_relevants head should be has max similarity if using ide_dec_hi feedback type
  def relevanceFeedbackQueryWeight (relevants:Seq[String],not_relevants:Seq[String], query2weight: Map[String,Double],
                                    feedback_type:coba.FeedbackType): Map[String,Double] = {
    feedback_type match {
      case coba.roccio() => query2weight.map{case (term,weight) => {
        val doc2weight = inverted_document_file(term)
        var new_weight = weight + relevants.foldLeft(0.0)(_ + doc2weight(_)) / Math.max(relevants.length,1)
        new_weight = new_weight - not_relevants.foldLeft(0.0)(_ + doc2weight(_)) / Math.max(not_relevants.length,1)
        (term,new_weight)
      }}.filter(_._2 > 0)
      case coba.ide_regular() => query2weight.map{case (term,weight) => {
        val doc2weight = inverted_document_file(term)
        var new_weight = weight + relevants.foldLeft(0.0)(_ + doc2weight(_))
        new_weight = new_weight - not_relevants.foldLeft(0.0)(_ + doc2weight(_))
        (term,new_weight)
      }}.filter(_._2 > 0)
      case coba.ide_dec_hi() => query2weight.map{case (term,weight) => {
        val doc2weight = inverted_document_file(term)
        var new_weight = weight + relevants.foldLeft(0.0)(_ + doc2weight(_))
        new_weight = new_weight - not_relevants.headOption.map(doc2weight(_)).getOrElse(0.0f)
        (term,new_weight)
      }}.filter(_._2 > 0)
    }
  }

  //get List of (doc name, similarity) using query with weight and inverted file
  def searchWithWeight(word2weight: Map[String,Double]) : Seq[(String,Float)] = {

    var document_similarity:Map[String,Float] = Map()

    {
      val doc2sim: collection.mutable.Map[String,Float] = collection.mutable.Map().withDefaultValue(0)
      word2weight.map(t => {
        val (word,weight) = t
        inverted_document_file(word).mapValues(_*weight).mapValues(_.toFloat).map(t => {
          val (doc,sim) = t
          doc2sim.update(doc,doc2sim(doc)+sim)
        })
      })

      document_similarity = doc2sim.toMap
    }
    document_similarity.toSeq.sortBy(_._2).reverse
  }

  //Computer the experiment result (precission,recall,non interpolated average precission)
  //for a search result by query using relevance
  def computeExperimentResult(query:String, search_result: Seq[(String,Float)], relevance: Seq[String]): experimentResult = {
    def computeRecall (judgement : Set[String], result: Seq[String]) : Float = {
      val relevant_result = result.filter(judgement.contains(_))
      relevant_result.length.toFloat / Math.max(judgement.size,1)
    }

    def computePrecision (judgement : Set[String], result: Seq[String]) : Float = {
      val relevant_result = result.filter(judgement.contains(_))
      relevant_result.length.toFloat / Math.max(result.length,1)
    }

    def computeNonInterpolatedPrecision (judgement : Set[String], result: Seq[String]) : Float = {
      var ret = 0.0
      result.zipWithIndex.foldLeft(0.0)((matched,t) => {
        val (doc,index) = t
        if (judgement.contains(doc)) {
          ret = ret + matched / (index + 1)
          matched + 1
        }
        else matched
      })
      (ret/Math.max(judgement.size,1)).toFloat
    }

    val resultString = search_result.map(_._1)
    val judge_set = relevance.toSet
    experimentResult(
      query,
      search_result,
      computePrecision(judge_set,resultString),
      computeRecall(judge_set,resultString),
      computeNonInterpolatedPrecision(judge_set,resultString))
  }

  //Experiment for normal TF-IDF-Normalization model
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
    } yield (query_name,query_text,relevance_text.lines.filterNot(_.isEmpty).toSeq)

    experiment_data.map( t=> {
      val (name,text,relevances) = t
      val search_result = search(tf,idf,normalization,stemmer,text)
      (name,computeExperimentResult(text,search_result,relevances.toSeq))
    })
  }

  //Experiment for pseudo feedback TF-IDF-Normalization model
  def experimentPseudoFeedback(tf: coba.TF, idf: Boolean, normalization: Boolean, stemmer : Boolean,
                 query_location: String, relevance_location: String, top_n: Int, feedback_type:coba.FeedbackType): Seq[(String,experimentResult)] = {
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
    } yield (query_name,query_text,relevance_text.lines.filterNot(_.isEmpty).toSeq)

    experiment_data.map( t=> {
      val (name,text,relevances) = t
      val search_result = pseudoFeedbackSearch(tf,idf,normalization,stemmer,text,top_n,feedback_type)
      (name,computeExperimentResult(text,search_result,relevances.toSeq))
    })
  }

  //Experiment for relevance feedback TF-IDF-Normalization model
  def experimentRelevanceFeedback(tf: coba.TF, idf: Boolean, normalization: Boolean, stemmer : Boolean,
                                  query_location: String, relevance_location: String, top_n: Int,
                                  feedback_type:coba.FeedbackType, search_seen: Boolean): Seq[(String,experimentResult)] = {
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
    } yield (query_name,query_text,relevance_text.lines.filterNot(_.isEmpty).toSeq)

    experiment_data.map( t=> {
      val (name,text,relevances) = t
      val query2weight = map_query2weight(tf,idf,normalization,stemmer,text)
      val first_result = searchWithWeight(query2weight)
      val search_result = relevanceFeedbackSearch(
        query2weight,top_n,feedback_type, relevances.toSet,search_seen,first_result.map(_._1)
      )
      (name,computeExperimentResult(text,search_result,relevances.toSeq))
    })
  }

  // change experiment result to readable string, mainly for convinience
  def experiments2Text(experiments: Seq[(String,experimentResult)]): String = {
    val text = experiments.foldLeft(new String())((s, t) => {
      val (name,result) = t
      var ret = s + "Query name: " + name + System.lineSeparator()
      ret = ret + "Query text: " + result.query + System.lineSeparator();
      ret = ret + "Precission: " + result.precission + System.lineSeparator();
      ret = ret + "Recall: " + result.recall + System.lineSeparator();
      ret = ret + "Non Interpolated average precission: " + result.interpolated_precission + System.lineSeparator()
      ret = ret + "-------------" + System.lineSeparator()
      ret
    });
    {
      val precissions = experiments.map(_._2.precission.toDouble).toArray
      val recalls = experiments.map(_._2.recall.toDouble).toArray
      val NIAPs = experiments.map(_._2.interpolated_precission.toDouble).toArray
      "Mean Precission = " + new Statistic(precissions).getMean + System.lineSeparator() +
      "Mean Recall = " + new Statistic(recalls).getMean + System.lineSeparator() +
      "Mean Non Interpolated average precission = " + new Statistic(NIAPs).getMean + System.lineSeparator() +
      "-------------" + System.lineSeparator() +
      text
    }
  }

  def searchToText(tf: coba.TF, idf: Boolean, normalization: Boolean, stemmer : Boolean,
             query: String) : String = {
    search(tf,idf,normalization,stemmer,query).foldLeft("")((z,t) => {
      val (name,similarity) = t
      z + name + " title:" + no2title(name) + " similarity:" + similarity + System.lineSeparator()
    })
  }

  //Calculate the TF-IDF of a term in document(word_map)
  def TF_IDF(term:String,word_map:Map[String,Int], tfKind:coba.TF, idf:Boolean): Double = {
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

  sealed trait FeedbackType;
  case class roccio() extends FeedbackType;
  case class ide_regular() extends FeedbackType;
  case class ide_dec_hi() extends FeedbackType;
  def main(args: Array[String]) = time {
    import java.io.PrintWriter

    val doc_location = "D:\\tugas\\Scala_for_Tf_Based_Information_retrieval\\ADI\\doc"
    val query_location = "D:\\tugas\\Scala_for_Tf_Based_Information_retrieval\\ADI\\query"
    val relevance_location = "D:\\tugas\\Scala_for_Tf_Based_Information_retrieval\\ADI\\relevance"
    val stop_word_location = "D:\\tugas\\Scala_for_Tf_Based_Information_retrieval\\stop_word.txt"

    val experiment_normal = true
    val experiment_pseudo_feedback = true
    val experiment_relevance_feedback = true

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

    val list_feedback = Seq(roccio(),ide_regular(),ide_dec_hi())
    val name_feedback = Seq("Roccio","Ide Regular","Ide Dec Hi")
    val all_feedback = list_feedback.zip(name_feedback)

    val list_search_seen = Seq(true,false)
    val name_search_seen = Seq("Iya","Tidak")
    val all_search_seen = list_search_seen.zip(name_search_seen)

    val list_top_n = Seq(5,10,20)
    val name_top_n = list_top_n.map(_.toString)
    val all_top_n = list_top_n.zip(name_top_n)

    val index_configuration = for {
      t <- all_TF;
      i <- all_IDF;
      s <- all_Stem;
      n <- all_normal
    } yield (t,i,s,n)

    val query_configuration = for {
      t <- all_TF;
      i <- all_IDF;
      n <- all_normal
    } yield (t,i,n)

    val pseudo_feedback_configuration = for {
      f <- all_feedback
      top_n <- all_top_n
    } yield (f,top_n)

    val relevance_feedback_configuration = for {
      f <- all_feedback
      top_n <- all_top_n
      seen <- all_search_seen
    } yield (f,top_n,seen)

    println("Done listing configuration")

    if (experiment_normal) time {
      val all_result = index_configuration.flatMap( conf1 => {
        val search_engine = new coba;
        time(search_engine.create_index(conf1._1._1,conf1._2._1,conf1._4._1,conf1._3._1,stop_word_location,doc_location))
        time(query_configuration.map(conf2 => {
          val res = search_engine.experiment(conf2._1._1,conf2._2._1,conf2._3._1,conf1._3._1,query_location,relevance_location)
          val stat_precission = new Statistic(res.map(t => t._2.precission).map(_.toDouble).toArray)
          val stat_recall = new Statistic(res.map(t => t._2.recall).map(_.toDouble).toArray)
          val stat_non_interpolated = new Statistic(res.map(t => t._2.interpolated_precission).map(_.toDouble).toArray)
          (conf1,conf2,stat_precission,stat_recall,stat_non_interpolated)
        }))
      })

      println("Done listing all result")
      val string = new StringBuilder
      all_result.map { case (conf_index, conf_query, stat_precission, stat_recall, stat_non_interpolated) =>
        (conf_index,conf_query,stat_non_interpolated.getMean)
      }.sortBy(_._3).reverse.map( t => {
        val (conf_index,conf_query,non_interpolated) = t
        string.append("Konfigurasi index" + System.lineSeparator())
        string.append("TF = " + conf_index._1._2 + System.lineSeparator())
        string.append("IDF = " + conf_index._2._2 + System.lineSeparator())
        string.append("Stemmer = " + conf_index._3._2 + System.lineSeparator())
        string.append("Normalization = " + conf_index._4._2 + System.lineSeparator())
        string.append("------------" + System.lineSeparator())
        string.append("Konfigurasi query" + System.lineSeparator())
        string.append("TF = " + conf_query._1._2 + System.lineSeparator())
        string.append("IDF = " + conf_query._2._2 + System.lineSeparator())
        string.append("Stemmer = " + conf_index._3._2 + System.lineSeparator())
        string.append("Normalization = " + conf_query._3._2 + System.lineSeparator())
        string.append("------------" + System.lineSeparator())
        string.append("Non Interpolated Precission" + System.lineSeparator())
        string.append("Mean = " + non_interpolated + System.lineSeparator())
        string.append("------------END-----------------" + System.lineSeparator())
      })

      val writer = new PrintWriter("result_normal.txt", "UTF-8");
      writer.println(string.toString());
      writer.close();
    }

    if (experiment_pseudo_feedback) time {
      val all_result = index_configuration.flatMap( conf1 => {
        val search_engine = new coba;
        println(conf1)
        time(search_engine.create_index(conf1._1._1,conf1._2._1,conf1._4._1,conf1._3._1,stop_word_location,doc_location))
        time(query_configuration.flatMap(conf2 => time(pseudo_feedback_configuration.map (conf3 => {
          val res = search_engine.experimentPseudoFeedback(conf2._1._1,conf2._2._1,conf2._3._1,conf1._3._1,query_location,
            relevance_location,conf3._2._1,conf3._1._1)
          val stat_precission = new Statistic(res.map(t => t._2.precission).map(_.toDouble).toArray)
          val stat_recall = new Statistic(res.map(t => t._2.recall).map(_.toDouble).toArray)
          val stat_non_interpolated = new Statistic(res.map(t => t._2.interpolated_precission).map(_.toDouble).toArray)
          (conf1,conf2,stat_precission,stat_recall,stat_non_interpolated,conf3)
        }))))
      })

      println("Done listing all result")
      val string = new StringBuilder
      all_result.map { case (conf_index, conf_query, stat_precission, stat_recall, stat_non_interpolated,conf_pseudo) =>
        (conf_index,conf_query,stat_non_interpolated.getMean,conf_pseudo)
      }.sortBy(_._3).reverse.map( t => {
        val (conf_index,conf_query,stat_non_interpolated,conf_pseudo) = t
        string.append("Konfigurasi index" + System.lineSeparator())
        string.append("TF = " + conf_index._1._2 + System.lineSeparator())
        string.append("IDF = " + conf_index._2._2 + System.lineSeparator())
        string.append("Stemmer = " + conf_index._3._2 + System.lineSeparator())
        string.append("Normalization = " + conf_index._4._2 + System.lineSeparator())
        string.append("------------" + System.lineSeparator())
        string.append("Konfigurasi query" + System.lineSeparator())
        string.append("TF = " + conf_query._1._2 + System.lineSeparator())
        string.append("IDF = " + conf_query._2._2 + System.lineSeparator())
        string.append("Stemmer = " + conf_index._3._2 + System.lineSeparator())
        string.append("Normalization = " + conf_query._3._2 + System.lineSeparator())
        string.append("------------" + System.lineSeparator())
        string.append("Konfigurasi pseudo feedback" + System.lineSeparator())
        string.append("Feedback = " + conf_pseudo._1._2 + System.lineSeparator())
        string.append("Top N = " + conf_pseudo._2._2 + System.lineSeparator())
        string.append("------------" + System.lineSeparator())
        string.append("Non Interpolated Precission" + System.lineSeparator())
        string.append("Mean = " + stat_non_interpolated + System.lineSeparator())
        string.append("------------END-----------------" + System.lineSeparator())
      })

      val writer = new PrintWriter("result_pseudo.txt", "UTF-8");
      writer.println(string.toString());
      writer.close();
    }

    if (experiment_relevance_feedback) time {
      val all_result = index_configuration.flatMap( conf1 => {
        val search_engine = new coba;
        println(conf1)
        time(search_engine.create_index(conf1._1._1,conf1._2._1,conf1._4._1,conf1._3._1,stop_word_location,doc_location))
        time(query_configuration.flatMap(conf2 => time(relevance_feedback_configuration.map (conf3 => {
          val res = search_engine.experimentRelevanceFeedback(conf2._1._1,conf2._2._1,conf2._3._1,conf1._3._1,query_location,
            relevance_location,conf3._2._1,conf3._1._1,conf3._3._1)
          val stat_precission = new Statistic(res.map(t => t._2.precission).map(_.toDouble).toArray)
          val stat_recall = new Statistic(res.map(t => t._2.recall).map(_.toDouble).toArray)
          val stat_non_interpolated = new Statistic(res.map(t => t._2.interpolated_precission).map(_.toDouble).toArray)
          (conf1,conf2,stat_precission,stat_recall,stat_non_interpolated,conf3)
        }))))
      })

      println("Done listing all result")
      val string = new StringBuilder
      all_result.map { case (conf_index,conf_query,stat_precission,stat_recall,stat_non_interpolated,conf_pseudo) =>
        (conf_index,conf_query,stat_non_interpolated.getMean,conf_pseudo)
      }.sortBy(_._3).reverse.map( t => {
        val (conf_index,conf_query,stat_non_interpolated,conf_pseudo) = t
        string.append("Konfigurasi index" + System.lineSeparator())
        string.append("TF = " + conf_index._1._2 + System.lineSeparator())
        string.append("IDF = " + conf_index._2._2 + System.lineSeparator())
        string.append("Stemmer = " + conf_index._3._2 + System.lineSeparator())
        string.append("Normalization = " + conf_index._4._2 + System.lineSeparator())
        string.append("------------" + System.lineSeparator())
        string.append("Konfigurasi query" + System.lineSeparator())
        string.append("TF = " + conf_query._1._2 + System.lineSeparator())
        string.append("IDF = " + conf_query._2._2 + System.lineSeparator())
        string.append("Stemmer = " + conf_index._3._2 + System.lineSeparator())
        string.append("Normalization = " + conf_query._3._2 + System.lineSeparator())
        string.append("------------" + System.lineSeparator())
        string.append("Konfigurasi relevance feedback" + System.lineSeparator())
        string.append("Feedback = " + conf_pseudo._1._2 + System.lineSeparator())
        string.append("Top N = " + conf_pseudo._2._2 + System.lineSeparator())
        string.append("Search Seen = " + conf_pseudo._3._2 + System.lineSeparator())
        string.append("------------" + System.lineSeparator())
        string.append("Non Interpolated Precission" + System.lineSeparator())
        string.append("Mean = " + stat_non_interpolated + System.lineSeparator())
        string.append("------------END-----------------" + System.lineSeparator())
      })

      val writer = new PrintWriter("result_relevance_feedback.txt", "UTF-8");
      writer.println(string.toString());
      writer.close();
    }
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  def javaSet2scalaSet[R] (java_set: java.util.Set[R]): Set[R] = {
    import scala.collection.JavaConversions._
    val set: scala.collection.mutable.Set[R] = java_set
    set.toSet
  }

  def stripSimilarity (results: Seq[(String,Float)]): Seq[String] = {
    return results.map(_._1)
  }
}
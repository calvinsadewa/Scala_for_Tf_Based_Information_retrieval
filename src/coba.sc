import java.io.PrintWriter

import coba._

val doc_location = "D:\\tugas\\STBI\\ADI\\doc"
val query_location = "D:\\tugas\\STBI\\ADI\\query"
val relevance_location = "D:\\tugas\\STBI\\ADI\\relevance"
val stop_word_location = "D:\\tugas\\STBI\\stop_word.txt"

val list_TF = Seq(new noTF(),new binaryTF(), new augmentedTF(), new logisticTF(), new rawTF())
val name_TF = Seq("No TF","Binary TF","Augmented TF","Logistic TF","Raw TF")
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
  println(conf1.toString())
  val search_engine = new coba;
  search_engine.create_index(conf1._1._1,conf1._2._1,conf1._4._1,conf1._3._1,stop_word_location,doc_location)
  all_configuration.map(conf2 => {
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

println(string)
val writer = new PrintWriter("result.txt", "UTF-8");
writer.println(string.toString());
writer.close();
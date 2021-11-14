import scala.io.Source
import scala.collection._
import java.util.ArrayList
import scala.collection.mutable.ListMap
import scala.collection.mutable.ListBuffer
import java.io._
import math._
import java.nio.charset.CodingErrorAction
import io.Codec

  class Generalized(var documents_freq_map: mutable.Map[String, mutable.Map[String, Int]], var TFIDF: mutable.Map[String, mutable.Map[String, Double]]){

    def TF_IDF(doc: String, word: String, sum: Double): Double ={
        var n: Double = 0.0 //number of occurences of a word across all documents
        var n_doc: Double = 0.0 //number of all documents

        for(i<-documents_freq_map.keys){ //loop over all documents
            if(documents_freq_map(i).contains(word)){ //if document contains the word we're looking for, add 1 to n
                n = n + 1
            }
            n_doc = n_doc + 1
        }

        var TF: Double = documents_freq_map(doc)(word) / sum //TF = occurences of "word" / sum
        var idf: Double = math.log10(n_doc / n)/ math.log10(2.0) 
        var terms_freq_inv_doc: Double = TF * idf //get TF.IDF

        return terms_freq_inv_doc
    }

    def Up_TFIDF(){
        var sum: Double = 0 //sum of all words in a single document

        for(i<-documents_freq_map.keys){ //loop over documents
            if(!TFIDF.contains(i)){ //if our map doesnt contain the document, add it
                TFIDF += (i -> mutable.Map[String, Double]()) //create key (document), value (empty map) for each document
            }

            else{ //if it already contains it, remove it and create new empty map
                TFIDF -= i
                TFIDF += (i -> mutable.Map[String, Double]())
            }

            sum = 0            
            for(k<-documents_freq_map(i).keys){ //sum of all words in the text
                sum = sum + documents_freq_map(i)(k)
            }

            for(j<-documents_freq_map(i).keys){ //loop over words in a document
                TFIDF(i) += (j -> TF_IDF(i, j, sum)) //create key(word), value(TF.IDF) pair for each word in the document
            }
        }
    }

    def up_map(doc: String, new_elements: mutable.ArrayBuffer[String]){
        if(!documents_freq_map.contains(doc)){ //if document doesnt exist in the map, add it with empty frequency map
            documents_freq_map += (doc -> mutable.Map[String, Int]())
        }

        for(i<-new_elements){ 
            if(!documents_freq_map(doc).contains(i)){ //if our map doesnt contain the word we add it to map with value 1
            documents_freq_map(doc) += (i -> 1)
        }
        else{
            documents_freq_map(doc)(i) = documents_freq_map(doc)(i) + 1 // map cointains the word - increase value by 1
        }
      }

      documents_freq_map(doc) = ListMap(documents_freq_map(doc).toSeq.sortWith(_._2 < _._2):_*) //sorting the map by value
    }
// =============================================================Reading========================================================
    def read_file(file_name: String){
      var dec = Codec.UTF8.decoder.onMalformedInput(CodingErrorAction.IGNORE)
      var lines = Source.fromFile(file_name)(dec).getLines.toList //getting text from file
      var prunned = prunning(lines)
      up_map(file_name, prunned)
    }


// =================================================Writing into console all documents=======================================
    def write_console_all(item: Int){
        var s = 0
        var all: mutable.Map[String, Int] = mutable.Map()

        for(i<-documents_freq_map.keys){ //loop over all files
            for(j<-documents_freq_map(i).keys){ //loop over all words in a file
                if(!all.contains(j)){ //if map doesnt contain the word, create new entry
                    all += (j -> documents_freq_map(i)(j))
                }
                else{ //otherwise increase the value by the value in current document
                    all(j) = all(j) + documents_freq_map(i)(j)
                }
            }
        }

        all = ListMap(all.toSeq.sortWith(_._2 < _._2):_*) //sorting by second
        for(i<-all.keys){
            if(s<item){
                println("Word: " + i + " => occurences: " + all(i)) //most frequent word
                s = s + 1
            }
        }

    }

// ===================================================Writing into console separate documents=======================================
    def write_console_each(item: Int){
      var s = 0

      for(i<-documents_freq_map.keys){ //loop over all files
        println("Document: " + i)
        s=0

        for(j<-documents_freq_map(i).keys){ //loop over all words in a single file
            if(s<item){
                println("\t" + "Word: " + j + " => occurences: " + documents_freq_map(i)(j)) //print occurences of a word
                s = s + 1
            }
        }
      }
    }

// =============================================Writing TF.IDF==============================================================
    def write_IDF(item: Int){
        var s = 0

        for(i<-TFIDF.keys){ //loop over all files
            TFIDF(i) = ListMap(TFIDF(i).toSeq.sortWith(_._2 < _._2):_*) //sorting the map

            println("Document: " + i)
            s=0

        for(j<-TFIDF(i).keys){ //loop over all words in a single file
            if(s<item){
                println("\t" + "Word: " + j + " -> TF.IDF: " + TFIDF(i)(j)) //print occurences of a word
                s = s + 1
            }
        }
      }
    }

//=======================================================Saving==============================================================
    def out_file(item: Int, file_name: String){
      var s = 0
      val pw = new FileWriter(new File(file_name + ".csv")) //saving to file

      for(i<-documents_freq_map.keys){ //loop over all documents
        pw.write("Document : " + i + "\n")
        s=0

        for(j<-documents_freq_map(i).keys){ //loop over all words in a single document
            if(s<item){
                pw.write(j + "," + documents_freq_map(i)(j) + "\n")
                s = s + 1
            }
        }
      }
      pw.close()
    }
// =======================================Deleting punctation===============================================================
    def prunning(st_array: List[String]): mutable.ArrayBuffer[String] ={
      var newLines: mutable.ArrayBuffer[String] = mutable.ArrayBuffer() //empty list that works like in python

      for(i<-st_array){
        newLines.append(i.replaceAll("""[\p{Punct}]"""," ").replaceAll("’"," ").replaceAll("”"," ").replaceAll(" s ", " ")) //removing punctuation, the weird ’ and single "s"
      }

      var result = newLines.flatMap(x=>x.split("\\s+").map(x=>x.trim)).map(x=>x.toLowerCase).toList //removing uppercase and whitespaces

      var l1: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()

      for(word<- result){ //removing all words of len<3
        if(word.length > 2){
          l1.append(word)
        }
      }

      var stop = Source.fromFile("Stop_words.txt").getLines.toList.toSet //stopwords
      var l2 = l1.filterNot(stop) //removing stopwords
      return l2 //return prunned list of words
    }
  }
object Test{

  def main(args: Array[String]){
    var map1: mutable.Map[String, mutable.Map[String, Int]] = mutable.Map()
    var map2: mutable.Map[String, mutable.Map[String, Double]] = mutable.Map()
    val freq1 = new Generalized(map1, map2)
    var i = 1
    freq1.read_file("01-The-Fellowship-Of-The-Ring.txt")
    freq1.read_file("Ancient-and-Modern-P-Thomas-E--Wills-[ebooksread.com].txt")
//    freq1.read_file("In-the-Court-of-King-Samuel-E--Lowe-[ebooksread.com].txt")
//    freq1.read_file("My-Queen--A-Weekly-J-Lurana-Waterhou-[ebooksread.com].txt")
//    freq1.read_file("Romeo-and-Juliet-William-Shakesp-[ebooksread.com].txt")
//    freq1.read_file("Secret-Adversary-Agatha-Christie-[ebooksread.com].txt")
//    freq1.read_file("The-Bible--Old-and-N-Unknown-[ebooksread.com].txt")
//    freq1.read_file("The-elements-of-coor-S--L---Sidney-L-[ebooksread.com].txt")
//    freq1.read_file("The-Philosophy-of-Mi-P--J--Proudhon-[ebooksread.com].txt")
//    freq1.read_file("The-Story-of-the-Liv-H-W--Conn-[ebooksread.com].txt")
    freq1.write_console_each(20)
    freq1.Up_TFIDF()
    freq1.write_IDF(20)
    freq1.write_console_all(30)
  }


}

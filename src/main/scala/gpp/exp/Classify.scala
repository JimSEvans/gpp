package gpp.exp

import chalk._
import tshrdlu.util._
import sys.process._


object TweetClassify {

  import nak.NakContext._
  import nak.core._
  import nak.data._
  import nak.liblinear.LiblinearConfig
  import nak.util.ConfusionMatrix

  def main(args: Array[String]) {

    val opts = ClassifyOpts(args)
    
    if (opts.version() == true) println("Version 0.1")

    if (opts.evalfile().length != 0 && opts.trainfile().length!=0) {

    lazy val (trainGold, trainItems) = FileReader(opts.trainfile())

    val (evalGold, evalItems) = FileReader(opts.evalfile())

    lazy val cost = opts.cost()

    lazy val extended = opts.extended()

    val predicted = {
        if (opts.method() == "majority") MajorityClassifier(trainGold, evalItems)
        else if (opts.method() == "lexicon") LexiconClassifier(evalItems)
        else if (opts.method() == "L2R_LR") NakClassifier(trainGold, trainItems, evalGold, evalItems, cost, extended)
        else MajorityClassifier(trainGold, evalItems)

    }

    println(ConfusionMatrix(evalGold, predicted, evalItems))

    if (opts.detailed()==true) println(ConfusionMatrix(evalGold, predicted, evalItems).detailedOutput)

    } //if train&&test


    else if (opts.evalfile().length != 0 && opts.method()=="lexicon") {

    val (evalGold, evalItems) = FileReader(opts.evalfile())

    val predicted = LexiconClassifier(evalItems)


    println(ConfusionMatrix(evalGold, predicted, evalItems))
    if (opts.detailed()==true) println(ConfusionMatrix(evalGold, predicted, evalItems).detailedOutput)


    } //if train but no test

    else if (opts.verbose()==false && opts.help()==false) println("Use --verbose or --help for information")
    else if (opts.verbose()==true) println("There's a problem with the training and/or evaluation file arguments. For all methods, you need to give an evaluation file. You must also give a training file for all methods except \"lexicon\".")

  } //main

} //object














object FileReader {

    val LabelRE = """(positive|negative|neutral)""".r

    def isGoodLabel (token:String):Boolean = token match {
        case LabelRE(token) => true
        case _ => false
    }

    def processFile(filename:String):Tuple2[Vector[String],Vector[String]] = {
        
        val xml = scala.xml.XML.loadFile(filename)
        val itemsXml = xml \ "item"
        val goldUnf:Vector[String] = itemsXml.map(_ \ "@label").map(_.text).toVector
        val itemsUnf = itemsXml.map(_ \ "content").map(_.text).toVector
        val (gold, items) = goldUnf.zip(itemsUnf).filter(x => isGoodLabel(x._1)).unzip
        (gold, items)

    }

    def combineTuples(tupList:List[Tuple2[Vector[String],Vector[String]]]):Tuple2[Vector[String],Vector[String]] = {
        val (goldList,itemsList) = tupList.unzip
        val gold = goldList.flatten.toVector
        val items = itemsList.flatten.toVector
        (gold, items)
    }

    def apply(fileList:List[String]):Tuple2[Vector[String],Vector[String]] = {
        combineTuples(fileList.map(processFile))

    }

}









object StanfordConverter {

    val getLabel = Map("0" -> "negative", "2" -> "neutral", "4" -> "positive")


    def arrsToXML(iter:Iterator[Array[String]]):Unit = {
        val foo = for (arr<-iter) yield {
            "<item label=\""+getLabel(arr(0))+"\"> <content>"+arr(5)+"</content> </item>"
        }
        println("<dataset>")
        foo.foreach(println)
        println("</dataset>")
    }


    def main(args: Array[String]):Unit = {
        val linesArrs = scala.io.Source.fromFile(args(0)).getLines.map(_.split(";;"))
        arrsToXML(linesArrs)
    }
}





object EmoticonConverter {


    def main(args: Array[String]):Unit = {
        
        val dirname = args(0)

        val toDrop = dirname.length+1

        val cmd = "ls " + dirname

        val filesString = cmd !!

        val files:Array[String] = filesString.split("\\s").filter(_.endsWith(".txt")).map(dirname+"/"+_)

        def getEmotionOfFile(filename:String):String = filename.drop(toDrop).dropRight(4)

        val getLabel = Map("sad" -> "negative", "neutral" -> "neutral", "happy" -> "positive")

        val EmoLineRE = """\d+\s+\d+\s+(\S.+)""".r

        val AmpRE = """(&)""".r

        def splitEmoLine (line:String,label:String):Tuple2[String,String] = line match {
            case EmoLineRE(item) => (label, AmpRE.replaceAllIn(item, "&amp;"))
            case _ => ("ERROR", "ERROR")
        }

        def fileToVectorOfTuples (filename:String):Vector[Tuple2[String,String]] = {
            val label = getLabel(getEmotionOfFile(filename))
            val linesVec:Vector[String] = scala.io.Source.fromFile(filename).getLines.toVector
            val labelItemTuplVec:Vector[Tuple2[String,String]] = linesVec.map(x=>splitEmoLine(x,label))
            labelItemTuplVec
            // Vector(("h","i"))
        }

        val allFilesVec:Vector[(String,String)] = files.map(fileToVectorOfTuples).toVector.flatten


        def tuplesToXML(vec:Vector[(String,String)]):Unit = {
            val foo = for (tupl<-vec) yield {
                "<item label=\""+tupl._1+"\"> <content>"+tupl._2+"</content> </item>"
            }

            println("<dataset>")
            foo.foreach(println)
            println("</dataset>")
        }

        tuplesToXML(allFilesVec)

    }

}






object MajorityClassifier {
    def apply (goldList:Vector[String], testItems:Vector[String]):Vector[String] = {
        val uniques = goldList.toSet.toVector
        val rankedByFreq = uniques.map(unique => (goldList.filter(x=>x==unique).length, unique))
            .sorted.reverse
        val mode = rankedByFreq.head._2
        Array.fill(testItems.length)(mode).toVector
    }
}








object Polarity extends Polarity




object LexiconClassifier {

    val lower = "abcdefghijklmnopqrstuvwxyz"
    val upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    val big2little = upper.zip(lower).toMap

    
    def mayLow (letter: Char): Char = {
        if (upper.contains(letter)) big2little(letter)
        else letter
    }

    def mayLowWord(word:String) = {
        word.map(x=>mayLow(x)).mkString("")
        }

    def getTokPolarity(token:String):String = {
        if (Polarity.posWords(token)) "pos"
        else if (Polarity.negWords(token)) "neg"
        else "neutral"
    }

    def getVectPolarity(arr:Seq[String]):String = {
        val aMap = arr.groupBy(x=>x).mapValues(x=>x.length).withDefault(x => 0)
        if (aMap("pos") > aMap("neg")) "positive"
        else if (aMap("pos") < aMap("neg")) "negative"
        else "neutral"
    }


    def apply(items:Vector[String]) = {

        val tokensVect = items.map(chalk.lang.eng.Twokenize(_))
        val loweredTokVect = tokensVect.map(_.map(mayLowWord))
        val polarityVectToks = loweredTokVect.map(_.map(getTokPolarity))
        val polarityVect = polarityVectToks.map(getVectPolarity)

        polarityVect
    }
}











object NakClassifier {

  import nak.NakContext._
  import nak.core._
  import nak.data._
  import nak.liblinear.LiblinearConfig
  import chalk.lang.eng.PorterStemmer
  import LexiconClassifier._

  lazy val stemmer = new PorterStemmer

  def apply (trainLab: Seq[String], trainItems: Seq[String], evalLab: Seq[String], evalItems: Seq[String], costParam: Double, extended: Boolean) = {



    val rawTrain = trainLab.zip(trainItems)

    val rawEval = evalLab.zip(evalItems)

    val SmileRE = """(:-?[\)D]|\(-?:)""".r

    def getSmileys (item:String):Vector[String] = SmileRE.findAllIn(item).toVector

    val PuncRE = """([\.,\:;\)\(\)/])""".r

    def isNonPunc (token:String):Boolean = token match {
        case PuncRE(token) => false
        case _ => true
    }

    // def cleanUp (input: String):Array[String] = {
    //   val tokenized = chalk.lang.eng.Twokenize(input)
    //   val filtered = tokenized.filterNot(x=>English.stopwords(x))
    //       .filter(isNonPunc)
    //   filtered.toArray.map(LexiconClassifier.mayLowWord)
    // }

    val rawTrainExamples = rawTrain.map(x=>Example(x._1,x._2)).toList

    val rawEvalExamples = rawEval.map(x=>Example(x._1,x._2)).toList

// This will be very basic featurization: every non-punctuation token ultimately
// becomes a feature like "word="whatever""

    def getBaselineBOWfeatures(item:String):List[Tuple2[String,String]] = {
        chalk.lang.eng.Twokenize(item).filter(isNonPunc).map(("word",_)).toList
    }

// These functions are for extended features

    def getUnigramFeatures (tokenized:List[String]):List[Tuple2[String,String]] = {
        tokenized.filter(x=>English.stopwords(x)==false).map(("word",_)).toList
    }

    def getBigramFeatures (tokenized:List[String]):List[Tuple2[String,String]] = {
        tokenized.sliding(2).map(_.mkString("_")).map(("bigram",_)).toList
    }

    def getStemTrigrams (tokenized:List[String]):List[Tuple2[String,String]] = {
        tokenized.map(stemmer(_)).sliding(3).map(_.mkString("_")).map(("stem_tri",_)).toList
    }

    def getPolarityFeatures (tokenized:List[String]):List[Tuple2[String,String]] = {
        tokenized.map(getTokPolarity).map(("polarity",_)).toList
    }


    def getStemUnigrams (tokenized:List[String]):List[Tuple2[String,String]] = {
        tokenized.filter(x=>English.stopwords(x)==false).map(stemmer(_)).map(("stem",_)).toList
    }

    def getStemBigrams (tokenized:List[String]):List[Tuple2[String,String]] = {
        tokenized.filter(x=>English.stopwords(x)==false).map(stemmer(_)).sliding(2).map(_.mkString("_")).map(("stem_bi",_)).toList
    }

    def emoticons (input:String):List[Tuple2[String,String]] = {
        getSmileys(input).map(("smiley",_)).toList
    }

    // def getStemBigrams (tokenized:List[String]):List[Tuple2[String,String]] = {
    //     tokenized.filter(isNonPunc).map(mayLowWord).filter(x=>English.stopwords(x)==false).map(stemmer(_)).map(("stem_bi",_)).toList
    // }

// Make featurizer

    val featurizer = {
        if (extended == true) new Featurizer[String,String] {
      def apply(input: String) = {
        val tokenized = chalk.lang.eng.Twokenize(input).filter(isNonPunc).map(mayLowWord)
        val unigrams = getUnigramFeatures(tokenized)
        val bigrams = getBigramFeatures(tokenized)
        val stemTrigrams = getStemTrigrams(tokenized)
        val stems = getStemUnigrams(tokenized)
        val stemBigrams = getStemBigrams(tokenized)
        val polarity = getPolarityFeatures(tokenized)
        val smileys = emoticons(input)
        val allFeats = unigrams ++ stemTrigrams ++ stems ++ polarity ++ stemBigrams ++ smileys
        for ((attr,value) <- allFeats)
          yield FeatureObservation(attr+"="+value)
      }}
        else new Featurizer[String,String] {
      def apply(input: String) = {
        val feats = getBaselineBOWfeatures(input)
        for ((attr,value) <- feats)
          yield FeatureObservation(attr+"="+value)
      }}
      }

    // println(featurizer("Obama +2 for bringing up Spain fiasco #tweetdebate"))



    // Configure and train with liblinear. Here we use the (default) L2-Regularized 
    // Logistic Regression classifier with a C value of .5. We accept the default
    // eps and verbosity values.

    val config = LiblinearConfig(cost=costParam)
    val classifier = trainClassifier(config, featurizer, rawTrainExamples)

    // Partially apply the labels to the curried 2-arg NakContext.maxLabel function 
    // to create the 1-arg maxLabelPpa function to get the best label for each example.

    def maxLabelFunc = maxLabel(classifier.labels) _

    // Make predictions on the evaluation data. 

    val threes = for (ex <- rawEvalExamples) yield 
      (ex.label, maxLabelFunc(classifier.evalRaw(ex.features)), ex.features)

 
    val (goldLabels, predictions, inputs) = threes.unzip3

    predictions
  }
}











/**
 * An object that sets of the configuration for command-line options using
 * Scallop and returns the options, ready for use.
 */
object ClassifyOpts {

  import org.rogach.scallop._
  
  def apply(args: Array[String]) = new ScallopConf(args) {
    banner("""
Classification application.
         """)

    val methodTypes = Set("majority","lexicon","L2R_LR")
    val cost = opt[Double]("cost", default=Some(1.0), validate = (0<), descr="The cost parameter C. Bigger values means less regularization (more fidelity to the training set). Note: if you are using the GIS solver, this option instead indicates the standard deviation of the Gaussian penalty (bigger values still mean less regularization).")
    val detailed = opt[Boolean]("detailed")
    val evalfile = opt[List[String]]("eval", descr="The files containing evalualation events.")
    val extended = opt[Boolean]("extended", short='x', descr="Use extended features")
    val method = opt[String]("method", default=Some("L2R_LR"), validate = methodTypes, descr = "The type of solver to use. Possible values: " + methodTypes.toSeq.sorted.mkString(",") )
    val trainfile = opt[List[String]]("train", required=false,descr="The files containing training events.")
    val verbose = opt[Boolean]("verbose")
    val help = opt[Boolean]("help", noshort = true, descr = "Show this message")
    val version = opt[Boolean]("version", noshort=true, default=Some(false), descr = "Show version of this program")


  }
}


package wekaBased
import java.util

import base.LOG
import weka.classifiers._
import weka.classifiers.bayes.{BayesNet, NaiveBayes}
import weka.core.Instances
/**
  * Created by Asia on 2017/9/7.
  */
class myClassfier(insTrain:Instances,insTest:Instances) {
  val instancesTR=insTrain
  val instancesTE=insTest
  //val claf=new NaiveBayes()
  val clafN=new NaiveBayes()
  def classfy()={


    instancesTR.setClassIndex(12)
    instancesTE.setClassIndex(12)

    clafN.buildClassifier(instancesTR)
    val num=instancesTE.numInstances()
    var s=""
    for(i<-0 to 100){
        s+=instancesTE.instance(i).value(0)+","
        clafN.distributionForInstance(instancesTE.instance(i)).foreach(x=>s+=x+",")
      s+=instancesTE.instance(i).classValue()
        s+="\n"
    }//Of for

    val evaluation=new Evaluation(instancesTE)
    evaluation.evaluateModel(clafN,insTest)
    //println(util.Arrays.toString(clafN.distributionForInstance(insTest.instance(200))))
    println("Sum==>"+evaluation.toSummaryString)

    //println("Mat==>"+evaluation.toMatrixString)
    //println(util.Arrays.toString())
    println(s)
    //LOG.data("predict",s)
  }    //Of classfy
}//Of myClassfier

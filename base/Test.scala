package base
import java.io.{BufferedReader, File, FileWriter}
import java.util

import wekaBased._
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.plot._
import breeze.stats.distributions.Gaussian
import com.sun.prism.paint.Color
import weka.core.converters.ArffLoader
import weka.core.converters.ArffLoader.ArffReader
import wekaBased.FileChange

import scala.util.Random
/**
  * Created by Asia on 2017/9/3.
  */
class Test {
  val basePath="F:\\competition\\020初赛数据\\020初赛数据\\processed\\"
  var array=new Array[util.ArrayList[String]](4)
  val myMatrix=new Array[MyMatrix](6)
  val basePath1="F:\\competition\\020初赛数据\\"
  val off_train="ccf_offline_stage1_train.csv"
  val on_train="ccf_online_stage1_train.csv"
  val test="ccf_offline_stage1_test_revised.csv"
  val newFile=basePath1+"submit3.csv"
  //val filew=new FileWriter(basePath1+"on_tran.arff",true)
  for(i<-0 to 3){
    array(i)=new util.ArrayList[String]()
  }//Of for
  def testBrezz()={
    //val f=Figure()
    //var p=f.subplot(0)
    val fileWriter=new FileWriter(new File(newFile))
    val data=new util.ArrayList[Double]()
    var file=new FileLoad(LOG.path+"predict")
    var buy=0
    var not=0
    file.process(x=>{
        val v=x.split(",")
        var value=v(2).toDouble
        if(value<0.999&&value>0.9){
          value=value*0.6
        } //Of if
        else if(value<0.0001){
          value=value*500
          if(value>1)value=0.9
        }//Of if
        data.add(value)
    })
    /*p+=hist((0 to data.size()-1).map(x=>data.get(x)),bins = 500)
    f.refresh()*/
    file=new FileLoad(basePath1+test)
    val dataL=data.size()
    var i=0
    file.process(x=>{
      val v=x.split(",")
      var str=""
      str+=v(0)+","+v(2)+","+v(5)+","+data.get(i).formatted("%.5f")+"\n"
      i+=1
      fileWriter.write(str)
    })
    fileWriter.close()
  }//Of testBrezz
  def frequence(stream: BufferedReader): Array[MyMatrix]={
    var value:Array[String]=null;
    for(i<-0 to array.length-2) {
      array(i) = new util.ArrayList[String]()
      myMatrix(i)=new MyMatrix()
    }//Of for
    array(6) = new util.ArrayList[String]()
    var temp=stream.readLine()
    while(temp!=null){
      value=temp.split(",")
      for(i<-0 to value.length-1){
        if(!array(i).contains(value(i)))
        array(i).add(value(i))
      }//Of for i
      for (i<-0 to value.length-2){
        myMatrix(i).add(array(i).indexOf(value(i)),array(i).indexOf(value(6)),1)
      }//Of for
    }//Of while
    stream.close();
    myMatrix
  }//Of frequenc
  def plotImage(myMatrix: Array[MyMatrix])={
    val f=Figure()
    var p=new Array[Plot](6)
    for(i<-0 to 5) {
      p(i) = f.subplot(i)
      p(i)+=image(myMatrix(i).toMatrix)
    }//of ofr
    f.refresh()
  }//Of plot
  def TeseWekaLoad()={
    val instances=new FileChange().changeFile()

  }//Of TestWekaLoad
  def promethod(string: String): Unit ={
    //filew.write(string+"\n")
  }//Of String
  def testChangeFile(): Unit ={
    new FileChange().changeFile()

    //filew.close()
  }//Of def
  def testBayes(): Unit ={
    var trainIN=new ArffLoader()
    trainIN.setSource(new File(LOG.path+"new1_TRAIN.arff"))
    var testIn=new ArffLoader()
    testIn.setSource(new File(LOG.path+"xxx.arff"))
    val cla=new myClassfier(trainIN.getDataSet(),testIn.getDataSet())
    cla.classfy()
  }//Of testBayes





  /*def test(count1:Int,count2:Int): Unit ={
      val arr1=new Array[Int](count1)
      val arr2=new Array[Int](count2)
      for(j<-count1) {
        for (i <- 1 to 9) {
        } //Of for
      }//Of for j

  }//Of test*/
}//Of class Test

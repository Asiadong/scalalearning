import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

import scala.util.control.Breaks

/**
  * Created by Asia on 2018/6/6.
  */
object K_means {
  val maxIter=100
  def main(args: Array[String]) = {
    // 屏蔽日志
    //Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
    //Logger.getLogger("org.apache.jetty.server").setLevel(Level.OFF)
    // set ENV
    val conf = new SparkConf()
    val sc = new SparkContext(conf)
    val data = sc.textFile("", 1).map(x=> x.split(",").map(x=>x.toDouble))
    var centers: Array[(Int,Array[Double])] = null
    val thers=100;



    Breaks.breakable {
      for (i <- 1 to maxIter) {
        val point = data.map(x => (getClosest(centers, x), (x, 1)))
        val newCenters = point.reduceByKey { case ((sum1, count1), (sum2, count2)) => (addArr(sum1, sum2), count1 + count2) }.map { case (id, (sum, count)) => (id, getMean(sum, count)) }.collect
        var tempThers = 0d
        for ((id, center) <- newCenters) {
          tempThers = disArr(centers(id)._2, center)
        } //Of for
        if (tempThers < thers) {
          point.saveAsTextFile("")
          print("totalTime:")
          Breaks.break()
        }
      }
    }

  }





  def getSum(pre:Array[Double],post:Array[Double]):Array[Double]={
    var result=new Array[Double](pre.length)
    for ( i<-0 to pre.length-1){
      result(i)=pre(i)+post(i)
    }
    result
  }
  def getMean(sum:Array[Double],count:Int):Array[Double]={
    sum.map(x=>x/count)
  }


  /**
    * get the id of the closest data
    * @param centers
    * @param data
    */
  def getClosest(centers:Array[(Int,Array[Double])],data:Array[Double]):Int={
    var closest=9999999d
    var temp=0.0
    var id=0
      for(i<-0 to centers.length-1){
        for(j<-centers(i)._2.length-1){
          temp+=math.pow(centers(i)._2(j)-data(j),2)
        }//Of for j
        if(closest>temp) {
          closest = temp
          id=i
        }

      }//Of for i
     id
  }

  def addArr(arr1:Array[Double],arr2:Array[Double])={
    if (arr1.length!=arr2.length) null
    else {
      var result=new Array[Double](arr1.length)
      for(i<-0 to arr1.length-1){
        result(i)=arr1(i)+arr2(i)
      }//Of for
      result
    }
  }
  def disArr(arr1:Array[Double],arr2:Array[Double]):Double={
    if (arr1.length!=arr2.length) null
    else {
      var result=0d
      for(i<-0 to arr1.length-1){
        result+=math.pow(arr1(i)-arr2(i),2)
      }//Of for
      result
    }
  }

}

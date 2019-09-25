package base

import java.util

import breeze.linalg.{DenseMatrix, Matrix}

/**
  * Created by Asia on 2017/9/3.
  * this is used to express the data
  */
class MyMatrix {
  var rows=0;
  var lines=0;
  val data=new util.ArrayList[Array[Int]]()
  def add(row: Int,line:Int,value:Int): Unit ={
    if(row>rows){
      rows+=1
    }//Of if
    if(line>lines){
      lines+=1
    }//Of if
    data.add(Array[Int](row,line,value))
  }//Of add
  def toMatrix:Matrix[Double]={
    val result=Matrix.zeros[Double](rows,lines)
    val iterator=data.iterator()
    while (iterator.hasNext){
      val temp=iterator.next()
      result(temp(0),temp(1))+=temp(2)*1d
    }//Of while
    result
  }//Of toMatrix
}//Of MyMatrix

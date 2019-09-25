package nn

import scala.util.Random
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object NN {
  var learn = 0.4;
  var work: Array[Layer] = null;
  class Node(length: Int,is_inputNode:Boolean) {
    var weightArray: Array[Double] =initialWeight(_ =>1d, length); //Save the weight matrix;
    var thelta = 0d;
    var input: Array[Double] = null;
    var output = 0d;
    var error = 0d;
    var expet:Array[Double]=null;
    if(!is_inputNode){
     weightArray=initialWeight(_ => Random.nextDouble() * 2 - 1, length); //Save the weight matrix
     thelta= Random.nextDouble(); //the theta
    }//Of if
    /**
     * The update node
     */
    def update() {
//      println(">>>>>>>>this learn,error,input:"+learn+","+error+","+input(0));
      for (i <- 0 to input.length - 1) {
        weightArray(i) += learn * error * output*input(i);
      } //Of for
      thelta += learn * error;
    } //Of update in Node
    def sig(x: Double): Double = 1 / (1 + scala.math.pow(scala.math.E, -x));
    def initialWeight(f: Unit => Double, length: Int): Array[Double] = {
      var s = new Array[Double](length);
      s.map(x => f());
    } //Of weight
    def computeN(input: Array[Double]): Double = {
//      print("nodeCompute");
      var s: Double = 0;
      var i: Int = 0;
      this.input = input.clone();
      while (i < input.length) {
        s += input(i) * weightArray(i);
        i += 1;
      } //Of while
      output = sig(s + thelta);
//      println("input"+input)
      output;
    } //Of compute
    override def toString={
      var re="[";
      for(weight<-weightArray){
        re+=weight.formatted("%.4f")+",";
      }//Of for
      re.stripSuffix(",")+"]";
    }//Of to string
  } //Of class Node
  class Layer(num: Int, preCount: Int = 1,is_input:Boolean=false) {
    var layer = build(num);
    var nextLayer: Layer = null;
    def build(num: Int): Array[Node] = {
      var i = 0;
      var nodes = new Array[Node](num);
      while (i < num) {
        nodes(i) = new Node(preCount,is_input);
        i += 1;
      } //Of while
      nodes;
    } //Of build
    override def toString():String={
      var re:String="this layer:\n";
      for(node<-layer){
        re+=node.toString()+"\t";
      }//Of for
      re;
    }//Of toString
    def compute(input: Array[Double]): Array[Double] = {
//      print("layerCompute"+input.length)
      var re = new Array[Double](layer.length);
      for (i <- 0 to layer.length - 1) {
//        println("node"+i);
        re(i) = layer(i).computeN(input);
      } //Of for
//            println("layerComputeEnd")
      re;
    } //Of compute
    def computeInputLayer(in:Array[Double]):Array[Double]={
      var re = new Array[Double](in.length);
      for(i <- 0 to in.length - 1) {
        re(i) = in(i);
       // println(i)
        layer(i).output=in(i);
      } //Of for
      re;
    }//Of computeInputLayer
    /**
     * compute the error of each hidden layer
     */
    def err(nextL: Layer, preErr: Array[Double]): Array[Double] = {
      var errors = new Array[Double](layer.length);
      nextLayer = nextL;
      for (i <- 0 to layer.length - 1) {
        errors(i) = layer(i).output * (1 - layer(i).output) * sum(i, 0, preErr);
//        println("<<<<<<<<<<<<<node errors:"+errors(i));
        layer(i).error=errors(i);
      } //Of for
      errors;
    } //Of err
    def outErr(Expect: Array[Double]): Array[Double] = {
      var i = 0;
      var result: Array[Double] = new Array[Double](Expect.length);
      for (node <- layer) {
        node.error = node.output * (1 - node.output) * (Expect(i)-node.output);
        result(i) = node.error;
        i += 1;
      } //Of for
      result;
    } //Of outErr
    def sum(i: Int, k: Int, errk: Array[Double]): Double = {
      if (errk.length == 0) 0
      else errk.head * ((nextLayer.layer)(k).weightArray(i)) + sum(i, k + 1, errk.tail);
    } //Of 
  } //Of class Layer
  override def toString():String={
    var re:String="";
    for(layer<-work){
      re+=layer.toString()+"\n";
    }//Of for
    re;
  }//Of toString
  
  def build(structure: Array[Int]) = {
    val layerCount = structure.length;
    var work: Array[Layer] = new Array[Layer](layerCount);
    work(0) = new Layer(structure(0),1,true);
    for (i <- 1 to layerCount - 1) {
      work(i) = new Layer(structure(i), structure(i - 1));
    } //Of for
    this.work = work;
  } //Of build
  def push(input: Array[Double]): Array[Double] = {
//     print("push")
    var out=work(0).computeInputLayer(input);
    for (i <- 1 to work.length-1) {
      out = work(i).compute(out);
    } //Of for
    out;
  } //Of push
  def back(out: Array[Double]) = {
    val layerCount = work.length;
    var errLay = work(layerCount - 1).outErr(out);
    var j=0;
    for (i <- 2 to layerCount - 1 ) {
      j= layerCount-i;
      errLay = work(j).err(work(j + 1), errLay);
    } //Of back
    for(j<- 1 to layerCount-1){
//      println(j);
      update(work(j));
    }//Of for
  } //Of back
  def update(paraLayer: Layer) = {
    for (node <- paraLayer.layer) {
      node.update();
    } //Of for
  } //Of update
  def train(input: Array[Double], times: Int = 0,valid:Boolean=false) = {
    val result=input.last;
    val data=input.init;
//    data.foreach(println)
    if(valid){
      push(data);
      println("the total error is :"+totalError(getExpect(result)));
    }//Of if
    if (times != 0) {
      for (i <- 1 to times) {
        push(data);
        back(getExpect(result));   
      } //Of for
    } //Of if
  } //Of run
  def test(input: Array[Double]):Boolean={
    val result=input.last.toInt;
    val data=input.init;
    var re=push(data);
    judge2(re,getExpect(result));
  }//Of test

  def test2(input: Array[Double]):Boolean={
   /* var dou=0d;
    var re=push(input);
    var red=0d;
    var exp=0d;
    for(i<-0 to re.length-1){
      if(re(i)>0.5){
        red+=math.pow(2,i)
      } //Of if
    }//Of for
    println(red)
      if(red<=15){
        dou=(-1.0)/2250*red*red+1
      }//Of if
      else if(red<=30){
        dou=(-0.0005925)*red*red+0.76667
      }//Of if
    else{
        dou=(-1.0)/18200*red*red+100.0/182
      }
    dou*/
    /**
      * new Test method
      */
    val result=input.last
    val data=input.init
    val re=push(data);
    judge2(re,getExpect(result))
    /**
      * test end
      */
  }//Of test2
  def judge(re:Array[Double],ex:Array[Double]):Boolean={
    for(i<-0 to re.length-1){
      println("exp"+ex(i)+" re"+re(i));
      if((re(i)-0.5)*(ex(i)-0.5)<0)
        return false;
    }//Of for
    true;
  }//Of judge

  def judge2(re:Array[Double],ex:Array[Double]):Boolean={
    var red=0d;
    var exp=0d;
/*    for(i<-0 to re.length-1){
      if(re(i)>0.5){
        red+=math.pow(2,i)
      } //Of if
      exp+=ex(i)*math.pow(2,i)
    }//Of for
    println("exp"+exp+" re"+red);
    if((red<=60&&exp!=100)||(red>60&&exp==100))*/
    if(math.abs(re(0)-ex(0))<0.1 && math.abs(re(1)-ex(1))<0.1 && math.abs(re(2)-ex(2))<0.5){
    return true
    }//Of if
    else
    false
  }//Of judge2
  def getExpect(last:Double):Array[Double]={
  /*  case 0d=>Array(0d,0d)
    case 1d=>Array(0d,1d)
    case 2d=>Array(1d,0d)*/
    if (last<0) Array(1d,0d,1d)
    else if (last==1.0)Array(0d,1d,0d)
    else Array(0d,1d,last)
  }//Of
  def getExpect2(last:Double)={

  }//Of double
  def getExpect(last:Int):Array[Double]={
    var result=new Array[Double](7)
    for(i<-0 to 6){
      result(i)=(last>>i)&1
    }//Of for
    result
  }//Of getExpect
  def totalError(expect:Array[Double]): Double ={
    var err=0d;
    for(i<-0 to expect.length-1){
      err+=math.pow((expect(i)-work(work.length-1).layer(i).output),2);
    }//Of for
    0.5*err;
  }//Of totalError
  def main_test(args: Array[String]): Unit = {
      val data=Array(2,6,6,6,2);
      build(data);

      print("before train:\n"+NN);
      var lines=Source.fromFile("src/trainTest").getLines();
      var tmp=new ArrayBuffer[Double];
//      for(line<-lines){
    //        line.split("\\s+").foreach(x=>{
    //        tmp+=x.toDouble
    //        });
    //        train(tmp.toArray,10);
    //        tmp.clear();
    //      }//Of for
    for(i<-1 to 5000){
      train(Array(0d,0d,1d),1);
      train(Array(0d,1d,0d),1);
      train(Array(1d,0d,2d),1);
      train(Array(1d,1d,1d),1);
      if (i%100==0){
        print("the time is:"+i+"\n");
        train(Array(0d,0d,1d),1,true);
        train(Array(0d,1d,0d),1,true);
        train(Array(1d,0d,2d),1,true);
        train(Array(1d,1d,1d),1,true);
      }//Of if
    }//Of for
      print("after train:\n"+NN);
      println(test(Array(3d,3d,1d)));
      println(test(Array(1d,1d,1d)));
      println(test(Array(0d,0d,1d)));
      println(test(Array(1d,0d,2d)));
  } //Of main
}//Of NN
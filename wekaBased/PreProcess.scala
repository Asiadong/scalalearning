package wekaBased

import java.io.File
import java.util
import java.util._

import base.{DateProcesser, Entrace, FileLoad, LOG}
import weka.core.converters.{ArffLoader, ArffSaver}
import weka.filters.Filter
import weka.filters.unsupervised.attribute.Add
import scala.collection.mutable
/**
  * Created by Asia on 2017/9/8.
  */
class PreProcess {

  val fileOnTrain=Entrace.basePathEnd+"on_tran.arff"
  val fileOffTrain=Entrace.basePathEnd+"off_train.arff"
  val fileTest=Entrace.basePathEnd+"end_test.arff"
  var arffl:ArffLoader=null
  var fil:File=null
  var index=4
  val bigSets=new Array[util.HashSet[Int]](7)
  val bigMap=new util.HashMap[Double,Array[Int]]()
  val bigMap2=new util.HashMap[Double,Array[Int]]()
  val testSetU=mutable.Set[Int]()
  val testSetD=mutable.Set[String]()
  val uidBag=new mutable.ListBuffer[Int]
  var uidMax=0
  var uidMin=0
  val midBag=new mutable.ListBuffer[Int]
  var midMax=0
  var midMin=0
  val rateBag=new mutable.ListBuffer[String]

  def tempf2(string: String): Unit ={
    val v=string.split(",")
    val uid=java.lang.Double.parseDouble(v(0))
    val action=v(2)
    val disRate=v(4)
    val count=v(5)
    val date=v(6)
    /*if(arrsetU1.contains(uid)){
      if(bigMap.containsKey(uid)){
        val value=bigMap.get(uid)
        value(Integer.parseInt(v(2)))+=1
        if(!disRate.equals("null")){
             value(3)=1
        }//Of if
        if(count.equals("null")){
          value(4)+=1
        }//Of if
        else if(date.equals("null")){
          value(5)+=1
        }//Of else if
        else{
          value(6)+=1
        }//Of else
        bigMap.put(uid,value)
      }//Of if
      else{
      val value=new Array[Int](7)
        value(Integer.parseInt(v(2)))+=1
        if(!disRate.equals("null")){
          value(3)=1
        }//Of if
        if(count.equals("null")){
          value(4)+=1
        }//Of if
        else if(date.equals("null")){
          value(5)+=1
        }//Of else if
        else{
          value(6)+=1
        }//Of else
        bigMap.put(uid,value)
      }//Of else
    }//Of if*/
    
  }//Of temp

  def devide1(string: String): Unit ={
    val v=string.split(",")
    val uid= 1.0*(v(0).toInt - uidMin)/(uidMax-uidMin)
    val action=v(2).toInt/2.0
    val discount=1.0*rateBag.indexOf(v(4))/rateBag.length
    var dateR=0d
    var date=0d
    var vars=0d
    var date_1:DateProcesser=null
    var dateR_1:DateProcesser=null
    var m=0d
    if (v(5)=="null"){
       dateR=0
      vars=1.0
    }//Of if
    else{
      dateR_1=new DateProcesser(v(5))
      dateR=dateR_1.day*1.0/31
      m=dateR_1.month
    }//Of esle
    if (v(6)=="null"){
      date=0
    }//Of if
    else{
      date_1=new DateProcesser(v(6))
      date=date_1.day*1.0/31
      m=date_1.month
      if(vars!=1.0)vars=(date_1.D.getTime-dateR_1.D.getTime)/(1000*3600*24*31.0)
    }//Of esle

    /*if(arrsetU1.contains(uid)){
      if(bigMap.containsKey(uid)){
        val value=bigMap.get(uid)
        value(Integer.parseInt(v(2)))+=1
        if(!disRate.equals("null")){
             value(3)=1
        }//Of if
        if(count.equals("null")){
          value(4)+=1
        }//Of if
        else if(date.equals("null")){
          value(5)+=1
        }//Of else if
        else{
          value(6)+=1
        }//Of else
        bigMap.put(uid,value)
      }//Of if
      else{
      val value=new Array[Int](7)
        value(Integer.parseInt(v(2)))+=1
        if(!disRate.equals("null")){
          value(3)=1
        }//Of if
        if(count.equals("null")){
          value(4)+=1
        }//Of if
        else if(date.equals("null")){
          value(5)+=1
        }//Of else if
        else{
          value(6)+=1
        }//Of else
        bigMap.put(uid,value)
      }//Of else
    }//Of if*/
    LOG.data("OnSpit"+m+".csv",uid+","+action+","+discount+","+dateR+","+date+","+vars+"\n")
  }//Of temp
  def devide2(string: String): Unit ={
    val v=string.split(",")
    val uid= 1.0*(v(0).toInt - uidMin)/(uidMax-uidMin)
    val mid=1.0*(v(1).toInt-midMin)/(midMax-midMin)
    var dist=0d
    if(v(4)=="null")dist=1
    else dist=v(4).toInt*1.0/11
    val discount=1.0*rateBag.indexOf(v(3))/rateBag.length
    var m=0d
    var dateR=0d
    var date=0d
    var vars=0d
    var date_1:DateProcesser=null
    var dateR_1:DateProcesser=null
    if (v(5)=="null"){
      dateR=0d
      vars=1d
    }//Of if
    else{
      dateR_1=new DateProcesser(v(5))
      dateR=dateR_1.day*1.0/31
      m=dateR_1.month
    }//Of esle
    if (v(6)=="null"){
      date=0d
      vars=0-1.0
    }//Of if
    else{
      date_1=new DateProcesser(v(6))
      date=date_1.day*1.0/31
      m=date_1.month
      if(vars!=1d)vars=(date_1.D.getTime-dateR_1.D.getTime)/(1000*3600*24*31.0)
    }//Of esle

    /*if(arrsetU1.contains(uid)){
      if(bigMap.containsKey(uid)){
        val value=bigMap.get(uid)
        value(Integer.parseInt(v(2)))+=1
        if(!disRate.equals("null")){
             value(3)=1
        }//Of if
        if(count.equals("null")){
          value(4)+=1
        }//Of if
        else if(date.equals("null")){
          value(5)+=1
        }//Of else if
        else{
          value(6)+=1
        }//Of else
        bigMap.put(uid,value)
      }//Of if
      else{
      val value=new Array[Int](7)
        value(Integer.parseInt(v(2)))+=1
        if(!disRate.equals("null")){
          value(3)=1
        }//Of if
        if(count.equals("null")){
          value(4)+=1
        }//Of if
        else if(date.equals("null")){
          value(5)+=1
        }//Of else if
        else{
          value(6)+=1
        }//Of else
        bigMap.put(uid,value)
      }//Of else
    }//Of if*/
    LOG.data("OffSpit"+m+".csv",uid+","+mid+","+discount+","+m/6.0+","+dateR+","+dist+","+vars+"\n")
    /*if(arrsetU1.contains(uid)){
      if(bigMap.containsKey(uid)){
        val value=bigMap.get(uid)
        value(Integer.parseInt(v(2)))+=1
        if(!disRate.equals("null")){
             value(3)=1
        }//Of if
        if(count.equals("null")){
          value(4)+=1
        }//Of if
        else if(date.equals("null")){
          value(5)+=1
        }//Of else if
        else{
          value(6)+=1
        }//Of else
        bigMap.put(uid,value)
      }//Of if
      else{
      val value=new Array[Int](7)
        value(Integer.parseInt(v(2)))+=1
        if(!disRate.equals("null")){
          value(3)=1
        }//Of if
        if(count.equals("null")){
          value(4)+=1
        }//Of if
        else if(date.equals("null")){
          value(5)+=1
        }//Of else if
        else{
          value(6)+=1
        }//Of else
        bigMap.put(uid,value)
      }//Of else
    }//Of if*/

    //LOG.data("OffSpit"+date.month+".csv",string+"\n")
  }//Of temp
  def devide3(string: String): Unit ={
    val v=string.split(",")
    val uid= 1.0*(v(0).toInt - uidMin)/(uidMax-uidMin)
    val mid=1.0*(v(1).toInt-midMin)/(midMax-midMin)
    var dist=0d
    if(v(4)=="null")dist=1
    else dist=v(4).toInt*1.0/11
    val discount=1.0*rateBag.indexOf(v(3))/rateBag.length
    var m=0d
    var dateR=0d
    if (v(5)=="null"){
      dateR=0d
    }//Of if
    else{
      val dateR_1=new DateProcesser(v(5))
      dateR=dateR_1.day*1.0/31
      m=dateR_1.month
    }//Of esle
   /* if (v(6)=="null"){
      date=0
    }//Of if
    else{
      date_1=new DateProcesser(v(6))
      date=date_1.day*1.0/31
      m=date_1.month/6.0
    }//Of esle
    val vars=(date_1.D.getTime-dateR_1.D.getTime)/(1000*3600*24*31.0)*/
    /*if(arrsetU1.contains(uid)){
      if(bigMap.containsKey(uid)){
        val value=bigMap.get(uid)
        value(Integer.parseInt(v(2)))+=1
        if(!disRate.equals("null")){
             value(3)=1
        }//Of if
        if(count.equals("null")){
          value(4)+=1
        }//Of if
        else if(date.equals("null")){
          value(5)+=1
        }//Of else if
        else{
          value(6)+=1
        }//Of else
        bigMap.put(uid,value)
      }//Of if
      else{
      val value=new Array[Int](7)
        value(Integer.parseInt(v(2)))+=1
        if(!disRate.equals("null")){
          value(3)=1
        }//Of if
        if(count.equals("null")){
          value(4)+=1
        }//Of if
        else if(date.equals("null")){
          value(5)+=1
        }//Of else if
        else{
          value(6)+=1
        }//Of else
        bigMap.put(uid,value)
      }//Of else
    }//Of if*/
    LOG.data("TestSpit"+m+".csv",uid+","+mid+","+discount+","+m/6.0+","+dateR+","+dist+"\n")

    /*if(arrsetU1.contains(uid)){
      if(bigMap.containsKey(uid)){
        val value=bigMap.get(uid)
        value(Integer.parseInt(v(2)))+=1
        if(!disRate.equals("null")){
             value(3)=1
        }//Of if
        if(count.equals("null")){
          value(4)+=1
        }//Of if
        else if(date.equals("null")){
          value(5)+=1
        }//Of else if
        else{
          value(6)+=1
        }//Of else
        bigMap.put(uid,value)
      }//Of if
      else{
      val value=new Array[Int](7)
        value(Integer.parseInt(v(2)))+=1
        if(!disRate.equals("null")){
          value(3)=1
        }//Of if
        if(count.equals("null")){
          value(4)+=1
        }//Of if
        else if(date.equals("null")){
          value(5)+=1
        }//Of else if
        else{
          value(6)+=1
        }//Of else
        bigMap.put(uid,value)
      }//Of else
    }//Of if*/


  }//Of temp
  def bag(string: String): Unit ={
    val v=string.split(",")
    val mid=v(1).toInt
    //val disCount=v(index)
    testSetU.add(mid)
   // testSetD.add(disCount)
  }//Of bag

  def tempf1_1(string: String): Unit ={
    uidBag+=string.toInt
  }//Of temp
  def tempf1_2(string: String): Unit ={
    midBag+=string.toInt
  }//Of temp
  def tempf1_3(string: String): Unit ={
  rateBag+=string
  }//Of temp


  def secProcess()={
    var f:FileLoad=null

    for(i<-0 to 5){
      var tempID=0d
      var result=0d
      f=new FileLoad(LOG.path+"OnSpit"+i+".0.csv")
      f.process(x=>{
        val v=x.split(",")
        if(v(0).toDouble!=tempID) {
          LOG.data("OnReN"+i+".csv",tempID+","+result+"\n")
          tempID = v(0).toDouble
          if(math.abs(v(6).toDouble)== 1.0)result=0
          else result=v(6).toDouble
        }//Of if
        else {
          if(math.abs(v(6).toDouble)== 1.0)result+=0
          else result+=v(6).toDouble
        }//Of else
      },false)
      LOG.data("OnReN"+i+".csv",tempID+","+result+"\n")
      LOG.out
    }//Of for
    println("onREend")
    for(i<-0 to 5) {
      var tempID = 0d
      var result = 0d
      f = new FileLoad(LOG.path + "OffSpit" + i + ".0.csv")
      f.process(x => {
        val v = x.split(",")
        if (v(0).toDouble != tempID ) {
          LOG.data("OffReN" + i + ".csv", tempID + "," + result + "\n")
          tempID = v(0).toDouble
          if (math.abs(v(6).toDouble)== 1.0) result = 0
          else result = v(6).toDouble
        } //Of if
        else {
          if (math.abs(v(6).toDouble)== 1.0) result += 0
          else result += v(6).toDouble
        } //Of else
      },false)
      LOG.data("OffReN" + i + ".csv", tempID + "," + result + "\n")
      LOG.out
    }//Of for
    println("offREend")
  }//Of secProcess

  def featureSelect: Unit ={
    var fl=new FileLoad(LOG.path+"idBag")
    fl.process(x=>tempf1_1(x),false)
    println("loadUIDend")
    uidMax=uidBag.max
    println("loadUIDMax"+uidMax)
    uidMin=uidBag.min
    println("loadUIDMin"+uidMin+uidBag.indexOf(uidMin))
    fl=new FileLoad(LOG.path+"midBag")
    fl.process(x=>tempf1_2(x),false)
    println("loadMIDend")
    midMax=midBag.max
    println("loadMIDMax"+midMax)
    midMin=midBag.min
    println("loadMIDMin"+midMin+midBag.indexOf(midMin))
    fl=new FileLoad(LOG.path+"rateBag")
    fl.process(x=>tempf1_3(x),false)
    println("loadRateend")


    /*fl=new FileLoad(Entrace.basePathEnd+"ccf_offline_stage1_train.csv")
    fl.process(x=>devide2(x))
    LOG.out
    println("process2end")
    fl=new FileLoad(Entrace.basePathEnd+"ccf_offline_stage1_test_revised.csv")
    fl.process(x=>devide3(x))
    println("process3end")
    LOG.out*/
    fl=new FileLoad(Entrace.basePathEnd+"ccf_online_stage1_train.csv")
    fl.process(x=>devide1(x))
    println("process1end")
    LOG.out
  }//Of featureSelect

  def mapToString(data:util.HashMap[Double,Array[Int]]):String ={
    val iterator=data.entrySet().iterator()
    var result=""
    while (iterator.hasNext){
      val D=iterator.next()
      result+=D.getKey
      val tempData=D.getValue()
      tempData.foreach(x=>result+=","+x)
      result+="\n"
    }//Of while
    result
  }//Of mapToString
  def selectInstance(): Unit ={
    arffl=new ArffLoader()
    fil=new File(LOG.path+ "new_test.arff")
    arffl.setFile(fil)
    var instances_off_train=arffl.getDataSet()

    println("loadOFFTest end")
    var addAttribute=new Add()
    addAttribute.setInputFormat(instances_off_train)
    addAttribute.setOptions(Array[String]("-T", "NOM", "-N", "disDate", "-C", "last","-L","null,early,middle,late"))
    instances_off_train=Filter.useFilter(instances_off_train,addAttribute)
    /*addAttribute=new Add()
    addAttribute.setInputFormat(instances_off_train)
    addAttribute.setOptions(Array[String]("-T", "NOM", "-N", "decision", "-C", "last","-L","buy,not"))
    instances_off_train=Filter.useFilter(instances_off_train,addAttribute)
    val instance_test=new Instances(instances_off_train,0)
    val instance_train=new Instances(instances_off_train,0)
    instances_off_train.setRelationName("ALL_DATA")
    instance_test.setRelationName("TEST_DATA")*/
    instances_off_train.setRelationName("TEST_DATA")
    val num=instances_off_train.numInstances()
    for(i <- 0 to num-1){
      /*if(instances_off_train.instance(i).stringValue(6).equals("null")){
        instances_off_train.instance(i).setValue(15,"not")
      }//Of if
      else
        instances_off_train.instance(i).setValue(15,"buy")*/
      if(instances_off_train.instance(i).stringValue(5).equals("null")){
        instances_off_train.instance(i).setValue(14,"null")
      }//Of if
      else{
        val date=java.lang.Double.parseDouble(instances_off_train.instance(i).stringValue(5))
        if(date%100<=10){
          instances_off_train.instance(i).setValue(13,"early")
        }//Of if
        else if(date%100<=20){
          instances_off_train.instance(i).setValue(13,"middle")
        }//Of else if
        else{
          instances_off_train.instance(i).setValue(13,"late")
        }//Of else
      }//Of else
     /* if(i%47==31){
        instance_test.add(instances_off_train.instance(i))
      }//Of if
      else
        instance_train.add(instances_off_train.instance(i))*/
    }//Of for
    instances_off_train.deleteAttributeAt(2)
    instances_off_train.deleteAttributeAt(4)
    /*instance_test.deleteAttributeAt(2)
    instance_train.deleteAttributeAt(2)
    instances_off_train.deleteAttributeAt(4)
    instance_test.deleteAttributeAt(4)
    instance_train.deleteAttributeAt(4)
    instances_off_train.deleteAttributeAt(4)
    instance_test.deleteAttributeAt(4)
    instance_train.deleteAttributeAt(4)*/
    val arffs=new ArffSaver()
    arffs.setFile(new File(LOG.path+ "new_end_Test.arff"))
    arffs.setInstances(instances_off_train)
    //for(i<-16854 to 16900)
    //val s=instances_off_train.instance(16855)
    //val s2=instances_off_train.instance(16856)
    // println(instances_off_train.instance(16856).toString())
    arffs.writeBatch()
   /* arffs.resetOptions()
    arffs.setFile(new File(LOG.path+ "new1_TEST.arff"))
    arffs.setInstances(instance_test)*/
    //for(i<-16854 to 16900)
    //val s=instances_off_train.instance(16855)
    //val s2=instances_off_train.instance(16856)
    // println(instances_off_train.instance(16856).toString())
    /*arffs.writeBatch()
    arffs.resetOptions()
    arffs.setFile(new File(LOG.path+ "new1_TRAIN.arff"))
    arffs.setInstances(instance_train)*/
    //for(i<-16854 to 16900)
    //val s=instances_off_train.instance(16855)
    //val s2=instances_off_train.instance(16856)
    // println(instances_off_train.instance(16856).toString())
    //arffs.writeBatch()
  }//Of selectInstance

  def dataSpilt()={
    var fl=new FileLoad(Entrace.basePathEnd+"ccf_offline_stage1_train.csv")
    fl.process(x=>bag(x))
    //index=3
   /* fl=new FileLoad(Entrace.basePathEnd+"ccf_offline_stage1_train.csv")
    fl.process(x=>bag(x))
    index=3*/
    fl=new FileLoad(Entrace.basePathEnd+"ccf_offline_stage1_test_revised.csv")
    fl.process(x=>bag(x))
    testSetU.foreach(x=>LOG.data("midBag",x+"\n"))
    //testSetD.foreach(x=>LOG.data("rateBag",x+"\n"))
    LOG.out
  }//Of dataSplit

  def compare(): Unit ={
    for(i<- 0 to bigSets.length-1){
      bigSets(i)=new util.HashSet[Int]()
    }//Of for i
    var fl=new FileLoad(LOG.path+"expandedAttribute")
    //fl.process(x=>tempf1(x))
    arffl=new ArffLoader()
    fil=new File(fileTest)
    arffl.setFile(fil)
   var instances_off_train=arffl.getDataSet()
    println("loadOFFtest end")
   val addAttributes=new Array[Add](7)
    //val StringLables= new Array[String](7)

    /*addAttribute.setAttributeIndex("last")

    addAttribute.setAttributeName("extrAtt")
    addAttribute.setAttributeType(new SelectedTag("String"))*/
    for(i<- 0 to 6){
      var label=""
      val it=bigSets(i).iterator()
      while(it.hasNext()){
        label+=it.next()+","
      }//Of while
      label=label.substring(0,label.length()-1)
      addAttributes(i)=new Add()
      addAttributes(i).setInputFormat(instances_off_train)
      println(label)
      addAttributes(i).setOptions(Array[String]("-T", "NUM", "-N", "extra"+i, "-C", 7+i+""/*,"-L",label*/))
      instances_off_train=Filter.useFilter(instances_off_train,addAttributes(i))
    }//Of for i
    /*addAttribute.setInputFormat(instances_off_train)
    addAttribute.setOptions(Array[String]("-T", "STR", "-N", "extra", "-C", "8"))
    instances_off_train=Filter.useFilter(instances_off_train,addAttribute)*/

    val num=instances_off_train.numInstances()
    val attrCount=instances_off_train.numAttributes()
    for(i<-0 to num-1) {
      val uid = instances_off_train.instance(i).value(0)
      if (bigMap2.containsKey(uid)) {
        for (j <- 0 to 6)
          instances_off_train.instance(i).setValue(j + 6, bigMap2.get(uid)(j))
      } //Of if
      else {
        for (j <- 0 to 6)
          instances_off_train.instance(i).setValue(j + 6, 0)
      } //Of else
    }
    val arffs=new ArffSaver()
    arffs.setFile(new File(LOG.path+ "new_test.arff"))
    arffs.setInstances(instances_off_train)
    //for(i<-16854 to 16900)
    //val s=instances_off_train.instance(16855)
    //val s2=instances_off_train.instance(16856)
   // println(instances_off_train.instance(16856).toString())
      arffs.writeBatch()
    /*
    fil=new File(fileOnTrain)
    arffl.reset()*/
    //var fl=new FileLoad(LOG.path+"off_on_U")
    //fl.process(x=>tempf1(x))
    /*LOG.data("on_U",arrsetU2.size()+"U")
    LOG.data("on_U",arrsetM2.size()+"M")*/
    //arrsetM2.clear()
    //arrsetU2.clear()
    //fl=new FileLoad(Entrace.basePathEnd+"ccf_online_stage1_train.csv")
    //fl.process(x=>tempf2(x))
    /*LOG.data("off_U",arrsetU2.size()+"U")
    LOG.data("off_U",arrsetM2.size()+"M")*/
    /*arrsetM2.clear()
    arrsetU2.clear()*/
    //fl=new FileLoad(Entrace.basePathEnd+"ccf_offline_stage1_test_revised.csv")
    //fl.process(x=>tempf1(x))
    //LOG.data("test_U",arrsetU2.size()+"U")
    //LOG.data("test_U",arrsetM2.size()+"M")
    //arrsetM2.clear()
    //arffl.setFile(fil)
    //val instances_on_train=arffl.getDataSet()
    /*println("loadONtrain end")
    fil=new File(fileTest)
    arffl.reset()
    arffl.setFile(fil)
    val instances_test=arffl.getDataSet()
    println("loadTest end")*/
    /*
    start to compare
     */
    /*var count=instances_off_train.numInstances()
    var temp:Instance=null;
    for (i<-0 to count-1){
      temp=instances_off_train.instance(i)
      //arrsetU1.add(temp.value(0))
      //arrsetM1.add(temp.value(1))
      if(arrsetU2.contains(temp.value(0))){
        arrsetU3.add(temp.value(0))
      }//Of if
      if(arrsetM2.contains(temp.value(1))){
        arrsetM3.add(temp.value(1))
      }//Of if
    }//Of for
    /**
      * second
      */
     /*count=instances_on_train.numInstances()
    for (i<-0 to count-1){
      temp=instances_on_train.instance(i)
      arrsetU2.add(temp.value(0))
      arrsetM2.add(temp.value(1))
      if(arrsetU1.contains(temp.value(0))){
        arrsetU3.add(temp.value(0))
      }//Of if
      if(arrsetM1.contains(temp.value(1))){
        arrsetM3.add(temp.value(1))
      }//Of if
    }//Of for
    */
    var sU=""
    var sM=""
    var itU=arrsetU3.iterator()
    var itM=arrsetM3.iterator()
    while(itU.hasNext){
      sU+=itU.next()+"\n"
    }//Of while
    while(itM.hasNext){
      sM+=itM.next()+"\n"
    }//Of while
    LOG.data("off_on_U",sU)
    LOG.data("off_on_M",sM)
    println("off on save end")

    /**
      * third
      */
    count=instances_test.numInstances()
    for (i<-0 to count-1){
      temp=instances_test.instance(i)
      /*if(arrsetU1.contains(temp.value(0))){
        arrsetU4.add(temp.value(0))
      }//Of if
      if(arrsetM1.contains(temp.value(1))){
        arrsetM4.add(temp.value(1))
      }//Of if*/
      if(arrsetU2.contains(temp.value(0))){
        arrsetU5.add(temp.value(0))
      }//Of if
      if(arrsetM2.contains(temp.value(1))){
        arrsetM5.add(temp.value(1))
      }//Of if
    }//Of for

   var sU=""
    var sM=""
    var itU=arrsetU4.iterator()
    var itM=arrsetM4.iterator()
    while(itU.hasNext){
      sU+=itU.next()+"\n"
    }//Of while
    while(itM.hasNext){
      sM+=itM.next()+"\n"
  }//Of while
  LOG.data("off_test_U",sU)
  LOG.data("off_test_M",sM)
    println("off test save end")
    sU=""
    sM=""
    itU=arrsetU5.iterator()
    itM=arrsetM5.iterator()
    while(itU.hasNext){
      sU+=itU.next()+"\n"
    }//Of while
    while(itM.hasNext){
      sM+=itM.next()+"\n"
    }//Of while
    LOG.data("test_on_U",sU)
    LOG.data("test_on_M",sM)
    println("all end")*/
  }//Of compare
}//Of PreProcess

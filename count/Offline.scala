package count

/**
  * Created by Asia on 2017/7/20.
  */
class Offline extends Online{

  override def iter(value: String): String ={
    val tmp=value.split(",");
    UID=tmp(0);
    MID=tmp(1);
    CID=tmp(3);
    UID;
  }//Of iter
}

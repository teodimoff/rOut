package io.rout.example.xml

case class Payload(s: String,i:Int,b: Boolean,extend: Extend)

case class Extend(float: Float,double: Double,list: List[Int])
package tweak.compiler 

trait TweakObject 

class TweakValue extends TweakObject

class TweakInteger(i: Int) extends TweakValue

class TweakDouble(d: Double) extends TweakValue

class TweakString(s: String) extends TweakValue

class TweakSymbol(s: String) extends TweakValue 

//class TweakFn extends TweakObject

//class TweakList extends TweakObject

/* implicit def intToTInt(i: Int): TweakInteger = TweakInteger(i)
implicit def doubleToTDouble(d: Double): TweakDouble = TweakDouble(d)
implicit def stringToTString(s: String): TweakString = TweakString(s) */

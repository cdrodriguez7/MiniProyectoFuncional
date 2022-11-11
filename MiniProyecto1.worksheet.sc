val f1 = (x : Double) => -Math.pow(x,2)+(8*x)-12
val f2 = (x : Double) => 3*Math.pow(x,2)
val f3 = (x : Double) => x+2*Math.pow(x,2)-Math.pow(x,3)+5*Math.pow(x,4)
val f4 = (x : Double) => ((2*x+1)/Math.pow(x,2)+x)
val f5 = (x : Double) => Math.pow(Math.E,x)
val f6 = (x : Double) => (1/Math.sqrt(x-1))
val f7 = (x : Double) => (1/1+Math.pow(x,2))

//simpson 1/3 compuesta
def compuesta (a:Int, b:Int, f : Double => Double) = {
	val n = 5 //numero de sub intervalos
    val h = (b-a)/n.toDouble
    val xj = (j : Double) => a+j*h
	val fnc = (j : Double) => h/3 * f(xj(2*j-2)) + 4*f(xj(2*j-1)) + f(xj(2*j))
}
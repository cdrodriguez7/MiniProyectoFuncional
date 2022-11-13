val f1 = (x : Double) => -Math.pow(x,2)+(8*x)-12
val f2 = (x : Double) => 3*Math.pow(x,2)
val f3 = (x : Double) => x+2*Math.pow(x,2)-Math.pow(x,3)+5*Math.pow(x,4)
val f4 = (x : Double) => ((2*x+1)/(Math.pow(x,2)+x))
val f5 = (x : Double) => Math.pow(Math.E,x)
val f6 = (x : Double) => (1/Math.sqrt(x-1))
val f7 = (x : Double) => (1/(1+Math.pow(x,2)))


//simpson 1/3 "compuesta"

val n = 3000 //numero de sub intervalos

def compuesta (a:Int, b:Int, n: Int, f : Double => Double) = {
val h = ((b-a)/(n*1.0))
val xj = (j : Double) => a+j*h 
val fnc = (j : Double) => (f(xj(2*j-2)) + 4*f(xj(2*j-1)) + f(xj(2*j)))
val abc = ((1 to n/2).map(fnc(_)).sum)
(abc * h)/3
}

compuesta(3,5,n,f1)
compuesta(0,2,n,f2)
compuesta(-1,1,n,f3)
compuesta(1,2,n,f4)
compuesta(0,1,n,f5)
compuesta(2,3,n,f6)
compuesta(0,1,n,f7)


//Simpson 1/3 "extendida"

def extendida (a:Int, b:Int, f : Double => Double) = {
val n = 2*(b-a)
val h = (b-a)/n.toDouble
val xj = (j : Double) => a+j*h 

val fnc = (f(a) + 4* (1 to n-1 by 2).map(xj(_)).sum + 2 * (2 to n-2 by 2).map (xj(_)).sum + f(b))
fnc * (h/3)
}

extendida(3,5,f1)
extendida(0,2,f2)
extendida(-1,1,f3)
extendida(1,2,f4)
extendida(0,1,f5)
extendida(2,3,f6)
extendida(0,1,f7)



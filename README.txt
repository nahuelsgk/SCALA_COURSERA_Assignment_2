Entorno:
> cd /home/nahuel/Desktop/Projectos/COURSERA/ReactiveProgramming/Assignment_Week2/calculator
> /home/nahuel/git/sbt/bin/sbt

Importar en eclipse:
Añadir la SCALA lib en los properties del proyecto para arrancar los WorkSheet:w

[] 1a Tarea
Implementar la la funcion 
src/scala/calculator/TweetLength.scala:def tweetRemainingCharsCount
f: Signal[String] => Signal[Int]

La funcion tiene un Signal como parametro.
Los lanzar un eventos es lanzar una señal. Las operaciones fundamentales son.
1) La señal tiene un valor concreto.
2) Definir señales en funcion de otras señales.


[] 2a Tarea: Root solver	
Arranca console
> import calculator._
Defino tres señales
> val a = Signal(1.0)
> val a = Signal(1.0)
> val a = Signal(1.0)
> val delta = Polynomial.computeDelta(Signal(1.0),Signal(1.0),Signal(1.0))
> delta()
> val solucion = Polynomial.computeSolutions(a,b,c,delta)
> solucion()
¿COMO SE PUEDE ACTUALIZAR EL VALOR DE UN SEÑAL para que solucion funcione?

 


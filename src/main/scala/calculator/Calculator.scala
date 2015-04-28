package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    //println("Computando valores de la calculadora")
    //Ejemplo de recorrer array: namedExpressions.foreach(p => println(">> key=" + p._1 + ", value="+ eval( p._2(), namedExpressions)))
    //Pero el siguiente es mas limpio
    //for((a,b) <- namedExpressions) println(">> key: "+a+"->"+b())
    
    namedExpressions map {
      case (variable, signalExpr) => (variable, Signal{
          //println("Evaluando variable " + variable)
          eval(signalExpr(), namedExpressions)
        }
      )
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    //println("Evaluando una expresion")
    
    expr match{
      case Literal(v)   => {
        //println("Evalua un literal")
        v 
      } 
      case Ref(ref)     => {
        
        /*if(  Ref(ref) == getReferenceExpr(ref,references)  ){
          //for((a,b) <- references) println(">> key: "+a+"->"+b)
          val dereferences = references - ref
          //for((a,b) <- dereferences) println(">> key: "+a+"->"+b)
          eval(getReferenceExpr(ref,dereferences), dereferences)
        } 
        else eval(getReferenceExpr(ref,references), references)*/
        val dereferences = references - ref
        eval(getReferenceExpr(ref,references), dereferences)
        
      }
      case Plus(a, b)   => {
        //println("Evalua la suma")
        eval(a, references) + eval(b, references)
      } 
      case Minus(a, b)  => {
        //println("Evalua la resta")
        eval(a, references) - eval(b, references)
      }
      case Times(a, b)  => {
        //println("Evalua la multiplicacion")
        eval(a, references) * eval(b, references)
      }
      case Divide(a, b) => {
        if (b == Literal(0.0)) Double.NaN
        //println("Evalua la division")
        else eval(a, references) / eval(b, references)
      }
      case _            => {
        //println("No reconoce la expresion")
        Double.NaN
      }
    } 
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}

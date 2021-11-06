/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Triangle.AbstractSyntaxTrees;

import Triangle.SyntacticAnalyzer.SourcePosition;

/**
 *
 * @author sande
 */
public class compoundDeclRecursive extends Declaration {

  public compoundDeclRecursive (Profuncs pAST, SourcePosition thePosition) {
    super (thePosition);
    P = pAST;
    
  }

  public Object visit (Visitor v, Object o) {
    return v.visitcompoundDeclRecursive(this, o);
  }

  public Profuncs P;
}

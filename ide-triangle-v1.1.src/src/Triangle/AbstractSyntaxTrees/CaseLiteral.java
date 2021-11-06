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
public class CaseLiteral extends Case {
    
    public CaseLiteral (Terminal tAST,Command cAST, SourcePosition thePosition) {
    super (thePosition);
    T = tAST;
    C=cAST;
  }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitCaseLiteral(this, o);
    }
    
    public Terminal T;
    public Command C;
    
    
}
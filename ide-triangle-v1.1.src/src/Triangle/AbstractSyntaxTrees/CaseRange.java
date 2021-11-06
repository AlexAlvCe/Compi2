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
public class CaseRange extends Case {
    
    public CaseRange (Terminal t1AST, Terminal t2AST,Command cAST, SourcePosition thePosition) {
    super (thePosition);
    T1 = t1AST;
    T2 = t2AST;
    C = cAST;
  }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitCaseRange(this, o);
    }
    
    public Terminal T1;
    public Terminal T2;
    public Command C;

    
}

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Triangle.AbstractSyntaxTrees;

import Triangle.SyntacticAnalyzer.SourcePosition;
import java.util.ArrayList;

/**
 *
 * @author sande
 */
public class CasesElse extends Cases {
    
    public CasesElse (ArrayList<Case> cAST, Command c2AST, SourcePosition thePosition) {
    super (thePosition);
    listC = cAST;
    C2 = c2AST;
  }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitCasesElse(this, o);
    }
    
    public ArrayList<Case> listC;
    public Command C2;

    
}
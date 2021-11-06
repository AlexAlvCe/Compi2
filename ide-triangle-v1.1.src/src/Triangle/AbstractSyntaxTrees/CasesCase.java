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
public class CasesCase extends Cases {
    
    public CasesCase (ArrayList<Case> c1AST, SourcePosition thePosition) {
    super (thePosition);
    listC = c1AST;
  }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitCasesCase(this, o);
    }
    
    public ArrayList<Case> listC;

    
}

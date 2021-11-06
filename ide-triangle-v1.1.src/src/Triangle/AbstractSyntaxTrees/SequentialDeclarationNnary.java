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
public class SequentialDeclarationNnary extends Declaration {

  public SequentialDeclarationNnary (Declaration d1AST, ArrayList<Declaration> d2AST,
                       SourcePosition thePosition) {
    super (thePosition);
    D1 = d1AST;
    LD = d2AST;
  }

  public Object visit(Visitor v, Object o) {
    return v.visitSequentialDeclarationNnary(this, o);
  }

  public Declaration D1;
  public ArrayList<Declaration> LD;
}
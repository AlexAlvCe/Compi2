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
public class Pro_funcs extends Profuncs {

  public Pro_funcs (ArrayList<Declaration>iAST, SourcePosition thePosition) {
    super (thePosition);
    L = iAST;
  }

  
  public Object visit (Visitor v, Object o) {
    return v.visitPro_funcs(this, o);
  }

  public ArrayList<Declaration>  L;
}

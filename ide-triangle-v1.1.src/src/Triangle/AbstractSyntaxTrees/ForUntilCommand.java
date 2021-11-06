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
public class ForUntilCommand extends Command {
    public ForUntilCommand (Identifier iAST1, Expression eAST1, Expression eAST2, Expression eAST3, Command cAST1, SourcePosition thePosition) {
    super (thePosition);
    I = iAST1;
    E1 = eAST1;
    E2 = eAST2;
    E3 = eAST3;
    C1 = cAST1;
  }

  public Object visit(Visitor v, Object o) {
    return v.visitForUntilCommand(this, o);
  }
  
  public Identifier I;
  public Expression E1;
  public Expression E2;
  public Expression E3;
  public Command C1;
}

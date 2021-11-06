/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Triangle.AbstractSyntaxTrees;

import Triangle.SyntacticAnalyzer.SourcePosition;
/**
 *
 * @author Alexander
 */
public class SelectCommand extends Command {

  public SelectCommand (Expression eAST, Cases cAST, SourcePosition thePosition) {
    super (thePosition);
    E = eAST;
    C = cAST;
  }

  @Override
  public Object visit(Visitor v, Object o) {
    return v.visitSelectCommand(this, o);
  }

  public Cases C;
  public Expression E;
}

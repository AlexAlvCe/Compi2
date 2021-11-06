/*
 * @(#)IfCommand.java                        2.1 2003/10/07
 *
 * Copyright (C) 1999, 2003 D.A. Watt and D.F. Brown
 * Dept. of Computing Science, University of Glasgow, Glasgow G12 8QQ Scotland
 * and School of Computer and Math Sciences, The Robert Gordon University,
 * St. Andrew Street, Aberdeen AB25 1HG, Scotland.
 * All rights reserved.
 *
 * This software is provided free for educational use only. It may
 * not be used for commercial purposes without the prior written permission
 * of the authors.
 */

package Triangle.AbstractSyntaxTrees;

import Triangle.SyntacticAnalyzer.SourcePosition;
import java.util.ArrayList;

public class IfCommand extends Command {

  public IfCommand (ArrayList<Expression> eAST, ArrayList<Command> c1AST,
                    SourcePosition thePosition) {
    super (thePosition);
    E = eAST;
    C = c1AST;
  }

  public Object visit(Visitor v, Object o) {
    return v.visitIfCommand(this, o);
  }

  public ArrayList<Expression> E;
  public ArrayList<Command> C;
}

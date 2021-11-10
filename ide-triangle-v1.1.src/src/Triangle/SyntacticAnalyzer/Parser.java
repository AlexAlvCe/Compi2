/*
 * @(#)Parser.java                        2.1 2003/10/07
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

package Triangle.SyntacticAnalyzer;

import Triangle.AbstractSyntaxTrees.SelectCommand;
import Triangle.ErrorReporter;
import Triangle.AbstractSyntaxTrees.ActualParameter;
import Triangle.AbstractSyntaxTrees.ActualParameterSequence;
import Triangle.AbstractSyntaxTrees.ArrayAggregate;
import Triangle.AbstractSyntaxTrees.ArrayExpression;
import Triangle.AbstractSyntaxTrees.ArrayTypeDenoter;
import Triangle.AbstractSyntaxTrees.AssignCommand;
import Triangle.AbstractSyntaxTrees.BinaryExpression;
import Triangle.AbstractSyntaxTrees.CallCommand;
import Triangle.AbstractSyntaxTrees.CallExpression;
import Triangle.AbstractSyntaxTrees.Case;
import Triangle.AbstractSyntaxTrees.CaseLiteral;
import Triangle.AbstractSyntaxTrees.CaseRange;
import Triangle.AbstractSyntaxTrees.Cases;
import Triangle.AbstractSyntaxTrees.CasesCase;
import Triangle.AbstractSyntaxTrees.CasesElse;
import Triangle.AbstractSyntaxTrees.CharacterExpression;
import Triangle.AbstractSyntaxTrees.CharacterLiteral;
import Triangle.AbstractSyntaxTrees.Command;
import Triangle.AbstractSyntaxTrees.ConstActualParameter;
import Triangle.AbstractSyntaxTrees.ConstDeclaration;
import Triangle.AbstractSyntaxTrees.ConstFormalParameter;
import Triangle.AbstractSyntaxTrees.Declaration;
import Triangle.AbstractSyntaxTrees.DotVname;
import Triangle.AbstractSyntaxTrees.EmptyActualParameterSequence;
import Triangle.AbstractSyntaxTrees.EmptyCommand;
import Triangle.AbstractSyntaxTrees.EmptyFormalParameterSequence;
import Triangle.AbstractSyntaxTrees.Expression;
import Triangle.AbstractSyntaxTrees.FieldTypeDenoter;
import Triangle.AbstractSyntaxTrees.ForDo;
import Triangle.AbstractSyntaxTrees.ForInCommand;
import Triangle.AbstractSyntaxTrees.ForUntilCommand;
import Triangle.AbstractSyntaxTrees.ForWhileCommand;
import Triangle.AbstractSyntaxTrees.FormalParameter;
import Triangle.AbstractSyntaxTrees.FormalParameterSequence;
import Triangle.AbstractSyntaxTrees.FuncActualParameter;
import Triangle.AbstractSyntaxTrees.FuncDeclaration;
import Triangle.AbstractSyntaxTrees.FuncFormalParameter;
import Triangle.AbstractSyntaxTrees.Identifier;
import Triangle.AbstractSyntaxTrees.IfCommand;
import Triangle.AbstractSyntaxTrees.IfExpression;
import Triangle.AbstractSyntaxTrees.IntegerExpression;
import Triangle.AbstractSyntaxTrees.IntegerLiteral;
import Triangle.AbstractSyntaxTrees.LetCommand;
import Triangle.AbstractSyntaxTrees.LetExpression;
import Triangle.AbstractSyntaxTrees.MultipleActualParameterSequence;
import Triangle.AbstractSyntaxTrees.MultipleArrayAggregate;
import Triangle.AbstractSyntaxTrees.MultipleFieldTypeDenoter;
import Triangle.AbstractSyntaxTrees.MultipleFormalParameterSequence;
import Triangle.AbstractSyntaxTrees.MultipleRecordAggregate;
import Triangle.AbstractSyntaxTrees.Operator;
import Triangle.AbstractSyntaxTrees.Pro_funcs;
import Triangle.AbstractSyntaxTrees.ProcActualParameter;
import Triangle.AbstractSyntaxTrees.ProcDeclaration;
import Triangle.AbstractSyntaxTrees.ProcFormalParameter;
import Triangle.AbstractSyntaxTrees.Profuncs;
import Triangle.AbstractSyntaxTrees.Program;
import Triangle.AbstractSyntaxTrees.RecordAggregate;
import Triangle.AbstractSyntaxTrees.RecordExpression;
import Triangle.AbstractSyntaxTrees.RecordTypeDenoter;
import Triangle.AbstractSyntaxTrees.SequentialCommand;
import Triangle.AbstractSyntaxTrees.SequentialDeclaration;
import Triangle.AbstractSyntaxTrees.SequentialDeclarationNnary;
import Triangle.AbstractSyntaxTrees.SimpleTypeDenoter;
import Triangle.AbstractSyntaxTrees.SimpleVname;
import Triangle.AbstractSyntaxTrees.SingleActualParameterSequence;
import Triangle.AbstractSyntaxTrees.SingleArrayAggregate;
import Triangle.AbstractSyntaxTrees.SingleFieldTypeDenoter;
import Triangle.AbstractSyntaxTrees.SingleFormalParameterSequence;
import Triangle.AbstractSyntaxTrees.SingleRecordAggregate;
import Triangle.AbstractSyntaxTrees.SubscriptVname;
import Triangle.AbstractSyntaxTrees.Terminal;
import Triangle.AbstractSyntaxTrees.TypeDeclaration;
import Triangle.AbstractSyntaxTrees.TypeDenoter;
import Triangle.AbstractSyntaxTrees.UnaryExpression;
import Triangle.AbstractSyntaxTrees.VarActualParameter;
import Triangle.AbstractSyntaxTrees.VarDeclaration;
import Triangle.AbstractSyntaxTrees.VarDeclarationEXP;
import Triangle.AbstractSyntaxTrees.VarFormalParameter;
import Triangle.AbstractSyntaxTrees.Visitor;
import Triangle.AbstractSyntaxTrees.Vname;
import Triangle.AbstractSyntaxTrees.VnameExpression;
import Triangle.AbstractSyntaxTrees.WhileCommand;
import Triangle.AbstractSyntaxTrees.compoundDeclLocal;
import Triangle.AbstractSyntaxTrees.compoundDeclRecursive;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class Parser {
  public Hashtable<String,Variable> declaraciones = new Hashtable<>();
  public Hashtable<String,ProcFunc> procYFuncs = new Hashtable<>();
  public Variable retorno =  new Variable();
  public ProcFunc retornoProFunc =  new ProcFunc();
  public ArrayList<Integer> params= new ArrayList<>();
  public int profundidad=0;
  public int declarationPorfundidad = 0 ;
  private Scanner lexicalAnalyser;
  private ErrorReporter errorReporter;
  private Token currentToken ;
  private SourcePosition previousTokenPosition;

  public Parser(Scanner lexer, ErrorReporter reporter) {
    lexicalAnalyser = lexer;
    lexer.setParser(this);
    errorReporter = reporter;
    previousTokenPosition = new SourcePosition();
  }

// accept checks whether the current token matches tokenExpected.
// If so, fetches the next token.
// If not, reports a syntactic error.

  void accept (int tokenExpected) throws SyntaxError {
    if (currentToken.kind == tokenExpected) {
      previousTokenPosition = currentToken.position;
      currentToken = lexicalAnalyser.scan();
    } else {
      syntacticError("\"%\" expected here", Token.spell(tokenExpected));
    }
  }

  void acceptIt() {
    previousTokenPosition = currentToken.position;
    currentToken = lexicalAnalyser.scan();
  }

// start records the position of the start of a phrase.
// This is defined to be the position of the first
// character of the first token of the phrase.

  void start(SourcePosition position) {
    position.start = currentToken.position.start;
  }

// finish records the position of the end of a phrase.
// This is defined to be the position of the last
// character of the last token of the phrase.

  void finish(SourcePosition position) {
    position.finish = previousTokenPosition.finish;
  }

  void syntacticError(String messageTemplate, String tokenQuoted) throws SyntaxError {
    SourcePosition pos = currentToken.position;
    errorReporter.reportError(messageTemplate, tokenQuoted, pos);
    throw(new SyntaxError());
  }
  void contextualError(String messageTemplate, String tokenQuoted) throws SyntaxError {
    SourcePosition pos = currentToken.position;
    errorReporter.reportError(messageTemplate, tokenQuoted, pos);
    
  }

///////////////////////////////////////////////////////////////////////////////
//
// PROGRAMS
//
///////////////////////////////////////////////////////////////////////////////

  public Program parseProgram() {

    Program programAST = null;

    previousTokenPosition.start = 0;
    previousTokenPosition.finish = 0;
    currentToken = lexicalAnalyser.scan();

    try {
      Command cAST = parseCommand();
      programAST = new Program(cAST, previousTokenPosition);
      if (currentToken.kind != Token.EOT) {
        syntacticError("\"%\" not expected after end of program",
          currentToken.spelling);
      }
    }
    catch (SyntaxError s) { return null; }
    return programAST;
  }

///////////////////////////////////////////////////////////////////////////////
//
// LITERALS
//
///////////////////////////////////////////////////////////////////////////////

// parseIntegerLiteral parses an integer-literal, and constructs
// a leaf AST to represent it.

  IntegerLiteral parseIntegerLiteral() throws SyntaxError {
    IntegerLiteral IL = null;

    if (currentToken.kind == Token.INTLITERAL) {
      previousTokenPosition = currentToken.position;
      String spelling = currentToken.spelling;
      IL = new IntegerLiteral(spelling, previousTokenPosition);
      retorno.variable = spelling;
      retorno.type = Token.INTLITERAL;
      retorno.visibleAProfundidad=declarationPorfundidad;
      currentToken = lexicalAnalyser.scan();
    } else {
      IL = null;
      syntacticError("integer literal expected here", "");
    }
      
    return IL;
  }

// parseCharacterLiteral parses a character-literal, and constructs a leaf
// AST to represent it.

  CharacterLiteral parseCharacterLiteral() throws SyntaxError {
    CharacterLiteral CL = null;

    if (currentToken.kind == Token.CHARLITERAL) {
      previousTokenPosition = currentToken.position;
      String spelling = currentToken.spelling;
      CL = new CharacterLiteral(spelling, previousTokenPosition);
      retorno.variable = spelling;
      retorno.type = Token.CHARLITERAL;
      retorno.visibleAProfundidad=declarationPorfundidad;
      currentToken = lexicalAnalyser.scan();
    } else {
      CL = null;
      syntacticError("character literal expected here", "");
    }
    return CL;
  }

  
// parseIdentifier parses an identifier, and constructs a leaf AST to
// represent it.

  
  
  Identifier parseIdentifier() throws SyntaxError {
    Identifier I = null;

    if (currentToken.kind == Token.IDENTIFIER) {
      previousTokenPosition = currentToken.position;
      String spelling = currentToken.spelling;
      I = new Identifier(spelling, previousTokenPosition);
      currentToken = lexicalAnalyser.scan();
        retorno.variable=spelling;
        retorno.type = Token.IDENTIFIER;
        retorno.visibleAProfundidad = declarationPorfundidad;
    } else {
      I = null;
      syntacticError("identifier expected here", "");
    }
    return I;
  }

// parseOperator parses an operator, and constructs a leaf AST to
// represent it.

  Operator parseOperator() throws SyntaxError {
    Operator O = null;

    if (currentToken.kind == Token.OPERATOR) {
      previousTokenPosition = currentToken.position;
      String spelling = currentToken.spelling;
      O = new Operator(spelling, previousTokenPosition);
      currentToken = lexicalAnalyser.scan();
    } else {
      O = null;
      syntacticError("operator expected here", "");
    }
    return O;
  }
///////////////////////////////////////////////////////////////////////////////
//
// CASE
//
///////////////////////////////////////////////////////////////////////////////
 /*
 * ParseCase: esta rutina se encarga de recorrer un case reconociendo su sintaxis, y sino esta correcta dispara un error.
 */ 
  Case parseCase() throws SyntaxError {
    Case caseAST = null; // in case there's a syntactic error
    SourcePosition commandPos = new SourcePosition();
    start(commandPos);
    accept(Token.WHEN);
    if (Token.RANGE==currentToken.kind){
        acceptIt();
        Terminal l1AST= parseCaseLiteral();
        accept(Token.DOT);
        accept(Token.DOT);
        Terminal l2AST = parseCaseLiteral();
        accept(Token.THEN);
        Command cAST = parseCommand();
        caseAST = new CaseRange(l1AST, l2AST,cAST, commandPos);
    }else{
        Terminal l1AST= parseCaseLiteral();
        accept(Token.THEN);
        Command cAST = parseCommand();
        caseAST = new CaseLiteral(l1AST,cAST, commandPos);
        
        
    }
    
    return caseAST;
   
}
  /*
  * ParseCases: esta rutina se encarga de recorrer un cases reconociendo su sintaxis, y sino esta correcta dispara un error. 
  */
  Cases parseCases() throws SyntaxError {
    Cases cAST = null; // in case there's a syntactic error
    SourcePosition commandPos = new SourcePosition();
    start(commandPos);
    ArrayList<Case> ListCAST = new ArrayList<>();
    while(currentToken.kind == Token.WHEN){
    ListCAST.add(parseCase());
    }
    switch (currentToken.kind) {
    
    case Token.ELSE:{
        Command cl = parseElseCase();
        cAST = new CasesElse(ListCAST, cl, commandPos);
    }
    break;
    
    default: {
        cAST = new CasesCase(ListCAST, commandPos);;
    }
    break;
    }
    return cAST;
  }
  /*
  *ParseElseCase: esta rutina se encarga de recorrer un elseCases reconociendo su sintaxis, y sino esta correcta dispara un error. 
  */
  Command parseElseCase() throws SyntaxError {
    Command commandAST = null; // in case there's a syntactic error
    SourcePosition commandPos = new SourcePosition();
    start(commandPos);
    switch (currentToken.kind) {
    
    case Token.ELSE:{
        acceptIt();
        commandAST = parseCommand();
    }
    break;

    default:
      syntacticError("\"%\" cannot start a case",
        currentToken.spelling);
      break;

    }
    return commandAST;
   
}

Terminal parseCaseLiteral() throws SyntaxError {
    Terminal CL = null;

    if (currentToken.kind == Token.CHARLITERAL) {
      previousTokenPosition = currentToken.position;
      String spelling = currentToken.spelling;
      CL = new CharacterLiteral(spelling, previousTokenPosition);
      currentToken = lexicalAnalyser.scan();
    } else if (currentToken.kind == Token.INTLITERAL) {
      previousTokenPosition = currentToken.position;
      String spelling = currentToken.spelling;
      CL = new IntegerLiteral(spelling, previousTokenPosition);
      currentToken = lexicalAnalyser.scan();
    } else {
      CL = null;
      contextualError("integer literal expected here", "");
    }
    return CL;
  }  
///////////////////////////////////////////////////////////////////////////////
//
// COMMANDS
//
///////////////////////////////////////////////////////////////////////////////

// parseCommand parses the command, and constructs an AST
// to represent its phrase structure.

  Command parseCommand() throws SyntaxError {
    Command commandAST = null; // in case there's a syntactic error

    SourcePosition commandPos = new SourcePosition();
    
    start(commandPos);
    commandAST = parseSingleCommand();
    while (currentToken.kind == Token.SEMICOLON) {
      acceptIt();
      Command c2AST = parseSingleCommand();
      finish(commandPos);
      commandAST = new SequentialCommand(commandAST, c2AST, commandPos);
    }
    return commandAST;
  }

  
  /*
  *ParseCommand: Se le añadio el reconocimeinto de los casos para las palabras reservadas:
  *  skip, let, if, select, repeat, a partir de esas se hacer el parseo de las diferentes 
  *  opciones referentes a esta regla
  */
  
  
  Command parseSingleCommand() throws SyntaxError {
    Command commandAST = null; // in case there's a syntactic error
    SourcePosition commandPos = new SourcePosition();
    start(commandPos);
    switch (currentToken.kind) {
    
    case Token.IDENTIFIER:
      {
        Identifier iAST = parseIdentifier();
            String r=retorno.variable;
        if (currentToken.kind == Token.LPAREN) {
          acceptIt();
          if(currentToken.kind!= Token.RPAREN){
          ActualParameterSequence apsAST = parseActualParameterSequence();
          System.err.println("token2:"+currentToken.kind);
          if( "putint".equals(r)&& declaraciones.containsKey(retorno.variable)){
            Variable aux = declaraciones.get(retorno.variable);
            if(aux.visibleAProfundidad <= profundidad){
                if (aux.type != Token.INTLITERAL){
                    contextualError("\"%\" ","int expected here");
                }
            }else{ contextualError("\"%\" ","variable not visible");}
          }else if("putint".equals(r)&& (retorno.type != Token.INTLITERAL) ){
              contextualError("\"%\" ","int expected here");
          }else if("getint".equals(r)&& declaraciones.containsKey(retorno.variable)){
              if(declaraciones.get(retorno.variable).constant){
                  contextualError("\"%\"  is a constant" , retorno.variable);
              }
          }
          if(retorno.type == Token.IDENTIFIER){
              
          if( declaraciones.containsKey(retorno.variable)   ){
            Variable aux = declaraciones.get(retorno.variable);
            if(aux.visibleAProfundidad > profundidad){
                    contextualError("\"%\" ","variable not visible");
            }else if(aux.type == Token.PROC){
                contextualError("\"%\" it is a proc, it does not return any value  ",aux.variable);
            }
          }else{contextualError("\"%\" ",retorno.variable+" undeclared variablee");}
          }
          
          
          accept(Token.RPAREN);
          finish(commandPos);
          commandAST = new CallCommand(iAST, apsAST, commandPos);
          }else{
                ActualParameterSequence apsAST = parseActualParameterSequence();
                accept(Token.RPAREN);
                finish(commandPos);
                commandAST = new CallCommand(iAST, apsAST, commandPos);
          }

        } else {
          if (declaraciones.containsKey(r)){
              if(declaraciones.get(r).constant){
                  contextualError("\"%\"  is a constant" , retorno.variable);
              }
          }
          Vname vAST = parseRestOfVname(iAST);
          accept(Token.BECOMES);
          Expression eAST = parseExpression();
          finish(commandPos);
          commandAST = new AssignCommand(vAST, eAST, commandPos);
        }
      }
      break;
     /*
      * Recorre la opción del let validando su sintaxis a travez del token, 
      *asigna a commandAST UN new LetCommand
      */ 
    case Token.LET:
      {
        acceptIt();
        profundidad++;
        declarationPorfundidad++;
        Declaration dAST = parseDeclaration();
        accept(Token.IN);
        declarationPorfundidad--;
        Command cAST = parseCommand();
        accept(Token.END);
        profundidad--;
        updateVars();
        finish(commandPos);
        commandAST = new LetCommand(dAST, cAST, commandPos);
      }
      break;
      /*
      * Recorre la opción del if validando su sintaxis a travez del token, 
      *asigna a commandAST UN new IfCommand
      */
    case Token.IF:
      {
        ArrayList<Command> commands= new ArrayList<>();
        ArrayList<Expression> expressions= new ArrayList<>();
        acceptIt();
        Expression e = parseExpression();
        if(e instanceof BinaryExpression){
        expressions.add(e);
        accept(Token.THEN);
        commands.add(parseCommand());
        while(Token.OR == currentToken.kind){
            accept(Token.OR);
            Expression ex = parseExpression();
            if(e instanceof BinaryExpression){
            expressions.add(ex);
            accept(Token.THEN);
            commands.add(parseCommand());
            }else{contextualError("\"%\" ",
            "Boolean expression expected");}
        }
        accept(Token.ELSE);
        commands.add(parseCommand());
        accept(Token.END);
        finish(commandPos);
        commandAST = new IfCommand(expressions,commands, commandPos);
        }else{contextualError("\"%\" ","Boolean expression expected");}
      }
      break;
      /*
      * Recorre la opción del select validando su sintaxis a travez del token, 
      *asigna a commandAST UN new SelectCommand
      */
    case Token.SELECT:
    {
        acceptIt();
        Expression eAST = parseExpression();
        accept(Token.FROM);
        Cases cAST = parseCases();
        accept(Token.END);
        commandAST =  new SelectCommand(eAST, cAST, commandPos); 
       
        break;
    }
    
    /*
    *Recorre los diferentes tipos de loops validando con el Token para validar su sintaxis
    */
    case Token.REPEAT:
        {
        acceptIt();
        switch (currentToken.kind) {
            case Token.WHILE:
                {
                acceptIt();
                Expression expAST = parseExpression();
                accept(Token.DO);
                Command cAST = parseCommand();
                accept(Token.END);
                finish(commandPos);
                commandAST = new WhileCommand(expAST, cAST, commandPos);
                }
                break;
            
            case Token.UNTIL:
                {
                acceptIt();
                Expression EXP = parseExpression();
                accept(Token.DO);
                Command cAST = parseCommand();
                accept(Token.END);
                finish(commandPos);
                commandAST = new WhileCommand(EXP, cAST, commandPos);//TODO: newUntilCommand
                }
                break;
            case Token.DO:
                {
                 acceptIt();
                  Command cAST = parseCommand();
                 if (currentToken.kind == Token.WHILE){
                    acceptIt();
                    Expression EXP = parseExpression();
                    commandAST = new WhileCommand(EXP, cAST, commandPos);
                    accept(Token.END);
                 }else if(currentToken.kind == Token.UNTIL){
                     acceptIt();
                    Expression EXP = parseExpression();
                    commandAST = new WhileCommand(EXP, cAST, commandPos);
                    accept(Token.END);
                 }
                 
                }
                break;
            case Token.FOR:
                {
                    acceptIt();
                    Identifier iAST = parseIdentifier();
                    if (currentToken.kind == Token.BECOMES){
                        acceptIt();
                        accept(Token.RANGE);
                        Expression EXP1 = parseExpression();
                        accept(Token.DOTDOT);
                        Expression EXP2 = parseExpression();
                        if (currentToken.kind == Token.WHILE){
                            acceptIt();
                            Expression EXP3 = parseExpression();
                            accept(Token.DO);
                            Command com1 = parseCommand();
                            accept(Token.END);
                            commandAST = new ForWhileCommand(iAST,EXP1,EXP2,EXP3,com1, commandPos);

                        }else if(currentToken.kind == Token.DO){
                            acceptIt();
                            Command com2 = parseCommand();
                            accept(Token.END);
                            
                            commandAST = new ForDo(iAST,EXP1,EXP2, com2, commandPos);

                        }else if(currentToken.kind == Token.UNTIL){
                            acceptIt();
                            Expression EXP3 = parseExpression();
                            accept(Token.DO);
                            Command com1 = parseCommand();
                            accept(Token.END);
                            commandAST = new ForUntilCommand(iAST,EXP1,EXP2,EXP3,com1, commandPos);//TODO: ForUntilCommand
                        }
                    }else if(currentToken.kind == Token.IN){
                        acceptIt();
                        Expression EXP3 = parseExpression();
                        accept(Token.DO);
                        Command com1 = parseCommand();
                        accept(Token.END);
                        commandAST = new ForInCommand(iAST,EXP3,com1,commandPos);
                    }
                }
                break;
        }
        }
        break;
    case Token.END:
    case Token.ELSE:
    case Token.IN:
        /*
        Case skip, genera un comando vacio
        */
    case Token.SKIP:
        acceptIt();
        finish(commandPos);
        commandAST = new EmptyCommand(commandPos);
        break;

    default:
      syntacticError("\"%\" cannot start a command",
        currentToken.spelling);
      break;

    }

    return commandAST;
  }

///////////////////////////////////////////////////////////////////////////////
//
// EXPRESSIONS
//
///////////////////////////////////////////////////////////////////////////////

  Expression parseExpression() throws SyntaxError {
    Expression expressionAST = null; // in case there's a syntactic error

    SourcePosition expressionPos = new SourcePosition();

    start (expressionPos);

    switch (currentToken.kind) {

    case Token.LET:
      {
        acceptIt();
        Declaration dAST = parseDeclaration();
        accept(Token.IN);
        Expression eAST = parseExpression();
        finish(expressionPos);
        expressionAST = new LetExpression(dAST, eAST, expressionPos);
      }
      break;

    case Token.IF:
      {
        acceptIt();
        Expression e1AST = parseExpression();
        accept(Token.THEN);
        Expression e2AST = parseExpression();
        accept(Token.ELSE);
        Expression e3AST = parseExpression();
        finish(expressionPos);
        expressionAST = new IfExpression(e1AST, e2AST, e3AST, expressionPos);
      }
      break;

    default:
      expressionAST = parseSecondaryExpression();
      
      break;
    }
    return expressionAST;
  }

  Expression parseSecondaryExpression() throws SyntaxError {
    Expression expressionAST = null; // in case there's a syntactic error

    SourcePosition expressionPos = new SourcePosition();
    start(expressionPos);
    Variable aux= new Variable();
    expressionAST = parsePrimaryExpression();
    if(retorno.type == Token.IDENTIFIER && !declaraciones.containsKey(retorno.variable)){
      contextualError(retorno.variable+" Undeclared variable",
        "");
    }
    if (declaraciones.containsKey(retorno.variable)){
        aux= declaraciones.get(retorno.variable);
    }else{
        aux = retorno;
    }
    while (currentToken.kind == Token.OPERATOR) {
      Operator opAST = parseOperator();
      Expression e2AST = parsePrimaryExpression();
      if(retorno.type == Token.IDENTIFIER && !declaraciones.containsKey(retorno.variable)){
            contextualError(retorno.variable+" Undeclared variable",
            "");
      }
      if(retorno.type != aux.type){
          System.err.println("auxType:"+aux.variable+" "+aux.type + "Return type:"+retorno.variable+" "+retorno.type);
         if(declaraciones.containsKey(retorno.variable)){
             if(declaraciones.get(retorno.variable).type != aux.type){
                 contextualError("\"%\" ", "Non congruent Types");
             }
         }
         else{contextualError("\"%\" ", "Non congruent Types");}
      }
      
      expressionAST = new BinaryExpression (expressionAST, opAST, e2AST,
        expressionPos);
    }
    return expressionAST;
  }

  Expression parsePrimaryExpression() throws SyntaxError {
    Expression expressionAST = null; // in case there's a syntactic error

    SourcePosition expressionPos = new SourcePosition();
    start(expressionPos);

    switch (currentToken.kind) {

    case Token.INTLITERAL:
      {  
        IntegerLiteral ilAST = parseIntegerLiteral();
        finish(expressionPos);
          
        expressionAST = new IntegerExpression(ilAST, expressionPos);
             
      }
      break;

    case Token.CHARLITERAL:
      {
        CharacterLiteral clAST= parseCharacterLiteral();
        finish(expressionPos);
        expressionAST = new CharacterExpression(clAST, expressionPos);
      }
      break;

    case Token.LBRACKET:
      {
        acceptIt();
        ArrayAggregate aaAST = parseArrayAggregate();
        accept(Token.RBRACKET);
        finish(expressionPos);
        expressionAST = new ArrayExpression(aaAST, expressionPos);
      }
      break;

    case Token.LCURLY:
      {
        acceptIt();
        RecordAggregate raAST = parseRecordAggregate();
        accept(Token.RCURLY);
        finish(expressionPos);
        expressionAST = new RecordExpression(raAST, expressionPos);
      }
      break;

    case Token.IDENTIFIER:
      {
        Identifier iAST= parseIdentifier();
          String r = retorno.variable; 
        if (currentToken.kind == Token.LPAREN) {
          
          acceptIt();
          params = new ArrayList<>();
          ActualParameterSequence apsAST = parseActualParameterSequence();
          if(procYFuncs.containsKey(r)){
              if(r.equals(procYFuncs.get("f").variable))
              if(!procYFuncs.get(r).typeparam.toString().equals(params.toString())){contextualError("\"%\" wrong parameter",procYFuncs.get(r).variable);}
          }else{
              ProcFunc p = new ProcFunc();
              p.variable = r;
              p.typeparam = params;
              
              procYFuncs.put(p.variable, p);
             
          }
          accept(Token.RPAREN);
          finish(expressionPos);
          expressionAST = new CallExpression(iAST, apsAST, expressionPos);

        } else {
          Vname vAST = parseRestOfVname(iAST);
          finish(expressionPos);
          expressionAST = new VnameExpression(vAST, expressionPos);
        }
      }
      break;

    case Token.OPERATOR:
      {
        Operator opAST = parseOperator();
        Expression eAST = parsePrimaryExpression();
        finish(expressionPos);
        expressionAST = new UnaryExpression(opAST, eAST, expressionPos);
      }
      break;

    case Token.LPAREN:
      acceptIt();
      expressionAST = parseExpression();
      accept(Token.RPAREN);
      break;

    default:
      syntacticError("\"%\" cannot start an expression",
        currentToken.spelling);
      break;

    }
    return expressionAST;
  }

  RecordAggregate parseRecordAggregate() throws SyntaxError {
    RecordAggregate aggregateAST = null; // in case there's a syntactic error

    SourcePosition aggregatePos = new SourcePosition();
    start(aggregatePos);

    Identifier iAST = parseIdentifier();
    accept(Token.IS);
    Expression eAST = parseExpression();

    if (currentToken.kind == Token.COMMA) {
      acceptIt();
      RecordAggregate aAST = parseRecordAggregate();
      finish(aggregatePos);
      aggregateAST = new MultipleRecordAggregate(iAST, eAST, aAST, aggregatePos);
    } else {
      finish(aggregatePos);
      aggregateAST = new SingleRecordAggregate(iAST, eAST, aggregatePos);
    }
    return aggregateAST;
  }

  ArrayAggregate parseArrayAggregate() throws SyntaxError {
    ArrayAggregate aggregateAST = null; // in case there's a syntactic error

    SourcePosition aggregatePos = new SourcePosition();
    start(aggregatePos);

    Expression eAST = parseExpression();
    if (currentToken.kind == Token.COMMA) {
      acceptIt();
      ArrayAggregate aAST = parseArrayAggregate();
      finish(aggregatePos);
      aggregateAST = new MultipleArrayAggregate(eAST, aAST, aggregatePos);
    } else {
      finish(aggregatePos);
      aggregateAST = new SingleArrayAggregate(eAST, aggregatePos);
    }
    return aggregateAST;
  }

///////////////////////////////////////////////////////////////////////////////
//
// VALUE-OR-VARIABLE NAMES
//
///////////////////////////////////////////////////////////////////////////////

  Vname parseVname () throws SyntaxError {
    Vname vnameAST = null; // in case there's a syntactic error
    Identifier iAST = parseIdentifier();
    vnameAST = parseRestOfVname(iAST);
    return vnameAST;
  }

  Vname parseRestOfVname(Identifier identifierAST) throws SyntaxError {
    SourcePosition vnamePos = new SourcePosition();
    vnamePos = identifierAST.position;
    Vname vAST = new SimpleVname(identifierAST, vnamePos);

    while (currentToken.kind == Token.DOT ||
           currentToken.kind == Token.LBRACKET) {

      if (currentToken.kind == Token.DOT) {
        acceptIt();
        Identifier iAST = parseIdentifier();
        vAST = new DotVname(vAST, iAST, vnamePos);
      } else {
        acceptIt();
        Expression eAST = parseExpression();
        accept(Token.RBRACKET);
        finish(vnamePos);
        vAST = new SubscriptVname(vAST, eAST, vnamePos);
      }
    }
    return vAST;
  }

///////////////////////////////////////////////////////////////////////////////
//
// DECLARATIONS
//
///////////////////////////////////////////////////////////////////////////////

  Declaration parseDeclaration() throws SyntaxError {
    Declaration declarationAST = null; // in case there's a syntactic error

    SourcePosition declarationPos = new SourcePosition();
    start(declarationPos);
    Declaration dAST = parseCompountDeclaration();
    if (currentToken.kind == Token.SEMICOLON) {
      
      ArrayList<Declaration>  list = new ArrayList<>();
      while(currentToken.kind == Token.SEMICOLON){
      acceptIt();
      Declaration d2AST = parseCompountDeclaration();
      
      list.add(d2AST);
      }
      finish(declarationPos);
      declarationAST = new SequentialDeclarationNnary(dAST, list,declarationPos);
      
    }else{
        declarationAST= dAST;
    }
    return declarationAST;
  }

  Declaration parseSingleDeclaration() throws SyntaxError {
    Declaration declarationAST = null; // in case there's a syntactic error

    SourcePosition declarationPos = new SourcePosition();
    start(declarationPos);

    switch (currentToken.kind) {

    case Token.CONST:
      {
        acceptIt();
        Variable aux = new Variable();
        Identifier iAST = parseIdentifier();
        aux.variable = retorno.variable;
        aux.visibleAProfundidad = declarationPorfundidad;
        accept(Token.IS);
        Expression eAST = parseExpression();
        aux.type = retorno.type;
        if(declaraciones.containsKey(retorno.variable)){
            aux.type = declaraciones.get(retorno.variable).type;
        }
        
        aux.constant = true;
        declaraciones.put(aux.variable,aux);
        finish(declarationPos);
        declarationAST = new ConstDeclaration(iAST, eAST, declarationPos);
      }
      break;
      /*
      Valida la sintaxis de var, por medio del token
      asigna a declarationAST un new VarDeclaration
      */
    case Token.VAR:
      {
        acceptIt();
        Variable aux = new Variable();
        Identifier iAST = parseIdentifier();
        aux.variable = retorno.variable;
          
        aux.visibleAProfundidad = declarationPorfundidad;
        if (Token.COLON ==currentToken.kind ){
        accept(Token.COLON);
        TypeDenoter tAST = parseTypeDenoter();
        if("Integer".equals(retorno.variable) ){
            aux.type = Token.INTLITERAL;
            
            declaraciones.put(aux.variable,aux);
        }
        if("Boolean".equals(retorno.variable)){
            aux.type = -1;
            declaraciones.put(aux.variable,aux);
        }
        if("Char".equals(retorno.variable)){
            aux.type = Token.CHARLITERAL;
            declaraciones.put(aux.variable,aux);
        }
        finish(declarationPos);
        declarationAST = new VarDeclaration(iAST, tAST, declarationPos);
        }else{
        accept(Token.BECOMES);
        Expression eAST = parseExpression();
        if(declaraciones.containsKey(aux.variable)){
            //syntacticError("\"%\" is already in use",aux.variable);
        }
        aux.type = retorno.type;
        declaraciones.put(aux.variable,aux);
        finish(declarationPos);
        declarationAST = new VarDeclarationEXP(iAST, eAST, declarationPos);
        }
      }
      break;

      /*
      Se modifico para que parsee la sintaxis nueva expecificada en la documentacion por medio del token
      asigna a declarationAST un new ProcDeclaration
      */
    case Token.PROC:
      {
        acceptIt();
        Variable aux = new Variable();
        Identifier iAST = parseIdentifier();
        aux.variable = retorno.variable;
        aux.visibleAProfundidad = declarationPorfundidad;
        accept(Token.LPAREN);
        profundidad++;
        declarationPorfundidad++;
        FormalParameterSequence fpsAST = parseFormalParameterSequence();
        accept(Token.RPAREN);
        accept(Token.IS);
        Command cAST = parseCommand();
        declarationPorfundidad--;
        profundidad--;
        updateVars();
        accept(Token.END);
        aux.type = Token.PROC;
        declaraciones.put(aux.variable, aux);
        finish(declarationPos);
        declarationAST = new ProcDeclaration(iAST, fpsAST, cAST, declarationPos);
      }
      break;

    case Token.FUNC:
      {
        acceptIt();
        Variable aux = new Variable();
        Identifier iAST = parseIdentifier();
        aux.variable = retorno.variable;
        aux.visibleAProfundidad = declarationPorfundidad;
        accept(Token.LPAREN);
        FormalParameterSequence fpsAST = parseFormalParameterSequence();
        accept(Token.RPAREN);
        accept(Token.COLON);
        TypeDenoter tAST = parseTypeDenoter();
        if("Integer".equals(retorno.variable)){
            aux.type = Token.INTLITERAL;
        }
        if("Boolean".equals(retorno.variable)){
            aux.type = -1;
        }
        if("Char".equals(retorno.variable)){
            aux.type = Token.CHARLITERAL;
        }
        declaraciones.put(aux.variable, aux);
        accept(Token.IS);
        Expression eAST = parseExpression();
        finish(declarationPos);
        declarationAST = new FuncDeclaration(iAST, fpsAST, tAST, eAST,
          declarationPos);
      }
      break;

    case Token.TYPE:
      {
        acceptIt();
        Identifier iAST = parseIdentifier();
        accept(Token.IS);
        TypeDenoter tAST = parseTypeDenoter();
        finish(declarationPos);
        declarationAST = new TypeDeclaration(iAST, tAST, declarationPos);
      }
      break;

    default:
      syntacticError("\"%\" cannot start a declaration",
        currentToken.spelling);
      break;

    }
    return declarationAST;
  }
  /*
  *Recorre una declaracion compuesta validando su sintaxis por medio del token
  * decuelve una declaration
  */
  Declaration parseCompountDeclaration() throws SyntaxError{
    Declaration declarationAST = null; // in case there's a syntactic error

    SourcePosition declarationPos = new SourcePosition();
    start(declarationPos);
    switch (currentToken.kind) {

       case Token.RECURSIVE:
      {
        acceptIt();
        Profuncs pAST = parseProfuncs();
        accept(Token.END);
        finish(declarationPos);
        declarationAST = new compoundDeclRecursive(pAST, declarationPos);
      }
      break;
      case Token.LOCAL:
      {
        acceptIt();
        profundidad++;
        declarationPorfundidad++;
        Declaration dAST = parseDeclaration();
        accept(Token.IN);
        declarationPorfundidad--;
        Declaration d2AST = parseDeclaration();
        accept(Token.END);
        profundidad--;
        updateVars();
        finish(declarationPos);
        declarationAST = new compoundDeclLocal(dAST,d2AST, declarationPos);
      }
      break;
     default:
      {
        declarationAST = parseSingleDeclaration();
        finish(declarationPos);
        
        
      }
      break;
    }
    return declarationAST;
}
///////////////////////////////////////////////////////////////////////////////
//
//Pro-func
// 
///////////////////////////////////////////////////////////////////////////////
  /*
  Parsea las opciones de PROC Y FUNC validando su sintaxis según lo especificado
  Retorna una declaration
  */
  
  Declaration parseProfunc()throws SyntaxError{
    Declaration declarationAST = null; // in case there's a syntactic error

    SourcePosition declarationPos = new SourcePosition();
    start(declarationPos);
    switch (currentToken.kind) {

       case Token.PROC:
      {
        acceptIt();
        profundidad++;
        declarationPorfundidad++;
        Identifier iAST = parseIdentifier();
        retornoProFunc.variable= retorno.variable;
        retornoProFunc.type = Token.PROC;
        accept(Token.LPAREN);
        FormalParameterSequence fpsAST = parseFormalParameterSequence();
        retornoProFunc.typeparam = params;
        params = new ArrayList<>();
        retornoProFunc.visibleAProfundidad=declarationPorfundidad;
        procYFuncs.put(retornoProFunc.variable, retornoProFunc);
       
        accept(Token.RPAREN);
        accept(Token.IS);
        Command cAST = parseCommand();
        profundidad--;
        declarationPorfundidad--;
        updateVars();
        accept(Token.END);
        finish(declarationPos);
        declarationAST = new ProcDeclaration(iAST, fpsAST, cAST, declarationPos);
      }
      break;

    default:
      {
        accept(Token.FUNC);
        
        Identifier iAST = parseIdentifier();
        retornoProFunc.variable= retorno.variable;
        accept(Token.LPAREN);
        params = new ArrayList<>();
        FormalParameterSequence fpsAST = parseFormalParameterSequence();
       
        retornoProFunc.typeparam = params;
        params = new ArrayList<>();
        if(procYFuncs.containsKey(retornoProFunc.variable)){
            System.err.println("params to "+retornoProFunc.variable+" :"+retornoProFunc.typeparam);
            System.err.println("retparams:"+procYFuncs.get(retornoProFunc.variable).typeparam);
            if(procYFuncs.get(retornoProFunc.variable).define){
                contextualError("\"%\": id already in use",retornoProFunc.variable);
            }
            if(!procYFuncs.get(retornoProFunc.variable).typeparam.toString().equals(retornoProFunc.typeparam.toString())){contextualError("\"%\" wrong parameter",retornoProFunc.variable);
            }
        }
        
        accept(Token.RPAREN);
        accept(Token.COLON);
        
        TypeDenoter tAST = parseTypeDenoter();
        if("Integer".equals(retorno.variable) ){
            retornoProFunc.type =  Token.INTLITERAL;
        }
        if("Boolean".equals(retorno.variable) ){
            retornoProFunc.type =  -1;
            
        }
        if("Char".equals(retorno.variable)){
            retornoProFunc.type = Token.CHARLITERAL;
        }
        retornoProFunc.visibleAProfundidad=declarationPorfundidad;
        retornoProFunc.define = true;
        procYFuncs.put(retornoProFunc.variable, retornoProFunc);
        
        accept(Token.IS);
        
        Expression eAST = parseExpression();
        finish(declarationPos);
        declarationAST = new FuncDeclaration(iAST, fpsAST, tAST, eAST,
          declarationPos);
      }
      break;
    }
    return declarationAST;
  }
  /*
  Parsea un proFuncs validando su sintaxis por medio del token
  Devuelve un objeto Profuncs
  */
  Profuncs parseProfuncs() throws SyntaxError{
    
    SourcePosition parseProfuncsPos = new SourcePosition();
    start(parseProfuncsPos);
    
      ArrayList<Declaration> listPAST = new ArrayList<>();
      // 'Anadir a lista de procyFuncs'
      System.err.println("ver linea 1240");
      listPAST.add(parseProfunc());
      
    do{
          accept(Token.OR);
          listPAST.add(parseProfunc());
      }while (Token.OR == currentToken.kind);
      
      return new Pro_funcs(listPAST,parseProfuncsPos);
  }
///////////////////////////////////////////////////////////////////////////////
//
// PARAMETERS
//
///////////////////////////////////////////////////////////////////////////////

  FormalParameterSequence parseFormalParameterSequence() throws SyntaxError {
    FormalParameterSequence formalsAST;

    SourcePosition formalsPos = new SourcePosition();

    start(formalsPos);
    if (currentToken.kind == Token.RPAREN) {
      finish(formalsPos);
      formalsAST = new EmptyFormalParameterSequence(formalsPos);

    } else {
      formalsAST = parseProperFormalParameterSequence();
    }
    return formalsAST;
  }

  FormalParameterSequence parseProperFormalParameterSequence() throws SyntaxError {
    FormalParameterSequence formalsAST = null; // in case there's a syntactic error;

    SourcePosition formalsPos = new SourcePosition();
    start(formalsPos);
    FormalParameter fpAST = parseFormalParameter();
    if (currentToken.kind == Token.COMMA) {
      acceptIt();
      FormalParameterSequence fpsAST = parseProperFormalParameterSequence();
      finish(formalsPos);
      formalsAST = new MultipleFormalParameterSequence(fpAST, fpsAST,
        formalsPos);

    } else {
      finish(formalsPos);
      formalsAST = new SingleFormalParameterSequence(fpAST, formalsPos);
    }
    return formalsAST;
  }

  FormalParameter parseFormalParameter() throws SyntaxError {
    FormalParameter formalAST = null; // in case there's a syntactic error;

    SourcePosition formalPos = new SourcePosition();
    start(formalPos);

    switch (currentToken.kind) {

    case Token.IDENTIFIER:
      {                   
         Variable aux = new Variable();
        Identifier iAST = parseIdentifier();
        aux.variable = retorno.variable;  
        aux.visibleAProfundidad = declarationPorfundidad;
        
        accept(Token.COLON);
        TypeDenoter tAST = parseTypeDenoter();
        if(declaraciones.containsKey(aux.variable)){
            //syntacticError("\"%\" is already in use",aux.variable);
        }
        if("Integer".equals(retorno.variable) ){
            aux.type = Token.INTLITERAL;
            params.add(Token.INTLITERAL);
            declaraciones.put(aux.variable,aux);
        }
        if("Boolean".equals(retorno.variable) ){
            aux.type = -1;
            params.add(-1);
            declaraciones.put(aux.variable,aux);
        }
        if("Char".equals(retorno.variable)){
            aux.type =  Token.CHARLITERAL;
            params.add(Token.CHARLITERAL);
            declaraciones.put(aux.variable,aux);
        }
        
        finish(formalPos);
        formalAST = new ConstFormalParameter(iAST, tAST, formalPos);
      }
      break;

    case Token.VAR:
      {
        acceptIt();
        Identifier iAST = parseIdentifier();
        accept(Token.COLON);
        TypeDenoter tAST = parseTypeDenoter();
        if("Integer".equals(retorno.variable) ){
            params.add(Token.INTLITERAL);
        }
        if("Boolean".equals(retorno.variable) ){
            params.add(-1);
        }
        if("Char".equals(retorno.variable)){
            params.add(Token.CHARLITERAL);
        }
        finish(formalPos);
        formalAST = new VarFormalParameter(iAST, tAST, formalPos);
      }
      break;

    case Token.PROC:
      {
        params.add(Token.PROC);//No se si funcione
        acceptIt();
        Identifier iAST = parseIdentifier();
        accept(Token.LPAREN);
        FormalParameterSequence fpsAST = parseFormalParameterSequence();
        accept(Token.RPAREN);
        finish(formalPos);
        formalAST = new ProcFormalParameter(iAST, fpsAST, formalPos);
      }
      break;

    case Token.FUNC:
      {
        params.add(Token.FUNC);
        acceptIt();
        Identifier iAST = parseIdentifier();
        accept(Token.LPAREN);
        FormalParameterSequence fpsAST = parseFormalParameterSequence();
        accept(Token.RPAREN);
        accept(Token.COLON);
        TypeDenoter tAST = parseTypeDenoter();
        finish(formalPos);
        formalAST = new FuncFormalParameter(iAST, fpsAST, tAST, formalPos);
      }
      break;

    default:
      syntacticError("\"%\" cannot start a formal parameter",
        currentToken.spelling);
      break;

    }
    return formalAST;
  }


  ActualParameterSequence parseActualParameterSequence() throws SyntaxError {
    ActualParameterSequence actualsAST;

    SourcePosition actualsPos = new SourcePosition();

    start(actualsPos);
    if (currentToken.kind == Token.RPAREN) {
      finish(actualsPos);
      actualsAST = new EmptyActualParameterSequence(actualsPos);

    } else {
      actualsAST = parseProperActualParameterSequence();
    }
    return actualsAST;
  }

  ActualParameterSequence parseProperActualParameterSequence() throws SyntaxError {
    ActualParameterSequence actualsAST = null; // in case there's a syntactic error

    SourcePosition actualsPos = new SourcePosition();

    start(actualsPos);
    ActualParameter apAST = parseActualParameter();
    if (currentToken.kind == Token.COMMA) {
      acceptIt();
      ActualParameterSequence apsAST = parseProperActualParameterSequence();
      finish(actualsPos);
      actualsAST = new MultipleActualParameterSequence(apAST, apsAST,
        actualsPos);
    } else {
      finish(actualsPos);
      actualsAST = new SingleActualParameterSequence(apAST, actualsPos);
    }
    return actualsAST;
  }

  ActualParameter parseActualParameter() throws SyntaxError {
    ActualParameter actualAST = null; // in case there's a syntactic error

    SourcePosition actualPos = new SourcePosition();

    start(actualPos);

    switch (currentToken.kind) {

    case Token.IDENTIFIER:
    case Token.INTLITERAL:
    case Token.CHARLITERAL:
    case Token.OPERATOR:
    case Token.LET:
    case Token.IF:
    case Token.LPAREN:
    case Token.LBRACKET:
    case Token.LCURLY:
      {
        Expression eAST = parseExpression();
        params.add(retorno.type);
        finish(actualPos);
        actualAST = new ConstActualParameter(eAST, actualPos);
      }
      break;

    case Token.VAR:
      {
        acceptIt();
        Vname vAST = parseVname();
        finish(actualPos);
        actualAST = new VarActualParameter(vAST, actualPos);
      }
      break;

    case Token.PROC:
      {
        acceptIt();
        Identifier iAST = parseIdentifier();
        params.add(retorno.type);
        finish(actualPos);
        actualAST = new ProcActualParameter(iAST, actualPos);
      }
      break;

    case Token.FUNC:
      {
        acceptIt();
        Identifier iAST = parseIdentifier();
        if(procYFuncs.containsKey(retorno.variable)){
            params.add(procYFuncs.get(retorno.variable).type);
        }else{
            syntacticError("\"%\" Procedmiento o funcion no disponible",
        currentToken.spelling);
        }
        
        finish(actualPos);
        actualAST = new FuncActualParameter(iAST, actualPos);
      }
      break;

    default:
      syntacticError("\"%\" cannot start an actual parameter",
        currentToken.spelling);
      break;

    }
    return actualAST;
  }

///////////////////////////////////////////////////////////////////////////////
//
// TYPE-DENOTERS
//
///////////////////////////////////////////////////////////////////////////////

  TypeDenoter parseTypeDenoter() throws SyntaxError {
    TypeDenoter typeAST = null; // in case there's a syntactic error
    SourcePosition typePos = new SourcePosition();

    start(typePos);

    switch (currentToken.kind) {

    case Token.IDENTIFIER:
      {
        Identifier iAST = parseIdentifier();
        finish(typePos);
        typeAST = new SimpleTypeDenoter(iAST, typePos);
      }
      break;

    case Token.ARRAY:
      {
        acceptIt();
        IntegerLiteral ilAST = parseIntegerLiteral();
        accept(Token.OF);
        TypeDenoter tAST = parseTypeDenoter();
        finish(typePos);
        typeAST = new ArrayTypeDenoter(ilAST, tAST, typePos);
      }
      break;

    case Token.RECORD:
      {
        acceptIt();
        FieldTypeDenoter fAST = parseFieldTypeDenoter();
        accept(Token.END);
        finish(typePos);
        typeAST = new RecordTypeDenoter(fAST, typePos);
      }
      break;

    default:
      syntacticError("\"%\" cannot start a type denoter",
        currentToken.spelling);
      break;

    }
    return typeAST;
  }

  FieldTypeDenoter parseFieldTypeDenoter() throws SyntaxError {
    FieldTypeDenoter fieldAST = null; // in case there's a syntactic error

    SourcePosition fieldPos = new SourcePosition();

    start(fieldPos);
    Identifier iAST = parseIdentifier();
    accept(Token.COLON);
    TypeDenoter tAST = parseTypeDenoter();
    if (currentToken.kind == Token.COMMA) {
      acceptIt();
      FieldTypeDenoter fAST = parseFieldTypeDenoter();
      finish(fieldPos);
      fieldAST = new MultipleFieldTypeDenoter(iAST, tAST, fAST, fieldPos);
    } else {
      finish(fieldPos);
      fieldAST = new SingleFieldTypeDenoter(iAST, tAST, fieldPos);
    }
    return fieldAST;
  }
  public void updateVars(){
      
     
      for(Iterator<Variable> iterator = declaraciones.values().iterator(); iterator.hasNext(); ) {
        Variable var = iterator.next();
        if(var.visibleAProfundidad > profundidad){
             System.err.println("var:"+var.variable);
              iterator.remove();
        }
      }
  }
}

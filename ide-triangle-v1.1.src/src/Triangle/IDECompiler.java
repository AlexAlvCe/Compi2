/*
 * IDE-Triangle v1.0
 * Compiler.java 
 */

package Triangle;

import Triangle.CodeGenerator.Frame;
import java.awt.event.ActionListener;
import Triangle.SyntacticAnalyzer.SourceFile;
import Triangle.SyntacticAnalyzer.Scanner;
import Triangle.AbstractSyntaxTrees.Program;
import Triangle.SyntacticAnalyzer.Parser;
import Triangle.ContextualAnalyzer.Checker;
import Triangle.CodeGenerator.Encoder;
import Triangle.TreeWriterHTML.Writer;
import java.io.*;



/** 
 * This is merely a reimplementation of the Triangle.Compiler class. We need
 * to get to the ASTs in order to draw them in the IDE without modifying the
 * original Triangle code.
 *
 * @author Luis Leopoldo Pérez <luiperpe@ns.isi.ulatina.ac.cr>
 */
public class IDECompiler {

    // <editor-fold defaultstate="collapsed" desc=" Methods ">
    /**
     * Creates a new instance of IDECompiler.
     *
     */
    public IDECompiler() {
    }
    
    /**
     * Particularly the same compileProgram method from the Triangle.Compiler
     * class.
     * @param sourceName Path to the source file.
     * @return True if compilation was succesful.
     */
    public boolean compileProgram(String sourceName) {
        System.out.println("********** " +
                           "Triangle Compiler (IDE-Triangle 1.0)" +
                           " **********");
        
        System.out.println("Syntactic Analysis ...");
        SourceFile source = new SourceFile(sourceName);
        Scanner scanner = new Scanner(source);
        report = new IDEReporter();
        Parser parser = new Parser(scanner, report);
        boolean success = false;
        
        rootAST = parser.parseProgram();
        if (report.numErrors == 0) {
            String file =sourceName.substring(0,sourceName.length()-3)+"xml";
            Writer w = new Writer(file);
            w.write(rootAST);
            System.out.println("xml created in : "+ file);
           //System.out.println("Contextual Analysis ...");
            //Checker checker = new Checker(report);
            //checker.check(rootAST);
            if (report.numErrors == 0) {
               // System.out.println("Code Generation ...");
                //Encoder encoder = new Encoder(report);
                //encoder.encodeRun(rootAST, false);
                
                if (report.numErrors == 0) {
                  //  encoder.saveObjectProgram(sourceName.replace(".tri", ".tam"));
                    success = true;
                }
            }
        }

        if (success)
            System.out.println("Compilation was successful.");
        else
            System.out.println("Compilation was unsuccessful.");
        
        // Html
        String file = sourceName.substring(0,sourceName.length()-3)+"html";
        HtmlGenerator htmlGenerator = new HtmlGenerator(sourceName); 
        String html = htmlGenerator.buildFile();

        File file2 = new File(file);
        try{
            BufferedWriter bw = new BufferedWriter(new FileWriter(file2));
            bw.write(html);//Aqui tira el error, un nullPointerException
            bw.close();
        } catch (IOException ioe) {
            
          //ioe.printStackTrace();
            System.out.println("Error al crear el HTML");
        }catch(NullPointerException ne){
           // ne.printStackTrace();
           System.out.println("Error al crear el HTML, IDECompiler.java");
        }
        
        return(success);
    }
      
    /**
     * Returns the line number where the first error is.
     * @return Line number.
     */
    public int getErrorPosition() {
        return(report.getFirstErrorPosition());
    }
        
    /**
     * Returns the root Abstract Syntax Tree.
     * @return Program AST (root).
     */
    public Program getAST() {
        return(rootAST);
    }
    // </editor-fold>
    
    // <editor-fold defaultstate="collapsed" desc=" Attributes ">
    private Program rootAST;        // The Root Abstract Syntax Tree.    
    private IDEReporter report;     // Our ErrorReporter class.
    // </editor-fold>
}

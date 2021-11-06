
package Triangle;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 */
public class HtmlGenerator {
    
    private String fileName = ""; // Atributo con el path de archivo
    private String htmlFile = ""; // Atributo con el html de salida
    private static List<String> tokenTable; // Atributo con la lista de tokens 
    
    // Constructor
    public HtmlGenerator(String filename){
        this.fileName = filename; 
        this.tokenTable = Arrays.asList("array", "const", "do", "else", "end", "for", "from", "func", 
                                       "if", "in", "let","local", "of", "proc", "range", "record", 
                                       "recursive", "repeat", "select", "skip", "then", "to", "type", 
                                       "until", "var", "when", "while");
    }
    
    public String buildFile(){
        try{
            BufferedReader bf = new BufferedReader(new FileReader(fileName)); // Objeto para lectura
            ArrayList<String[]> array = new ArrayList<>(); // Donde se guardan las lineas separadas
            String bfRead; // Donde se guarda cada linea leida
            String[] line; // Donde se guarda la linea separada por palabras
             
            // Mientras haya contenido en el archivo
            while((bfRead = bf.readLine()) != null){
                line = bfRead.split(" ");
                array.add(line); // Se agregan las lineas separadas por palabras al array
            }
            
            // Inicia el string del archivo html
            htmlFile += "<p style=\"font-family: 'DejaVu Sans', monospace;\">\n";
            // Se van obteniendo y analizando las lineas previamente guardadas
            // Se guardan en otro array
            ArrayList<String[]> array2 = new ArrayList<>();
            for (int i = 0; i < array.size(); i++) {
                String[] l = array.get(i);
                l = analizeLine(l); // Se manda a analizar la linea
                array2.add(l);
            }
            
            // Se limpia el array original
            array.clear();
            // Se recorre el array con las nuevas lineas, y se toman las palabras nuevas en cada una
            for (int j = 0; j < array2.size(); j++){
                String[] temp = array2.get(j);
                for (int k = 0; k < temp.length; k++){
                    htmlFile += " " + temp[k];
                }
                // Salto de linea en el html
                htmlFile += "<br> \n";
            }
            // Se cierra el parrafo y se retorna el string de archivo
            htmlFile += "</p>";
            return htmlFile;
            
        }catch(Exception e){
            System.out.println("Error");
            return null;
        }
    }
    
    public static String[] analizeLine(String[] line){
        // Evaluacion de linea y de comentarios
        String comment = "";
        int flag = -1;
        for(int i = 0; i < line.length; i++){ // Se recorre la linea de forma: ["a", "in", "!", "OK"]
            if(line[i].contains("!")){ // Si el elemento leido contiene un "!"
                comment += line[i] + " "; // Se crea un nuevo comentario a partir de ese elemento
                flag = i; // Se guarda la posición en la que se encontró el "!"
                break; // Se para la lectura
            }
            line[i] = analizeWord(line[i]); // Si no se encuentra ningun "!", se manda a analizar normal
        }
        
        if(flag != -1){ // Si flag es distinto de -1, hubo comentario
            for(int j = flag+1; j < line.length; j++){ // Se recorre la linea a partir de la posición de "!"
                comment += line[j] + " "; // Se agregan los elementos al comentario
                line[j] = ""; // Se elimina el elemento del arreglo de linea
            }
            // Se sustituye el elemento en el que estaba "!" por el comentario total
            line[flag] = "<font color='#00b300'>" + comment + "</font>";
        }
        return line;
    }
    
    public static String analizeWord(String word){
        // Evaluacion contra palabra reservada
        if(tokenTable.contains(word)){
            word = "<b>" + word + "</b>";
            
        // Evaluacion contra caracter
        } else if(word.matches(".*('\\w').*")){ // *El match genera dos veces el html por el '\n'
            Matcher m = Pattern.compile("('\\w')").matcher(word); // Se evalua el match contra: 'c'
            String[] p; 
            if(m.find()){ // Si el match se dio
                p = word.split("('\\w')"); // Se dividira la palabra utilizando el patron
                word = p[0] + "<font color='#0000cd'>" + m.group(1) + "</font>" + p[1];
            }
            
        // Evaluacion contra numeros
        } else if(word.matches(".*(\\d+).*")){
            Matcher m = Pattern.compile("(\\d+)").matcher(word); // Se evalua el match contra: n
            String[] p = word.split("(\\d+)"); // Se dividira la palabra utilizando el patron
            String finalWord = "";
            int i = 0;
            if((p.length > 0) && (p.length > m.groupCount())){ // Si se logra dividir la palabra
                while(m.find()){ // Mientras haya match, se intercalan los elementos
                finalWord += analizeWord(p[i]);
                finalWord += "<font color='#0000cd'>" + m.group() + "</font>";
                i++;  
                }
                // Se agrega el ultimo elemento de la palabra dividida
                if(i < p.length){
                    finalWord += analizeWord(p[i]);
                }
                word = finalWord;
                
            } else if ((p.length > 0) && (p.length < m.groupCount())){ // Si se logra dividir la palabra
                while(m.find()){ // Mientras haya match, se intercalan los elementos
                finalWord += "<font color='#0000cd'>" + m.group() + "</font>";
                finalWord += analizeWord(p[i]);
                i++;
                }
            } else { // Solo habia una palabra
                word = "<font color='#0000cd'>" + word + "</font>";
            }

        // Evaluacion contra tabulaciones
        }else if(word.contains("\t")){
            String[] finalWord; // Donde se guardara la palabra final como arreglo
            String preWord = word.replace("\t", ""); // Si hay tabulaciones se eliminan y se guarda previa
            preWord = analizeWord(preWord); // Se envia a evaluar la nueva previa guardada 
            
            word = word.replace("\t", "&nbsp;&nbsp;&nbsp;&nbsp; "); // Se sustituyen los tabs por espacios
            finalWord = word.split(" "); // El arreglo final se conforma por los espacios, mas la original
            word = ""; // Se pone vacia la palabra original
            // La nueva palabra se conforma por los espacios del arreglo mas la palabra previa guardada
            for(int i = 0; i < (finalWord.length)-1; i++){
                word += finalWord[i];
            }
            word += preWord;
        }
        
        // Se retorna la palabra segun el proceso aplicado
        return word;
    }
  
}

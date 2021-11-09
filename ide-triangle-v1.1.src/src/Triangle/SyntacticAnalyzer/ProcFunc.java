/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Triangle.SyntacticAnalyzer;

import java.util.ArrayList;

/**
 *
 * @author sande
 */
public class ProcFunc {
    public String variable = "";
    public int type = 0;
    public int visibleAProfundidad =0;
    public ArrayList<Integer> typeparam= new ArrayList<>();
    public boolean funcion = false;
}
